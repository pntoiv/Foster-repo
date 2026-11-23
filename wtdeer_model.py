# Instructions: https://swforrest.github.io/deepSSF/Python/deepssf_train_validate_example.html

# Script settings ####

# Simulate trajectories: True or False
RUN_SIMULATIONS = True 

# Run prediction maps: True or False
RUN_PREDMAPS = True

# Run yday-by-forest-age suitability summaries: True or False
RUN_YDAY_SUMMARY = True

# Number of epochs
EPOCHS_MOV = 20
EPOCHS_HAB = 50
EPOCHS_BOTH = 30

print(f"Total epochs: {EPOCHS_MOV+EPOCHS_HAB+EPOCHS_BOTH}")

# Setup ####
import math
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import rasterio
import rasterio.plot  # submodule is not imported by `import rasterio` alone
import torch
import time
import networkx as nx

from torch.utils.data import DataLoader

from pathlib import Path
from IPython.display import Image  # For displaying the training GIF
from rasterio.transform import Affine
from matplotlib.colors import LogNorm

# Use this to load the functions used in this notebook, and call them with `deepssf.function_name()`
import deepssf

print(f"deepssf {deepssf.__version__}")
print(f"torch {torch.__version__}")

# Reproducibility — fix random seeds so results are the same on every run.
torch.manual_seed(42)
np.random.seed(42)

# DEVICE is capitalised — module-level constant (set once, never reassigned).
# get_device() returns 'cuda' if a GPU is available, 'mps' on Apple Silicon,
# or 'cpu' otherwise.  All tensors and the model are moved to this device.
DEVICE = deepssf.get_device()
print(f"Device: {DEVICE}")

# Set paths and directories
DATA_DIR = Path("/scratch/project_2021178/data")  # location of the bundled sample data
CSV_PATH = DATA_DIR / "wtdeer_data.csv"  # Steps data (prepared in R)
LAYER_PATHS = {
    "canopy":  str(DATA_DIR / "canopy.tif"),
    "forest_age": str(DATA_DIR / "forest_age.tif"),
    "broadleaved_biomass": str(DATA_DIR / "broadleaved_biomass.tif"),
    "agriculture": str(DATA_DIR / "agriculture.tif"),
    "urban": str(DATA_DIR / "urban.tif"),
    "wetlands": str(DATA_DIR / "wetlands.tif"),
    "water": str(DATA_DIR / "water.tif"),
}
OUTPUT_DIR   = Path("/scratch/project_2021178/outputs/wtdeer") # figures and model checkpoints go here
SNAPSHOT_DIR = OUTPUT_DIR / "training_snapshots"  # per-batch prediction-map snapshots
OUTPUT_DIR.mkdir(exist_ok=True)


# Import data ####

## Import GPS data ####

raw_df = pd.read_csv(CSV_PATH)
print(f"Raw GPS fixes : {len(raw_df):,}")
print(f"Individuals   : {raw_df['id'].nunique()}")

# Sort by the individual ID and timestamp to ensure correct temporal order
raw_df = raw_df.sort_values(by=["id", "time"]).reset_index(drop=True)

raw_df.head()


# # Option to convert the 'time' column to local timezone
#raw_df["time"] = (
#        pd.to_datetime(raw_df["time"], utc=True)
#        .dt.tz_convert("Europe/Helsinki")
#    )

raw_df.head()


## Import environmental layers ####

env_layers, raster_transform = deepssf.load_environmental_layers(LAYER_PATHS)
print("Layers loaded:", list(env_layers.keys()))
for name, arr in env_layers.items():
    print(f"  {name}: shape={arr.shape}, min={arr.min():.3f}, max={arr.max():.3f}")

# Each band of a multi-band layer becomes one CNN input channel, so count bands
# (not layers) here and reuse the total when setting ModelParams below.  Deriving
# it means switching rasters in LAYER_PATHS needs no other edits.
N_SPATIAL_CHANNELS = sum(1 if arr.ndim == 2 else arr.shape[0] for arr in env_layers.values())
print(f"Spatial channels: {N_SPATIAL_CHANNELS}")


# Transform GPS data into steps data

# The CSV is already in amt step format, but prepare_movement_df() expects one row
# per *fix* and rebuilds the steps itself (it also derives bearing, dt_hour and the
# cyclic time encodings the model needs).  Point it at the step-start columns and
# let it re-form the steps from consecutive fixes.
step_df = deepssf.prepare_movement_df(
    raw_df, 
    id_col="id", 
    time_col="time", 
    x_col="x", 
    y_col="y"
)

n_dropped = len(raw_df) - len(step_df)
print(f"Movement steps : {len(step_df):,}  ({n_dropped} dropped — last per individual)")
step_df.head()

step_df["t1_"].dt.tz


# Define the model ####

# Indicating scalar columns:
SCALAR_COLS = ["hour_t1_sin1",
               "hour_t1_cos1", 
               "yday_t1_sin1", 
               "yday_t1_cos1", 
               "dt_hour"]

# Resolution of the rasters in CRS units (meters) and window size

PIXEL_SIZE = abs(raster_transform.a)

if not math.isclose(abs(raster_transform.a), abs(raster_transform.e)):
    raise ValueError(
        f"Non-square pixels ({abs(raster_transform.a)} x {abs(raster_transform.e)}); "
        "the model assumes square cells."
    )

WINDOW_SIZE = 141  

print(f"Pixel size: {PIXEL_SIZE} m  →  window {WINDOW_SIZE} px = "
      f"{WINDOW_SIZE * PIXEL_SIZE:,.0f} m across")

print(f"Maximum possible step: {((WINDOW_SIZE - 1) / 2) * PIXEL_SIZE:,.0f} m")


# Number of training samples (movement steps) per training mini-batch
BATCH_SIZE  = 256

# Number of convolutional layers

# Movement subnetwork has a max pooling layer, so more layers will result in 
# greater dimension reduction that will feed into the fully-connected layer that outputs 
# the movement parameters

N_CONV_LAYERS_MOVE = 3 

# Habitat subnetwork does not have max pooling layers, so there is no dimension reduction
# More layers will increase the depth of the model and allow for more abstracted features to
# be identified, but will slow model training/prediction.
# This count includes the final convolution layer, which is the one that collapses the 
# feature maps down to the single-channel habitat selection surface, so the minimum is 1.

N_CONV_LAYERS_HAB = 4


# Number of convolutional filters (and therefore feature maps) for each convolution layer. 
# More convolutional filters can provide more flexible functions, 
# but will increase the number of parameters and slow down training times.
OUTPUT_CHANNELS = 6

# Kernel size
KERNEL_SIZE     = 3     # conv kernel spatial size: Larger convolutional kernels have a greater pixel-neighbourhood awareness
KERNEL_SIZE_MP  = 2     # max-pool kernel size
STRIDE_MP       = 2     # max-pool stride 

# number of epochs (runs in the training loop)
N_EPOCHS = 100

# Pre-calculate the flattened CNN output size
# This value will be the input to the fully-connected layers in the movement subnetwork, 
# and is calculated here so that ModelParams can be set without hard-coding it.

DENSE_DIM = deepssf.flattened_conv_dim(
    image_dim=WINDOW_SIZE,
    n_conv_layers_move=N_CONV_LAYERS_MOVE,
    output_channels=OUTPUT_CHANNELS,
)
print(f"After {N_CONV_LAYERS_MOVE}x conv+maxpool: dense_dim_in_all={DENSE_DIM}")

DENSE_DIM_HIDDEN = 64 # hidden units in the scalar MLP
DROPOUT_RATE = 0.2 # proportion of hidden units to drop out during training (applies only to movement kernel)

# Remove steps outside calculated window
original_data_size = len(step_df)

step_df = deepssf.filter_steps_by_window(
    step_df, window_size=WINDOW_SIZE, pixel_size=PIXEL_SIZE
)

print(f"Initial number of steps: {original_data_size:,}")
print(f"Steps after window filter: {len(step_df):,}")
print(f"Data reduction: {original_data_size - len(step_df):,} ({(original_data_size - len(step_df)) / original_data_size * 100:.1f}%)")

# Remove steps with extreme dt_hour
step_df = step_df[step_df['dt_hour'] < 5]

print(f"Steps after removal of extreme dt_hour: {len(step_df):,}")

step_df.head()

## Next we will split data and create data loaders for the model
## This phase also crops the environmental layers for each step according to window size

## We will not be using the make_dataloaders() function provided by DeepSSF package as it does not split the data the way we want it to

## First, we will define how the data is split into training, validation and testing subsets

rng = np.random.default_rng(42) #initialize random number generator

ids = rng.permutation(np.asarray(step_df["id"].unique()))

# Next select individuals that are completely out of the training data set
frac_out = 0.1 # Fraction of individuals that are excluded from training (out of 33 individuals)

n_test_ids = max(1, int(frac_out * len(ids))) if len(ids) > 1 else 0 # based on the fraction, how many individuals are excluded
test_ids = set(ids[:n_test_ids]) # select the individuals for testing
trainval_ids = ids[n_test_ids:]  # select rest of the individuals for testing and validation split

test_df = step_df[step_df["id"].isin(test_ids)].reset_index(drop=True) # data frame of the individuals used for testing

# Now the remaining individuals are split into training and validation
# This is done by assigning steps into blocks of subsequent steps, 
# and then randomly assigning these blocks into training and validation sets

frac_val = 0.15   # fraction of each remaining individual's track used for validation
gap_steps = 2     # buffer dropped at the train/val boundary, to reduce autocorrelation leakage (how many steps are removed between training and validation portions)
block_days = 3   # size of each contiguous block

train_frames, val_frames, test_frames = [], [], []

for _, g in step_df[step_df["id"].isin(trainval_ids)].groupby("id", sort=False):
    g = g.sort_values("t1_").reset_index(drop=True)
    g["block"] = (pd.to_datetime(g["t1_"]) - pd.to_datetime(g["t1_"]).min()).dt.days // block_days

    blocks = g["block"].unique()
    rng.shuffle(blocks)
    n_val_blocks = max(1, int(frac_val * len(blocks)))
    val_blocks = set(blocks[:n_val_blocks])

    val_frames.append(g[g["block"].isin(val_blocks)])
    train_frames.append(g[~g["block"].isin(val_blocks)])

train_df = pd.concat(train_frames, ignore_index=True)
val_df = pd.concat(val_frames, ignore_index=True)

print(f"Individuals — train/val: {len(trainval_ids)}, test: {len(test_ids)}")
print(f"Steps — train: {len(train_df):,}, val: {len(val_df):,}, test: {len(test_df):,}")


# Prepares the split data for models
train_ds = deepssf.MovementDataset(train_df, LAYER_PATHS, WINDOW_SIZE, scalar_cols=SCALAR_COLS, bearing_col="bearing_tm1")
val_ds   = deepssf.MovementDataset(val_df,   LAYER_PATHS, WINDOW_SIZE, scalar_cols=SCALAR_COLS, bearing_col="bearing_tm1")
test_ds  = deepssf.MovementDataset(test_df,  LAYER_PATHS, WINDOW_SIZE, scalar_cols=SCALAR_COLS, bearing_col="bearing_tm1")

# Create data loaders
dl_kwargs = dict(batch_size=BATCH_SIZE, num_workers=10, pin_memory=True, persistent_workers=True, prefetch_factor=2, timeout=180)
dl_train = DataLoader(train_ds, shuffle=True,  **dl_kwargs)
dl_val   = DataLoader(val_ds,   shuffle=False, **dl_kwargs)
dl_test  = DataLoader(test_ds,  shuffle=False, **dl_kwargs)

print(f"Train batches : {len(dl_train)}")
print(f"Val   batches : {len(dl_val)}")
print(f"Test  batches : {len(dl_test)}")


# Sanity check
x1, x2, x3, (px2, py2), _ = next(iter(dl_train))
print(f"Spatial   x1 : {x1.shape}   (batch × channels × H × W)")
print(f"Scalars   x2 : {x2.shape}   (batch × len(SCALAR_COLS))")
print(f"Bearing   x3 : {x3.shape}   (batch × 1)")
print(f"Next-step px2: {px2[:4].tolist()}  py2: {py2[:4].tolist()}")


# Model parameters
params = deepssf.ModelParams({
    "batch_size":                BATCH_SIZE,
    "image_dim":                 WINDOW_SIZE,           # spatial window size (pixels)
    "pixel_size":                PIXEL_SIZE,            # metres per pixel
    "dim_in_nonspatial_to_grid": len(SCALAR_COLS),      # scalar cols tiled into window
    "dense_dim_in_nonspatial":   len(SCALAR_COLS),      # scalar inputs to the dense MLP
    "dense_dim_hidden":          DENSE_DIM_HIDDEN,                   # hidden units in the scalar MLP
    "dense_dim_in_all":          DENSE_DIM,             # flattened CNN output size
    # One channel per raster band (not per layer) + the tiled scalar covariates
    "input_channels":            N_SPATIAL_CHANNELS + len(SCALAR_COLS),
    "output_channels":           OUTPUT_CHANNELS,       # feature maps per conv layer
    "kernel_size":               KERNEL_SIZE,           # conv kernel spatial size
    "stride":                    1,                     # conv stride (should be kept at 1)
    "kernel_size_mp":            KERNEL_SIZE_MP,        # max-pool kernel size
    "stride_mp":                 STRIDE_MP,             # max-pool stride 
    "padding":                   1,                     # same-padding preserves spatial dimensions (should be kept at 1)
    "num_movement_params":       12,                    # movement kernel parameters (changing this will require modifying the movement kernel)
    "n_conv_layers_hab":         N_CONV_LAYERS_HAB,     # conv layers, habitat branch
    "n_conv_layers_move":        N_CONV_LAYERS_MOVE,    # conv+maxpool layers, movement
    "dropout":                   DROPOUT_RATE,          # dropout probability (0.0 = no dropout, 0.2 = 20% dropout)
    "device":                    DEVICE,
})
print(f"input_channels: {params.input_channels}  (must match x1 channels + len(SCALAR_COLS))")


# Run the model ####

model = deepssf.ConvJointModel(params).to(DEVICE)
n_params = sum(p.numel() for p in model.parameters())
print(f"Model parameters : {n_params:,}")

loss_fn = deepssf.negativeLogLikeLoss(reduction="mean")

optimisers, schedulers = deepssf.make_optimisers(
    model, 
    lr_habitat=1.0e-3, 
    lr_movement=1.0e-3, 
    scheduler_patience=3, 
    scheduler_factor=0.1
)

early_stop = deepssf.EarlyStopping(
    patience=7,
    verbose=True,
    monitor="both",                 # stop only when BOTH heads have plateaued
    path=str(OUTPUT_DIR / "best_model.pt"),
    checkpoint_on="active",         # a habitat-only stage saves on habitat loss
    # Also keep each head's own best epoch, across the whole run
    head_paths={
        "habitat":  str(OUTPUT_DIR / "best_model_habitat.pt"),
        "movement": str(OUTPUT_DIR / "best_model_movement.pt"),
    },
)



history = deepssf.fit(
    model=model,
    image_trim_pixels=4,
    window_size=WINDOW_SIZE,
    dl_train=dl_train,
    dl_val=dl_val,
    loss_fn=loss_fn,
    optimisers=optimisers,
    schedulers=schedulers,
    stages=[
        {"epochs": EPOCHS_MOV, "train": ("movement",)},   # availability kernel first
        {"epochs": EPOCHS_HAB, "train": ("habitat",)},    # habitat to its own convergence
        {"epochs": EPOCHS_BOTH, "train": ("habitat", "movement")},  # joint settle
    ],
    early_stopping=early_stop,
    snapshot_dir=str(SNAPSHOT_DIR),
    snapshot_item=250,
    batch_size=BATCH_SIZE,
)



## Model outputs ####

gif_path = str(OUTPUT_DIR / "training_progress.gif")
deepssf.create_gif(str(SNAPSHOT_DIR), gif_path, fps=5)
print(f"Training GIF → {gif_path}")

# Three panels rather than one: the habitat and movement components move on very
# different scales, so sharing an axis would flatten the habitat curve into a
# horizontal line.  x = 0 is the pre-training baseline recorded by fit().
uniform_nll = np.log(WINDOW_SIZE ** 2)
epochs = np.arange(len(history["val_losses"]))
stages = np.asarray(history["stage"])

fig, axes = plt.subplots(1, 3, figsize=(14, 3.6), constrained_layout=True)

panels = [
    ("Combined", history["val_losses"], history["train_losses"]),
    ("Habitat", history["val_habitat_losses"], None),
    ("Movement", history["val_movement_losses"], None),
]

for ax, (title, val, train) in zip(axes, panels):
    if train is not None:
        ax.plot(epochs, train, color="0.6", lw=1.5, label="train")
    ax.plot(epochs, val, color="C0", lw=1.5, label="val")
    if title == "Habitat":
        # A uniform surface over the window scores log(H*W), so this line is the
        # "learned nothing" mark — the whole question for the habitat head is how
        # far below it the curve gets.  Movement sits far below it from the first
        # epoch, and drawing it there would only compress that panel's axis.
        ax.axhline(uniform_nll, color="0.4", ls="--", lw=1,
                   label=f"uniform ({uniform_nll:.2f})")
    # Shade the staged-training blocks so the schedule is readable off the curve.
    for s in np.unique(stages[stages >= 0])[1::2]:
        within = epochs[stages == s]
        ax.axvspan(within.min() - 0.5, within.max() + 0.5, color="0.92", zorder=0)
    ax.set_xlabel("Epoch (0 = untrained baseline)")
    ax.set_ylabel("NLL (nats)")
    ax.set_title(title)
    ax.legend(frameon=False, fontsize=8)
    ax.spines[["top", "right"]].set_visible(False)

fig.suptitle("Training history — combined loss and its two components")
plt.savefig(str(OUTPUT_DIR / "loss_history.png"), dpi=100)



# Test the model ####

# validate_next_step_probs() and simulate_trajectory() both stack the list returned
# by get_landscape() along a new channel dimension, so every element must be a *2-D*
# tensor.  Unbind the band dimension of any multi-band layer so the channel order
# matches what MovementDataset fed the model during training.
landscape_tensors = []
for arr in env_layers.values():
    t = torch.from_numpy(np.asarray(arr).astype(np.float32))
    landscape_tensors.extend([t] if t.ndim == 2 else list(t))

print(f"Landscape channels: {len(landscape_tensors)}  (each {tuple(landscape_tensors[0].shape)})")

def get_landscape(_month_index):
    # Returns one 2-D tensor per spatial channel, in the same order as LAYER_PATHS.
    return landscape_tensors


test_sample = test_df.reset_index(drop=True)
print(f"Testing on {len(test_sample):,} steps from {test_sample['id'].nunique()} held-out individuals")


val_results = deepssf.validate_next_step_probs(
    model,
    test_sample,
    get_landscape=get_landscape,
    transform=raster_transform,
    window_size=WINDOW_SIZE,
    scalar_cols=tuple(SCALAR_COLS),
    yday_col="yday_t1",
    bearing_col="bearing_tm1",
    month_index_fn=lambda _yday: 0,   # static layers
)
print(val_results[["habitat_prob", "move_prob", "next_step_prob"]].describe())

# The uniform probability of any cell in the window is 1/(W*H) = 1/(W^2) since W=H.
uniform_prob = 1/(WINDOW_SIZE ** 2)
uniform_prob

fig, axes = plt.subplots(1, 3, figsize=(12, 3))
for ax, col, title in zip(
    axes,
    ["habitat_prob", "move_prob", "next_step_prob"],
    ["Habitat", "Movement", "Next-step"],
    strict=True,
):
    data = val_results[col].dropna()
    ax.hist(data, bins=40, edgecolor="white")
    ax.axvline(
        uniform_prob,
        color="black",
        linestyle="--",
        linewidth=1.5,
        label="Uniform (1/W²)",
    )
    ax.set_title(title)
    ax.set_xlabel("Probability")
    ax.ticklabel_format(axis="x", style="sci", scilimits=(-3, 3))
    ax.legend()

axes[0].set_ylabel("Count")
plt.suptitle("Next-step probability distributions (test set)", y=1.02)
plt.tight_layout()
plt.savefig(str(OUTPUT_DIR / "validation_probs.png"), dpi=100, bbox_inches="tight")

val_csv = OUTPUT_DIR / "next_step_probs.csv"
val_results.to_csv(val_csv, index=False)

# Simulated trajectories ####

if RUN_SIMULATIONS:
    n_replicates = 10        # repeats per starting yday
    yday_interval = 40       # days between starting ydays

    # 1, 41, 81, ..., 361 -> 10 starting ydays
    ydays_to_simulate = np.arange(1, 366, yday_interval)

    sim_dfs = []
    sim_counter = 0

    for animal_id, g in step_df.groupby("id", sort=False):
        g = g.sort_values("t1_").reset_index(drop=True)
        start_x = g["x1_"].iloc[0]
        start_y = g["y1_"].iloc[0]
        starting_hour = g["hour_t1"].iloc[0]

        print(f"Individual {animal_id} — start: ({start_x:.0f}, {start_y:.0f})")

        for yday in ydays_to_simulate:
            for rep in range(n_replicates):
                sim_df = deepssf.simulate_trajectory(
                    model,
                    get_landscape=get_landscape,
                    transform=raster_transform,
                    start_x=start_x,
                    start_y=start_y,
                    n_steps=1000,
                    starting_yday=int(yday),
                    starting_hour=starting_hour,
                    time_between_steps=1.0,
                    base_year=2010,
                    window_size=WINDOW_SIZE,
                    month_index_fn=lambda _yday: 0,  # static layers
                )
                sim_df["sim_id"] = sim_counter      # unique across all runs
                sim_df["replicate"] = rep           # 0..n_replicates-1 within each yday
                sim_df["id"] = animal_id
                sim_df["starting_yday"] = int(yday)
                sim_dfs.append(sim_df)
                sim_counter += 1

            print(f"  yday={yday}: {n_replicates} replicates done")

    all_sims_df = pd.concat(sim_dfs, ignore_index=True)
    all_sims_df.to_csv(OUTPUT_DIR / "simulated_trajectories.csv", index=False)
    print(
        f"Saved {len(all_sims_df)} rows across {all_sims_df['id'].nunique()} individuals × "
        f"{len(ydays_to_simulate)} ydays × {n_replicates} replicates"
    )
else:
    print("Skipping simulations (RUN_SIMULATIONS = False)")


# Habitat prediction maps ####

if RUN_PREDMAPS:
    PRED_YDAY_WINTER = 1
    PRED_YDAY_SUMMER = 180 

    PRED_HOURS_WINTER = [4, 16]
    PRED_HOURS_SUMMER = [2, 17]

    # CRS
    with rasterio.open(next(iter(LAYER_PATHS.values()))) as src:
        raster_crs = src.crs

    # Winter
    hourly_surfaces_winter = {}
    for hour in PRED_HOURS_WINTER:
        scalars_h = deepssf.make_simulation_inputs(
            n_steps=1, 
            starting_yday=PRED_YDAY_WINTER, 
            starting_hour=float(hour),
            time_between_steps=1.0,
        )[0][0]
        h, m = divmod(round(hour * 60), 60)
        hourly_surfaces_winter[hour] = deepssf.predict_habitat_landscape(
            model, landscape_rasters=get_landscape(0), scalars=scalars_h
        )
        print(f"  {h:02d}:{m:02d} done", end="\r")

    hours_sorted_winter = sorted(hourly_surfaces_winter)
    height, width = hourly_surfaces_winter[hours_sorted_winter[0]].shape

    habitat_tif_winter = OUTPUT_DIR / f"habitat_landscape_yday{PRED_YDAY_WINTER}_hourly.tif"

    with rasterio.open(
        habitat_tif_winter, "w",
        driver="GTiff",
        height=height,
        width=width,
        count=len(hours_sorted_winter),      # one band per hour
        dtype="float32",
        crs=raster_crs,
        transform=raster_transform,
        nodata=np.nan,          # the masked edge buffer
        compress="deflate",
    ) as dst:
        for band, hour in enumerate(hours_sorted_winter, start=1):   # GDAL bands are 1-indexed
            dst.write(hourly_surfaces_winter[hour].astype("float32"), band)
            # The description is what QGIS shows in its band dropdown; the tags keep
            # the hour machine-readable for anything parsing the metadata.
            dst.set_band_description(band, f"habitat selection log prob {hour:02d}:00")
            dst.update_tags(band, hour=hour, yday=PRED_YDAY_WINTER)
        dst.update_tags(prediction_yday=PRED_YDAY_WINTER, hours=",".join(str(h) for h in hours_sorted_winter))

    print(f"Wrote {habitat_tif_winter}")
    print(f"  {len(hours_sorted_winter)} bands, {width} x {height} px "
          f"({habitat_tif_winter.stat().st_size / 1e6:.1f} MB)")

    # Summer
    hourly_surfaces_summer = {}
    for hour in PRED_HOURS_SUMMER:
        scalars_h = deepssf.make_simulation_inputs(
            n_steps=1, 
            starting_yday=PRED_YDAY_SUMMER, 
            starting_hour=float(hour),
            time_between_steps=1.0,
        )[0][0]
        h, m = divmod(round(hour * 60), 60)
        hourly_surfaces_summer[hour] = deepssf.predict_habitat_landscape(
            model, landscape_rasters=get_landscape(0), scalars=scalars_h
        )
        print(f"  {h:02d}:{m:02d} done", end="\r")

    hours_sorted_summer = sorted(hourly_surfaces_summer)
    height, width = hourly_surfaces_summer[hours_sorted_summer[0]].shape

    habitat_tif_summer = OUTPUT_DIR / f"habitat_landscape_yday{PRED_YDAY_SUMMER}_hourly.tif"

    with rasterio.open(
        habitat_tif_summer, "w",
        driver="GTiff",
        height=height,
        width=width,
        count=len(hours_sorted_summer),      # one band per hour
        dtype="float32",
        crs=raster_crs,
        transform=raster_transform,
        nodata=np.nan,          # the masked edge buffer
        compress="deflate",
    ) as dst:
        for band, hour in enumerate(hours_sorted_summer, start=1):   # GDAL bands are 1-indexed
            dst.write(hourly_surfaces_summer[hour].astype("float32"), band)
            # The description is what QGIS shows in its band dropdown; the tags keep
            # the hour machine-readable for anything parsing the metadata.
            dst.set_band_description(band, f"habitat selection log prob {hour:02d}:00")
            dst.update_tags(band, hour=hour, yday=PRED_YDAY_SUMMER)
        dst.update_tags(prediction_yday=PRED_YDAY_SUMMER, hours=",".join(str(h) for h in hours_sorted_summer))

    print(f"Wrote {habitat_tif_summer}")
    print(f"  {len(hours_sorted_summer)} bands, {width} x {height} px "
          f"({habitat_tif_summer.stat().st_size / 1e6:.1f} MB)")
else:
    print("Skipping prediction maps (RUN_PREDMAPS = False)")


# Habitat suitability by forest age ####

if RUN_YDAY_SUMMARY:
    from rasterio.windows import Window  # not needed here, but harmless if you extend to crops

    SUMMARY_HOURS = [4, 17]          # fixed hour of day for each yday loop
    SUMMARY_YDAYS = range(1, 366)    # e.g. range(1, 366, 7) for weekly steps
    NORMALISE = True                 # False = raw log-intensity, better for comparing across days

    # Raw (un-normalised) forest age on the SAME grid the model sees (full raster)
    with rasterio.open(LAYER_PATHS["forest_age"]) as src:
        raw_age = src.read(1, masked=True)
    assert raw_age.shape == env_layers["forest_age"].shape, (raw_age.shape, env_layers["forest_age"].shape)

    age_array = raw_age.astype(np.float32).filled(np.nan).ravel()
    age_array[age_array == 0] = np.nan            # exclude forest age 0
    age_valid = np.isfinite(age_array)

    # Done once: map each valid cell to an integer age class
    age_classes, age_idx = np.unique(age_array[age_valid], return_inverse=True)
    n_classes = len(age_classes)

    landscape = get_landscape(0)

    for hour in SUMMARY_HOURS:
        frames = []
        for yday in SUMMARY_YDAYS:
            scalars_h = deepssf.make_simulation_inputs(
                n_steps=1,
                starting_yday=int(yday),
                starting_hour=float(hour),
                time_between_steps=1.0,
            )[0][0]

            pred = deepssf.predict_habitat_landscape(
                model, landscape_rasters=landscape, scalars=scalars_h, normalise=NORMALISE
            )

            vals = pred.ravel()[age_valid]
            ok = np.isfinite(vals)                 # drops the NaN edge buffer
            p = np.exp(vals[ok])
            idx = age_idx[ok]

            sums = np.bincount(idx, weights=p, minlength=n_classes)
            counts = np.bincount(idx, minlength=n_classes)
            with np.errstate(invalid="ignore", divide="ignore"):
                means = np.where(counts > 0, sums / counts, np.nan)

            keep = counts > 0
            frames.append(pd.DataFrame({
                "yday": int(yday),
                "forest_age": age_classes[keep],
                "mean_prediction": means[keep],
                "n_cells": counts[keep],
            }))

            if int(yday) % 30 == 0:
                print(f"  hour {hour:02d}: yday {yday} done", end="\r")

        summary_df = pd.concat(frames, ignore_index=True)
        out_csv = OUTPUT_DIR / f"wtdeer_yday_forest_suitability_by_age_hour{hour}.csv"
        summary_df.to_csv(out_csv, index=False)
        print(f"Saved {out_csv}")
else:
    print("Skipping yday summaries (RUN_YDAY_SUMMARY = False)")