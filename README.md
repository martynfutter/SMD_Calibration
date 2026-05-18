# SMD Model Calibration Repository

This repository contains input, output and observation files for a distributed hydrological model that simulates **Soil Moisture Deficit (SMD)** at the **RAP (Bosque)** reach.  Accompanying R scripts provide a complete pipeline for adding timestamps to raw model output, matching it against field observations, and computing goodness-of-fit statistics.

---

## Repository Structure

```
├── *.par              Model parameter files
├── *.obs              Observation files (SMD field measurements)
├── r*.dat             Raw model result files (one per model run)
├── smd_pipeline.R     End-to-end analysis pipeline (recommended entry point)
├── add_datetime_to_results.R   Step 1 standalone script
├── match_smd.R                 Step 2 standalone script
├── smd_statistics.R            Step 3 standalone script
└── README.md
```

---

## File Format Reference

### Parameter files (`.par`)

Each `.par` file configures a single model run.  The naming convention is `<ReachID>_<RunID>.par` (e.g. `R1_01.par`).

| Line | Content | Example |
|------|---------|---------|
| 1 | Time step in seconds | `1800` |
| 2 | Number of time steps | `72838` |
| 3 | Run start date/time (`DD/MM/YYYY HH:MM:SS`) | `01/10/2021 00:00:00` |
| 4+ | Model parameters by component | — |

The parameter blocks, in order, are:

- **Quick** – fast (surface/near-surface) runoff store
- **QuickRip** – fast runoff store for the riparian zone
- **Soil** – soil water store
- **SoilRip** – soil water store for the riparian zone
- **Groundwater** – groundwater/baseflow store
- **RAP** – reach routing and vegetation parameters

Each store typically contains paired values representing two sub-reaches or land-use zones.  The final lines of the `RAP` block define the routing matrix between sub-reaches.

### Observation files (`.obs`)

Each `.obs` file contains field-measured values for a single reach and parameter.  The naming convention is `<ReachID>_<RunID>.obs` (e.g. `R1_30.obs`).

**Structure:**

```
************** Reach <name> ***********
-------------- <parameter> -----------
DD/MM/YYYY    HH:MM:SS    <value>
DD/MM/YYYY    HH:MM:SS    <value>
...
```

- Line 1: reach identifier (e.g. `Reach RAP`)
- Line 2: parameter name (this repository uses `SMD`)
- Lines 3+: tab-separated observations — date, time, numeric value

Only files where line 2 contains `SMD` are used by the analysis scripts.

**Example (from `R1_30.obs`):**

```
************** Reach RAP ***********
-------------- SMD -----------
18/12/2024    12:00:00    6.15
18/12/2024    12:30:00    6.15
...
```

Observations span from **18 December 2024** onwards at **30-minute** intervals.

### Model result files (`r*.dat`)

Raw model output files are named `r<RunID>.dat` (e.g. `r1.dat`).  They are space-delimited with **no header** and **no timestamps**.  Each row corresponds to one time step; the first row corresponds to `start_time + 1 × time_step`.

The file contains four columns:

| Column | Content |
|--------|---------|
| 1 | Simulated flow (cumecs) |
| 2 | Reserved (always 0) |
| 3 | **Simulated SMD (mm)** |
| 4 | Reserved (always 0) |

Timestamps must be reconstructed from the corresponding `.par` file before the results can be compared with observations (see pipeline below).

---

## Analysis Pipeline

The recommended workflow is the single consolidated script `smd_pipeline.R`.  Alternatively, the three standalone scripts can be run in sequence.

### Quick start

1. Place `smd_pipeline.R` in the same directory as your data files.
2. Edit the **USER CONFIGURATION** block at the top:

```r
par_file       <- "R1_01.par"   # .par file for this run
result_file    <- "r1.dat"      # raw model result file
obs_file       <- "R1_30.obs"   # observation file (SMD)
smd_col_in_dat <- 3             # column number of SMD in the .dat file
```

3. Run the script:

```bash
Rscript smd_pipeline.R
```

No additional R packages are required (base R only).

### Pipeline steps

| Step | Script | Description | Output |
|------|--------|-------------|--------|
| 1 | `add_datetime_to_results.R` | Reads time step and start date from the `.par` file; prepends `Date` and `Time` columns to the `.dat` file | `r1_dated.txt` |
| 2 | `match_smd.R` | Extracts SMD from the dated model file; performs an inner join with the `.obs` file on `(Date, Time)` | `R1_30_matched.txt` |
| 3 | `smd_statistics.R` | Computes R², Nash–Sutcliffe Efficiency, and Kling–Gupta Efficiency | `R1_30_statistics.txt` |
| 4 | *(pipeline only)* | Produces a time-series comparison plot | `R1_30_plot.png` |

### Goodness-of-fit statistics

| Statistic | Perfect score | Notes |
|-----------|--------------|-------|
| R² | 1.0 | Coefficient of determination |
| NS | 1.0 | Nash–Sutcliffe Efficiency; 0 = no better than the observed mean; negative = poor |
| KGE | 1.0 | Kling–Gupta Efficiency (Gupta et al., 2009); values > −0.41 outperform the mean benchmark (Knoben et al., 2019) |

---

## Model Overview

The model simulates the hydrological behaviour of the **RAP** (Riparian And Planted) reach within the **Bosque** catchment.  It represents two zones (main channel and riparian) with separate fast-flow, soil-water, and groundwater stores that drain through a routing matrix into the river reach.

Key run parameters for the files included in this repository:

| Parameter | Value |
|-----------|-------|
| Time step | 1800 s (30 min) |
| Start date | 01 October 2021 00:00 |
| Number of steps | 72 838 |
| Approximate end date | ~December 2025 |
| Observation period | December 2024 – present |

> **Note:** Additional `.par` and `.obs` files for other reaches or run configurations may be present in the repository.  The scripts will work with any `.par`/`.obs`/`r*.dat` combination provided the file naming conventions above are followed and the relevant column numbers are set correctly.

---

## References

Gupta, H. V., Kling, H., Yilmaz, K. K., & Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. *Journal of Hydrology*, 377(1–2), 80–91.

Knoben, W. J. M., Freer, J. E., & Woods, R. A. (2019). Technical note: Inherent benchmark or not? Comparing Nash–Sutcliffe and Kling–Gupta efficiency scores. *Hydrology and Earth System Sciences*, 23(10), 4323–4331.
