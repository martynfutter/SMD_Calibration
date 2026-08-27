## ============================================================
## smd_pipeline.R
##
## End-to-end pipeline for Soil Moisture Deficit (SMD) model
## evaluation.  Given a .par file, a model result file (r*.dat),
## and a .obs file, this script:
##
##   Step 1 - Adds dates/times to the raw model result file.
##   Step 2 - Extracts and matches observed vs modelled SMD.
##   Step 3 - Calculates goodness-of-fit statistics
##              (R^2, Nash-Sutcliffe, Kling-Gupta).
##   Step 4 - Produces a time-series plot (PNG).
##
## ------------------------------------------------------------
## FIX (this version): the previous version sorted matched rows
## with order(merged_df$key), where 'key' is a TEXT string built
## as "DD/MM/YYYY HH:MM:SS". Sorting that alphabetically sorts by
## day-of-month first, then month, then year - NOT chronologically.
## e.g. "01/01/2025 00:00:00" sorts before "18/12/2024 23:30:00"
## as text, even though it comes later in real time.
##
## Because plot()/lines() in base R draw points in the order the
## data frame is in (they do not auto-sort by x), this produced a
## line that jumped around between dates instead of tracing a
## smooth time series - the spiky/crisscrossed plot you saw.
##
## The fix: build a real POSIXct datetime column and sort on THAT,
## not on the text key. The text key is still used for the merge
## (which is fine - merge() doesn't care about ordering), but the
## final ordering step now uses the parsed datetime.
## ------------------------------------------------------------
##
## Requirements: base R only (no packages needed for steps 1-3).
##               'graphics' is used for the plot (base R).
##
## Usage:
##   1. Edit the USER CONFIGURATION block below.
##   2. Source the script or run it from the command line:
##        Rscript smd_pipeline.R
## ============================================================


# ================================================================
# USER CONFIGURATION
# ================================================================

par_file    <- "R1_01.par"   # Path to the .par parameter file
result_file <- "r1.dat"      # Path to the raw model result file

obs_file    <- "R1_30.obs"   # Path to the .obs observation file

# Column in the model result file that contains modelled SMD.
# The raw .dat file columns are numbered from 1.
# For r1.dat: col 1 = flow, col 2 = 0, col 3 = SMD, col 4 = 0
smd_col_in_dat <- 3

# Output file names (leave "" to auto-generate from input names)
dated_file   <- ""   # model result file with dates prepended
matched_file <- ""   # matched obs/model SMD file
stats_file   <- ""   # goodness-of-fit statistics text file
plot_file    <- ""   # PNG time-series plot

# ================================================================


# ---- Helper ----
bail <- function(...) stop(paste0(...), call. = FALSE)

auto_name <- function(base_file, suffix, ext) {
  paste0(tools::file_path_sans_ext(base_file), suffix, ".", ext)
}

if (dated_file   == "") dated_file   <- auto_name(result_file, "_dated", "txt")
if (matched_file == "") matched_file <- auto_name(obs_file,    "_matched", "txt")
if (stats_file   == "") stats_file   <- auto_name(obs_file,    "_statistics", "txt")
if (plot_file    == "") plot_file    <- auto_name(obs_file,    "_plot", "png")

cat("==================================================\n")
cat("  SMD Pipeline\n")
cat("==================================================\n")
cat(sprintf("  .par file      : %s\n", par_file))
cat(sprintf("  Model file     : %s\n", result_file))
cat(sprintf("  Obs file       : %s\n", obs_file))
cat(sprintf("  SMD column     : %d (in raw model file)\n", smd_col_in_dat))
cat("--------------------------------------------------\n\n")


# ================================================================
# STEP 1 - Add dates and times to the raw model result file
# ================================================================
cat("STEP 1: Adding dates/times to model result file ...\n")

if (!file.exists(par_file))
  bail("Cannot find .par file: ", par_file)

par_lines <- readLines(par_file, warn = FALSE)

if (length(par_lines) < 3)
  bail(".par file has fewer than 3 lines: ", par_file)

# Line 1: time step in seconds
time_step_sec <- suppressWarnings(as.numeric(trimws(par_lines[1])))
if (is.na(time_step_sec) || time_step_sec <= 0)
  bail("Line 1 of .par file is not a positive number: '", par_lines[1], "'")

# Line 3: start date/time (DD/MM/YYYY HH:MM:SS)
start_str <- trimws(par_lines[3])
start_dt  <- tryCatch(
  as.POSIXct(start_str, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
  error = function(e) NA
)
if (is.na(start_dt))
  bail("Cannot parse start date/time on line 3 of .par: '", start_str, "'")

cat(sprintf("  Time step      : %g seconds\n", time_step_sec))
cat(sprintf("  Start time     : %s\n", format(start_dt, "%d/%m/%Y %H:%M:%S")))

if (!file.exists(result_file))
  bail("Cannot find model result file: ", result_file)

result_data <- tryCatch(
  read.table(result_file, header = FALSE, sep = "", fill = TRUE,
             comment.char = "", stringsAsFactors = FALSE),
  error = function(e) bail("Could not read result file: ", e$message)
)

n_rows <- nrow(result_data)
if (n_rows == 0) bail("Model result file is empty: ", result_file)

cat(sprintf("  Result rows    : %d  (%d columns)\n", n_rows, ncol(result_data)))

# Build timestamp sequence: row i corresponds to start + i * time_step
timestamps <- start_dt + seq_len(n_rows) * time_step_sec

dated_df <- cbind(
  data.frame(Date = format(timestamps, "%d/%m/%Y"),
             Time = format(timestamps, "%H:%M:%S"),
             stringsAsFactors = FALSE),
  result_data
)

write.table(dated_df, file = dated_file, sep = "\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

cat(sprintf("  Output written : %s\n\n", dated_file))


# ================================================================
# STEP 2 - Extract and match observed vs modelled SMD
# ================================================================
cat("STEP 2: Matching observed and modelled SMD ...\n")

# ---- Read the .obs file ----
if (!file.exists(obs_file))
  bail("Cannot find .obs file: ", obs_file)

obs_lines <- readLines(obs_file, warn = FALSE)

if (length(obs_lines) < 3)
  bail(".obs file has fewer than 3 lines: ", obs_file)

header_block <- paste(obs_lines[1:2], collapse = "\n")
if (!grepl("SMD", header_block, ignore.case = TRUE))
  bail("This .obs file does not appear to contain SMD data.\n",
       "  Header line 1: ", obs_lines[1], "\n",
       "  Header line 2: ", obs_lines[2])

# Parse data rows (skip the 2-line header)
data_lines <- obs_lines[-(1:2)]
data_lines <- data_lines[nzchar(trimws(data_lines))]
if (length(data_lines) == 0)
  bail("No data rows in .obs file after header: ", obs_file)

obs_parsed <- strsplit(trimws(data_lines), "[ \t]+")
valid      <- sapply(obs_parsed, length) >= 3
obs_parsed <- obs_parsed[valid]

obs_df <- data.frame(
  Date    = sapply(obs_parsed, `[[`, 1),
  Time    = sapply(obs_parsed, `[[`, 2),
  SMD_obs = suppressWarnings(as.numeric(sapply(obs_parsed, `[[`, 3))),
  stringsAsFactors = FALSE
)
obs_df <- obs_df[!is.na(obs_df$SMD_obs), ]
cat(sprintf("  Observed rows  : %d\n", nrow(obs_df)))

# ---- Read the dated model file ----
mod_df <- tryCatch(
  read.table(dated_file, header = FALSE, sep = "\t",
             fill = TRUE, stringsAsFactors = FALSE),
  error = function(e) bail("Could not read dated model file: ", e$message)
)
if (nrow(mod_df) == 0)
  bail("Dated model file is empty: ", dated_file)

n_cols <- ncol(mod_df)
# In the dated file, columns 1 and 2 are Date and Time;
# the model columns start at 3.
smd_col_dated <- smd_col_in_dat + 2L

if (smd_col_dated < 3 || smd_col_dated > n_cols)
  bail(sprintf(
    "smd_col_in_dat=%d maps to column %d in the dated file, which has %d columns.",
    smd_col_in_dat, smd_col_dated, n_cols))

names(mod_df)[1] <- "Date"
names(mod_df)[2] <- "Time"
names(mod_df)[smd_col_dated] <- "SMD_mod"
mod_df$SMD_mod <- suppressWarnings(as.numeric(mod_df$SMD_mod))
mod_df <- mod_df[!is.na(mod_df$SMD_mod), c("Date", "Time", "SMD_mod")]
cat(sprintf("  Modelled rows  : %d\n", nrow(mod_df)))

# ---- Inner join on (Date, Time) ----
# The 'key' string is only used to MATCH rows between the two files.
# It must NOT be used to order them (see fix note below).
obs_df$key <- paste(trimws(obs_df$Date), trimws(obs_df$Time))
mod_df$key <- paste(trimws(mod_df$Date), trimws(mod_df$Time))

merged_df <- merge(
  obs_df[, c("key", "Date", "Time", "SMD_obs")],
  mod_df[, c("key", "SMD_mod")],
  by = "key", all = FALSE
)

if (nrow(merged_df) == 0) {
  cat("\nWARNING: No matching (Date, Time) pairs found.\n")
  cat("  First few observed timestamps:\n")
  print(head(obs_df[, c("Date", "Time")], 5))
  cat("  First few modelled timestamps:\n")
  print(head(mod_df[, c("Date", "Time")], 5))
  bail("No matched rows - cannot proceed.")
}

# ------------------------------------------------------------
# *** THE FIX ***
# Previously: merged_df <- merged_df[order(merged_df$key), ]
#   This sorted the "DD/MM/YYYY HH:MM:SS" string ALPHABETICALLY,
#   which groups rows by day-of-month first (not by real time),
#   scrambling the chronological order across months/years.
#
# Now: parse a real datetime and sort by that instead.
# ------------------------------------------------------------
merged_df$datetime <- as.POSIXct(
  paste(merged_df$Date, merged_df$Time),
  format = "%d/%m/%Y %H:%M:%S",
  tz = "UTC"
)

if (any(is.na(merged_df$datetime)))
  cat(sprintf(
    "  WARNING: %d matched row(s) had a Date/Time that failed to parse and will sort last.\n",
    sum(is.na(merged_df$datetime))))

merged_df <- merged_df[order(merged_df$datetime), ]

output_df <- merged_df[, c("Date", "Time", "SMD_obs", "SMD_mod")]

write.table(output_df, file = matched_file, sep = "\t",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

cat(sprintf("  Matched rows   : %d\n", nrow(output_df)))
cat(sprintf("  Output written : %s\n\n", matched_file))


# ================================================================
# STEP 3 - Goodness-of-fit statistics
# ================================================================
cat("STEP 3: Computing goodness-of-fit statistics ...\n")

obs <- output_df$SMD_obs
mod <- output_df$SMD_mod
n   <- length(obs)

if (n < 2)
  bail("Need at least 2 matched pairs to compute statistics; found ", n)

# R^2
r_squared <- cor(obs, mod)^2

# Nash-Sutcliffe Efficiency
ns <- 1 - (sum((obs - mod)^2) / sum((obs - mean(obs))^2))

# Kling-Gupta Efficiency (Gupta et al., 2009)
r_kge <- cor(obs, mod)
alpha <- sd(mod)  / sd(obs)
beta  <- mean(mod) / mean(obs)
kge   <- 1 - sqrt((r_kge - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)

stat_lines <- c(
  "==============================================",
  "  Goodness-of-Fit Statistics: SMD",
  "==============================================",
  sprintf("  Parameter file                : %s", par_file),
  sprintf("  Model result file             : %s", result_file),
  sprintf("  Observation file              : %s", obs_file),
  sprintf("  Matched pairs                 : %d", n),
  "----------------------------------------------",
  sprintf("  R2  (Coeff. of Determination) : %8.4f", r_squared),
  sprintf("  NS  (Nash-Sutcliffe Eff.)     : %8.4f", ns),
  sprintf("  KGE (Kling-Gupta Eff.)        : %8.4f", kge),
  "----------------------------------------------",
  "  KGE components:",
  sprintf("    r     (correlation)          : %8.4f", r_kge),
  sprintf("    alpha (variability ratio)    : %8.4f", alpha),
  sprintf("    beta  (bias ratio)           : %8.4f", beta),
  "==============================================",
  "",
  "Interpretation:",
  "  R2  : 1.0 = perfect.  Higher is better.",
  "  NS  : 1.0 = perfect.  0 = no better than the observed mean.",
  "        Negative = poor performance.",
  "  KGE : 1.0 = perfect.  > -0.41 outperforms the mean benchmark",
  "        (Knoben et al., 2019)."
)

cat(paste(stat_lines, collapse = "\n"), "\n\n")
writeLines(stat_lines, con = stats_file)
cat(sprintf("  Statistics written: %s\n\n", stats_file))


# ================================================================
# STEP 4 - Time-series plot
# ================================================================
cat("STEP 4: Producing time-series plot ...\n")

# output_df is now in true chronological order (fixed in Step 2),
# so datetimes here will line up with obs/mod row-for-row correctly.
dt_str    <- paste(trimws(output_df$Date), trimws(output_df$Time))
datetimes <- as.POSIXct(dt_str, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")

y_range <- range(c(obs, mod), na.rm = TRUE)
y_pad   <- diff(y_range) * 0.08
y_lim   <- y_range + c(-y_pad, y_pad)

png(plot_file, width = 1200, height = 600, res = 120)

par(mar = c(5, 5, 4, 2))

plot(datetimes, obs,
     type = "l", col = "steelblue", lwd = 1.5,
     xlab = "Date", ylab = "SMD (mm)",
     main = sprintf("SMD: Observed vs Modelled\n%s  |  R\u00b2=%.3f  NS=%.3f  KGE=%.3f",
                    tools::file_path_sans_ext(obs_file),
                    r_squared, ns, kge),
     ylim = y_lim,
     xaxt = "n")

lines(datetimes, mod, col = "firebrick", lwd = 1.5, lty = 2)

# Sensible date axis
axis.POSIXct(1, datetimes, format = "%d/%m/%y", las = 2, cex.axis = 0.8)

legend("topright",
       legend = c("Observed", "Modelled"),
       col    = c("steelblue", "firebrick"),
       lty    = c(1, 2), lwd = 2,
       bg     = "white", box.lty = 1)

dev.off()
cat(sprintf("  Plot written      : %s\n\n", plot_file))


# ================================================================
# SUMMARY
# ================================================================
cat("==================================================\n")
cat("  Pipeline complete.  Output files:\n")
cat(sprintf("    Dated model  : %s\n", dated_file))
cat(sprintf("    Matched SMD  : %s\n", matched_file))
cat(sprintf("    Statistics   : %s\n", stats_file))
cat(sprintf("    Plot         : %s\n", plot_file))
cat("==================================================\n")
