## ============================================================
## smd_analysis.R
##
## Reproduces the functionality of:
##   run_persist.R   – launches persist_cmd.exe
##   smd_pipeline.R  – dates model output, matches obs/mod,
##                     computes goodness-of-fit, plots results
##
## Requirements:
##   - processx  (install.packages("processx"))
##   - base R only for all other steps
##
## Usage:
##   1. Edit the USER CONFIGURATION block below.
##   2. Run:  source("smd_analysis.R")
##      or:   Rscript smd_analysis.R
## ============================================================


# ================================================================
# USER CONFIGURATION
# ================================================================

par_file       <- "R1_01.par"  # .par parameter file
result_file    <- "r1.dat"     # Raw model result file (r*.dat)
obs_file       <- "R1_30.obs"  # .obs observation file (SMD only)

# Column in the raw model result file that contains modelled SMD.
# Columns are numbered from 1 in the raw .dat file.
# Example: for r1.dat  col 1 = flow, col 2 = 0, col 3 = SMD, col 4 = 0
smd_col_in_dat <- 3

# Output file names – leave "" to auto-generate from input names
dated_file   <- ""   # Model result with dates/times prepended
matched_file <- ""   # Matched obs / modelled SMD table
stats_file   <- ""   # Goodness-of-fit statistics text file
plot_file    <- ""   # PNG time-series plot

# PERSiST arguments (passed to run_persist)
inca_stub    <- "smd"
size_arg     <- "none"
timeout_secs <- 300

# ================================================================


# ----------------------------------------------------------------
# Helper: stop with a clean message (no traceback clutter)
# ----------------------------------------------------------------
bail <- function(...) stop(paste0(...), call. = FALSE)

# ----------------------------------------------------------------
# Helper: auto-generate output file names
# ----------------------------------------------------------------
auto_name <- function(base_file, suffix, ext) {
  paste0(tools::file_path_sans_ext(base_file), suffix, ".", ext)
}

if (dated_file   == "") dated_file   <- auto_name(result_file, "_dated",      "txt")
if (matched_file == "") matched_file <- auto_name(obs_file,    "_matched",    "txt")
if (stats_file   == "") stats_file   <- auto_name(obs_file,    "_statistics", "txt")
if (plot_file    == "") plot_file    <- auto_name(obs_file,    "_plot",       "png")


# ================================================================
# PART 1 – Run PERSiST
# (Reproduces run_persist.R)
# ================================================================
cat("==================================================\n")
cat("  PART 1: Running PERSiST\n")
cat("==================================================\n")

# Resolve script directory so persist_cmd.exe is found regardless of
# the working directory at the time of sourcing.
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)

executable <- file.path(script_dir, "persist_cmd.exe")

if (!file.exists(executable)) {
  bail("Cannot find persist_cmd.exe in: ", script_dir)
}

persist_args <- c(
  "-par",  par_file,
  "-dat",  "dat.dat",
  "-inca", inca_stub,
  "-size", size_arg
)

cat(sprintf("  Executable     : %s\n", executable))
cat(sprintf("  Arguments      : %s\n", paste(persist_args, collapse = " ")))

result <- processx::run(
  command         = executable,
  args            = persist_args,
  wd              = script_dir,
  stdout          = "|",
  stderr          = "|",
  timeout         = timeout_secs,
  error_on_status = FALSE
)

# PERSiST is silent on success; any stdout indicates an error
if (nchar(trimws(result$stdout)) > 0) {
  error_log <- file.path(script_dir, "error.lst")
  writeLines(result$stdout, error_log)
  bail(sprintf("PERSiST reported an error (exit %d). See: %s",
               result$status, error_log))
}

if (result$status != 0) {
  bail(sprintf("PERSiST exited with status %d: %s",
               result$status, trimws(result$stderr)))
}

cat("  PERSiST completed successfully.\n\n")


# ================================================================
# PART 2 – SMD Pipeline
# (Reproduces smd_pipeline.R)
# ================================================================
cat("==================================================\n")
cat("  PART 2: SMD Pipeline\n")
cat("==================================================\n")
cat(sprintf("  .par file      : %s\n", par_file))
cat(sprintf("  Model file     : %s\n", result_file))
cat(sprintf("  Obs file       : %s\n", obs_file))
cat(sprintf("  SMD column     : %d (in raw model file)\n", smd_col_in_dat))
cat("--------------------------------------------------\n\n")


# ----------------------------------------------------------------
# STEP 1 – Add dates and times to the raw model result file
# ----------------------------------------------------------------
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

# Line 3: start date/time  (DD/MM/YYYY HH:MM:SS)
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

# Build timestamp sequence: row i gets timestamp  start + i * time_step
timestamps <- start_dt + seq_len(n_rows) * time_step_sec

dated_df <- cbind(
  data.frame(
    Date = format(timestamps, "%d/%m/%Y"),
    Time = format(timestamps, "%H:%M:%S"),
    stringsAsFactors = FALSE
  ),
  result_data
)

write.table(dated_df, file = dated_file, sep = "\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

cat(sprintf("  Output written : %s\n\n", dated_file))


# ----------------------------------------------------------------
# STEP 2 – Extract and match observed vs modelled SMD
# ----------------------------------------------------------------
cat("STEP 2: Matching observed and modelled SMD ...\n")

# ---- Read the .obs file ----
if (!file.exists(obs_file))
  bail("Cannot find .obs file: ", obs_file)

obs_lines <- readLines(obs_file, warn = FALSE)

if (length(obs_lines) < 3)
  bail(".obs file has fewer than 3 lines: ", obs_file)

# Verify this file contains SMD (checked against the 2-line header)
header_block <- paste(obs_lines[1:2], collapse = "\n")
if (!grepl("SMD", header_block, ignore.case = TRUE))
  bail(
    "This .obs file does not appear to contain SMD data.\n",
    "  Header line 1: ", obs_lines[1], "\n",
    "  Header line 2: ", obs_lines[2]
  )

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

# ---- Read the dated model file produced in Step 1 ----
mod_df <- tryCatch(
  read.table(dated_file, header = FALSE, sep = "\t",
             fill = TRUE, stringsAsFactors = FALSE),
  error = function(e) bail("Could not read dated model file: ", e$message)
)
if (nrow(mod_df) == 0)
  bail("Dated model file is empty: ", dated_file)

n_cols <- ncol(mod_df)

# In the dated file the first two columns are Date and Time;
# the original model columns start at position 3.
smd_col_dated <- smd_col_in_dat + 2L

if (smd_col_dated < 3 || smd_col_dated > n_cols)
  bail(sprintf(
    "smd_col_in_dat=%d maps to column %d in the dated file, which has %d columns.",
    smd_col_in_dat, smd_col_dated, n_cols
  ))

names(mod_df)[1]            <- "Date"
names(mod_df)[2]            <- "Time"
names(mod_df)[smd_col_dated] <- "SMD_mod"
mod_df$SMD_mod <- suppressWarnings(as.numeric(mod_df$SMD_mod))
mod_df         <- mod_df[!is.na(mod_df$SMD_mod), c("Date", "Time", "SMD_mod")]
cat(sprintf("  Modelled rows  : %d\n", nrow(mod_df)))

# ---- Inner join on (Date, Time) ----
obs_df$key <- paste(trimws(obs_df$Date), trimws(obs_df$Time))
mod_df$key <- paste(trimws(mod_df$Date), trimws(mod_df$Time))

merged_df <- merge(
  obs_df[, c("key", "Date", "Time", "SMD_obs")],
  mod_df[, c("key", "SMD_mod")],
  by  = "key",
  all = FALSE
)

if (nrow(merged_df) == 0) {
  cat("\nWARNING: No matching (Date, Time) pairs found.\n")
  cat("  First few observed timestamps:\n")
  print(head(obs_df[, c("Date", "Time")], 5))
  cat("  First few modelled timestamps:\n")
  print(head(mod_df[, c("Date", "Time")], 5))
  bail("No matched rows — cannot proceed.")
}

merged_df <- merged_df[order(merged_df$key), ]
output_df <- merged_df[, c("Date", "Time", "SMD_obs", "SMD_mod")]

write.table(output_df, file = matched_file, sep = "\t",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

cat(sprintf("  Matched rows   : %d\n", nrow(output_df)))
cat(sprintf("  Output written : %s\n\n", matched_file))


# ----------------------------------------------------------------
# STEP 3 – Goodness-of-fit statistics
# ----------------------------------------------------------------
cat("STEP 3: Computing goodness-of-fit statistics ...\n")

obs <- output_df$SMD_obs
mod <- output_df$SMD_mod
n   <- length(obs)

if (n < 2)
  bail("Need at least 2 matched pairs to compute statistics; found ", n)

# R² – square of the Pearson correlation coefficient
r_squared <- cor(obs, mod)^2

# Nash–Sutcliffe Efficiency
#   NS = 1 - [ Σ(obs - mod)² / Σ(obs - mean(obs))² ]
ns <- 1 - (sum((obs - mod)^2) / sum((obs - mean(obs))^2))

# Kling–Gupta Efficiency (Gupta et al., 2009)
#   KGE = 1 - sqrt( (r-1)² + (α-1)² + (β-1)² )
#   r = Pearson correlation
#   α = sd(mod) / sd(obs)    variability ratio
#   β = mean(mod) / mean(obs) bias ratio
r_kge <- cor(obs, mod)
alpha <- sd(mod)   / sd(obs)
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


# ----------------------------------------------------------------
# STEP 4 – Time-series plot
# ----------------------------------------------------------------
cat("STEP 4: Producing time-series plot ...\n")

dt_str    <- paste(trimws(output_df$Date), trimws(output_df$Time))
datetimes <- as.POSIXct(dt_str, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")

y_range <- range(c(obs, mod), na.rm = TRUE)
y_pad   <- diff(y_range) * 0.08
y_lim   <- y_range + c(-y_pad, y_pad)

png(plot_file, width = 1200, height = 600, res = 120)

par(mar = c(5, 5, 4, 2))

plot(datetimes, obs,
     type = "p", pch = 16, cex = 0.6, col = "steelblue",
     xlab = "Date", ylab = "SMD (mm)",
     main = sprintf(
       "SMD: Observed vs Modelled\n%s  |  R\u00b2=%.3f  NS=%.3f  KGE=%.3f",
       tools::file_path_sans_ext(obs_file),
       r_squared, ns, kge
     ),
     ylim = y_lim,
     xaxt = "n")

points(datetimes, mod, pch = 17, cex = 0.6, col = "firebrick")

axis.POSIXct(1, datetimes, format = "%d/%m/%y", las = 2, cex.axis = 0.8)

legend("topright",
       legend = c("Observed", "Modelled"),
       col    = c("steelblue", "firebrick"),
       pch    = c(16, 17),
       bg     = "white", box.lty = 1)

dev.off()
cat(sprintf("  Plot written      : %s\n\n", plot_file))


# ================================================================
# SUMMARY
# ================================================================
cat("==================================================\n")
cat("  Analysis complete.  Output files:\n")
cat(sprintf("    Dated model  : %s\n", dated_file))
cat(sprintf("    Matched SMD  : %s\n", matched_file))
cat(sprintf("    Statistics   : %s\n", stats_file))
cat(sprintf("    Plot         : %s\n", plot_file))
cat("==================================================\n")
