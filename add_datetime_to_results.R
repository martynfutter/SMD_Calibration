## ============================================================
## add_datetime_to_results.R
##
## Prepends date and time columns to a model result file,
## using the start date/time and time step from a .par file.
##
## Output date/time format matches .obs files:
##   DD/MM/YYYY <tab> HH:MM:SS <tab> col1 <tab> col2 ...
## ============================================================

# ----- USER CONFIGURATION -----------------------------------
# Set these two paths before running the script.
# Use forward slashes or double back-slashes on Windows.

par_file    <- "R1_01.par"    # path to the .par parameter file
result_file <- "r1.dat"       # path to the model result file
output_file <- "r1_dates.dat" # path for the output file
                              # (leave "" to auto-generate)
# ------------------------------------------------------------


# ---- Helper: stop with a clear message ----
bail <- function(...) stop(paste0(...), call. = FALSE)


# ---- 1. Read the .par file ----
if (!file.exists(par_file))
  bail("Cannot find .par file: ", par_file)

par_lines <- readLines(par_file, warn = FALSE)

if (length(par_lines) < 3)
  bail(".par file has fewer than 3 lines: ", par_file)

# Line 1: time step in seconds
time_step_sec <- suppressWarnings(as.numeric(trimws(par_lines[1])))
if (is.na(time_step_sec) || time_step_sec <= 0)
  bail("Line 1 of .par file is not a positive number: '", par_lines[1], "'")

# Line 3: start date and time  (DD/MM/YYYY HH:MM:SS)
start_str <- trimws(par_lines[3])
start_dt  <- tryCatch(
  as.POSIXct(start_str, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
  error = function(e) NA
)
if (is.na(start_dt))
  bail("Cannot parse start date/time on line 3 of .par file: '", start_str, "'")

cat(sprintf("Time step    : %g seconds\n", time_step_sec))
cat(sprintf("Start time   : %s\n", format(start_dt, "%d/%m/%Y %H:%M:%S")))


# ---- 2. Read the model result file ----
if (!file.exists(result_file))
  bail("Cannot find result file: ", result_file)

result_data <- tryCatch(
  read.table(result_file, header = FALSE, sep = "", fill = TRUE,
             comment.char = "", stringsAsFactors = FALSE),
  error = function(e) bail("Could not read result file: ", e$message)
)

n_rows <- nrow(result_data)
if (n_rows == 0)
  bail("Result file contains no data rows: ", result_file)

cat(sprintf("Result rows  : %d\n", n_rows))
cat(sprintf("Result cols  : %d\n", ncol(result_data)))


# ---- 3. Build the datetime sequence ----
# Each row corresponds to one time step after the start time.
# Row 1 = start_dt + 1 * time_step_sec, row 2 = start + 2 * ..., etc.
# Adjust the multiplier below if row 1 should equal the start time itself.

timestamps <- start_dt + seq_len(n_rows) * time_step_sec

date_col <- format(timestamps, "%d/%m/%Y")
time_col <- format(timestamps, "%H:%M:%S")


# ---- 4. Build the output data frame ----
output_df <- cbind(
  data.frame(Date = date_col, Time = time_col, stringsAsFactors = FALSE),
  result_data
)


# ---- 5. Write the output file ----
if (output_file == "") {
  base  <- tools::file_path_sans_ext(result_file)
  output_file <- paste0(base, "_dated.txt")
}

write.table(
  output_df,
  file      = output_file,
  sep       = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote     = FALSE
)

cat(sprintf("Output written: %s\n", output_file))
