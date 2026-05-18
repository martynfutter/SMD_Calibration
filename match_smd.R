## ============================================================
## match_smd.R
##
## Matches observed SMD values (from a .obs file) with
## modelled SMD values (from the dated output file produced
## by add_datetime_to_results.R).
##
## Output: tab-separated file with 4 columns —
##   Date | Time | SMD_obs | SMD_mod
## Only rows where BOTH observed and modelled values exist
## for the same (Date, Time) tuple are written.
## ============================================================


# ----- USER CONFIGURATION -----------------------------------

obs_file   <- "R1_30.obs"      # path to the .obs file
model_file <- "r1_dates.dat"   # path to the dated model output file
                               # (produced by add_datetime_to_results.R)

# Which column in the dated model file contains modelled SMD?
# The dated file has columns: 1=Date, 2=Time, 3=first model col, ...
# e.g. if SMD is the 1st original model column, set smd_col <- 3
smd_col <- 3

output_file <- "r1_30_match.dat"  # leave "" to auto-generate name

# ------------------------------------------------------------


# ---- Helper ----
bail <- function(...) stop(paste0(...), call. = FALSE)


# ---- 1. Read the .obs file ----
if (!file.exists(obs_file))
  bail("Cannot find .obs file: ", obs_file)

obs_lines <- readLines(obs_file, warn = FALSE)

# Confirm this is an SMD obs file by checking the header
if (length(obs_lines) < 3)
  bail(".obs file has fewer than 3 lines: ", obs_file)

header_block <- paste(obs_lines[1:2], collapse = "\n")
if (!grepl("SMD", header_block, ignore.case = TRUE))
  bail("This .obs file does not appear to contain SMD data.\n",
       "  Header found:\n  ", obs_lines[1], "\n  ", obs_lines[2])

# Parse data lines (everything after the 2-line header)
data_lines <- obs_lines[-(1:2)]

# Drop blank lines
data_lines <- data_lines[nzchar(trimws(data_lines))]

if (length(data_lines) == 0)
  bail("No data rows found in .obs file after the header: ", obs_file)

# Split each line on whitespace / tabs
obs_parsed <- strsplit(trimws(data_lines), "[ \t]+")

# Keep only lines that have at least 3 fields
valid <- sapply(obs_parsed, length) >= 3
if (!any(valid))
  bail("Could not parse any data rows from .obs file: ", obs_file)

obs_parsed <- obs_parsed[valid]

obs_df <- data.frame(
  Date    = sapply(obs_parsed, `[[`, 1),
  Time    = sapply(obs_parsed, `[[`, 2),
  SMD_obs = suppressWarnings(as.numeric(sapply(obs_parsed, `[[`, 3))),
  stringsAsFactors = FALSE
)

# Drop rows where the value could not be parsed
n_before <- nrow(obs_df)
obs_df   <- obs_df[!is.na(obs_df$SMD_obs), ]
n_after  <- nrow(obs_df)

if (n_after == 0)
  bail("No numeric SMD values could be read from: ", obs_file)

if (n_after < n_before)
  message(sprintf("Note: %d obs row(s) skipped (non-numeric value).",
                  n_before - n_after))

cat(sprintf("Observed rows read  : %d\n", n_after))


# ---- 2. Read the dated model output file ----
if (!file.exists(model_file))
  bail("Cannot find model file: ", model_file)

mod_df <- tryCatch(
  read.table(model_file, header = FALSE, sep = "\t",
             fill = TRUE, stringsAsFactors = FALSE),
  error = function(e) bail("Could not read model file: ", e$message)
)

if (nrow(mod_df) == 0)
  bail("Model file contains no rows: ", model_file)

n_cols <- ncol(mod_df)
cat(sprintf("Model rows read     : %d  (%d columns)\n", nrow(mod_df), n_cols))

# Validate the requested SMD column
if (smd_col < 3 || smd_col > n_cols)
  bail(sprintf(
    "smd_col = %d is out of range. The model file has %d columns (1=Date, 2=Time, 3..%d=model data).",
    smd_col, n_cols, n_cols))

# Rename the columns we care about
names(mod_df)[1] <- "Date"
names(mod_df)[2] <- "Time"
names(mod_df)[smd_col] <- "SMD_mod"

mod_df$SMD_mod <- suppressWarnings(as.numeric(mod_df$SMD_mod))

mod_df <- mod_df[, c("Date", "Time", "SMD_mod")]
mod_df <- mod_df[!is.na(mod_df$SMD_mod), ]

if (nrow(mod_df) == 0)
  bail("No numeric modelled SMD values found in column ", smd_col,
       " of: ", model_file)


# ---- 3. Match on (Date, Time) ----
# Build a single key for merging to avoid any whitespace ambiguity
obs_df$key <- paste(trimws(obs_df$Date), trimws(obs_df$Time))
mod_df$key <- paste(trimws(mod_df$Date), trimws(mod_df$Time))

merged_df <- merge(
  obs_df[, c("key", "Date", "Time", "SMD_obs")],
  mod_df[, c("key", "SMD_mod")],
  by   = "key",
  all  = FALSE    # inner join — only rows present in BOTH files
)

if (nrow(merged_df) == 0) {
  cat("\nWARNING: No matching (Date, Time) pairs found between the two files.\n")
  cat("  First few observed timestamps:\n")
  print(head(obs_df[, c("Date","Time")], 5))
  cat("  First few modelled timestamps:\n")
  print(head(mod_df[, c("Date","Time")], 5))
  stop("No matched rows — output file not written.", call. = FALSE)
}

# Sort chronologically and drop the key column
merged_df <- merged_df[order(merged_df$key), ]
output_df <- merged_df[, c("Date", "Time", "SMD_obs", "SMD_mod")]

cat(sprintf("Matched rows        : %d\n", nrow(output_df)))


# ---- 4. Write the output file ----
if (output_file == "") {
  base        <- tools::file_path_sans_ext(obs_file)
  output_file <- paste0(base, "_matched.txt")
}

write.table(
  output_df,
  file      = output_file,
  sep       = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote     = FALSE
)

cat(sprintf("Output written      : %s\n", output_file))
