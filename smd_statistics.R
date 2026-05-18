## ============================================================
## smd_statistics.R
##
## Reads the matched SMD file produced by match_smd.R and
## calculates three goodness-of-fit statistics between the
## observed and modelled SMD columns:
##
##   (i)  R²   – Coefficient of Determination
##   (ii) NS   – Nash–Sutcliffe Efficiency
##   (iii) KGE – Kling–Gupta Efficiency
##
## No external packages are required.
## ============================================================


# ----- USER CONFIGURATION -----------------------------------

matched_file <- "R1_30_match.dat"   # Path to the matched file from match_smd.R
                     # (leave "" to be prompted, or set explicitly,
                     #  e.g. "R1_30_matched.txt")

output_file  <- ""   # Path for the statistics output file.
                     # Leave "" to auto-generate from the input filename,
                     # e.g. "R1_30_matched_statistics.txt"

# ------------------------------------------------------------


# ---- Helper ----
bail <- function(...) stop(paste0(...), call. = FALSE)


# ---- 0. Resolve input file ----
if (matched_file == "") {
  if (interactive()) {
    matched_file <- readline(
      "Enter path to matched SMD file (from match_smd.R): ")
    matched_file <- trimws(matched_file)
  }
  if (matched_file == "")
    bail("No input file specified. Set matched_file at the top of the script.")
}

if (!file.exists(matched_file))
  bail("Cannot find file: ", matched_file)


# ---- 1. Read the matched file ----
df <- tryCatch(
  read.table(matched_file, header = TRUE, sep = "\t",
             stringsAsFactors = FALSE, fill = TRUE),
  error = function(e) bail("Could not read matched file: ", e$message)
)

required_cols <- c("SMD_obs", "SMD_mod")
missing_cols  <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0)
  bail("Required column(s) not found in ", matched_file, ": ",
       paste(missing_cols, collapse = ", "),
       "\n  Columns present: ", paste(names(df), collapse = ", "))

obs <- suppressWarnings(as.numeric(df$SMD_obs))
mod <- suppressWarnings(as.numeric(df$SMD_mod))

# Drop rows where either value is NA
valid <- !is.na(obs) & !is.na(mod)
n_dropped <- sum(!valid)
obs <- obs[valid]
mod <- mod[valid]
n   <- length(obs)

if (n < 2)
  bail("Need at least 2 valid paired values to compute statistics. ",
       "Only ", n, " valid row(s) found.")

if (n_dropped > 0)
  message(sprintf("Note: %d row(s) with NA values excluded from statistics.",
                  n_dropped))

cat(sprintf("\nInput file          : %s\n", matched_file))
cat(sprintf("Valid paired rows   : %d\n\n", n))


# ---- 2. Compute statistics ----

## (i) R² – square of the Pearson correlation coefficient
r_squared <- cor(obs, mod)^2

## (ii) NS – Nash–Sutcliffe Efficiency
##   NS = 1 - [ Σ(obs - mod)² / Σ(obs - mean(obs))² ]
ns <- 1 - ( sum((obs - mod)^2) / sum((obs - mean(obs))^2) )

## (iii) KGE – Kling–Gupta Efficiency (Gupta et al., 2009)
##   KGE = 1 - sqrt( (r-1)² + (α-1)² + (β-1)² )
##   where:
##     r = Pearson correlation between obs and mod
##     α = sd(mod) / sd(obs)         (variability ratio)
##     β = mean(mod) / mean(obs)     (bias ratio)
r_kge  <- cor(obs, mod)
alpha  <- sd(mod)  / sd(obs)
beta   <- mean(mod) / mean(obs)
kge    <- 1 - sqrt((r_kge - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)


# ---- 3. Resolve output file path ----
if (output_file == "") {
  base        <- tools::file_path_sans_ext(matched_file)
  output_file <- paste0(base, "_statistics.txt")
}


# ---- 4. Build output lines ----
lines <- c(
  "==============================================",
  "  Goodness-of-Fit Statistics: SMD",
  "==============================================",
  sprintf("  Input file                    : %s",   matched_file),
  sprintf("  Valid paired rows             : %d",   n),
  "----------------------------------------------",
  sprintf("  R²  (Coeff. of Determination) : %8.4f", r_squared),
  sprintf("  NS  (Nash-Sutcliffe Eff.)     : %8.4f", ns),
  sprintf("  KGE (Kling-Gupta Eff.)        : %8.4f", kge),
  "----------------------------------------------",
  "  KGE components:",
  sprintf("    r  (correlation)             : %8.4f", r_kge),
  sprintf("    alpha (variability ratio)    : %8.4f", alpha),
  sprintf("    beta  (bias ratio)           : %8.4f", beta),
  "==============================================",
  "",
  "Interpretation guide:",
  "  R2  : 1.0 = perfect fit.  Values closer to 1 are better.",
  "  NS  : 1.0 = perfect fit.  0 = model no better than obs mean.",
  "        Negative values indicate poor performance.",
  "  KGE : 1.0 = perfect fit.  Values > -0.41 outperform the mean",
  "        flow benchmark (Knoben et al., 2019)."
)


# ---- 5. Print to console ----
cat(paste(lines, collapse = "\n"), "\n\n")


# ---- 6. Write to file ----
writeLines(lines, con = output_file)
cat(sprintf("Statistics written  : %s\n", output_file))
