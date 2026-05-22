# run_persist.R
#
# Runs: persist_cmd.exe -par smd.par -dat dat.dat -inca smd -size none
#
# persist_cmd.exe and all input files are expected to live in the same
# directory as this R script. No working directory argument is needed.
#
# Requires: processx  (install.packages("processx"))

run_persist <- function(
    par_file     = "smd.par",
    dat_file     = "dat.dat",
    inca_stub    = "smd",
    size         = "none",
    timeout_secs = 300
) {

  # Resolve the directory this script lives in.
  # Works when sourced; falls back to R's current working directory
  # if run interactively line-by-line.
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile)),
    error = function(e) getwd()
  )

  executable <- file.path(script_dir, "persist_cmd.exe")

  if (!file.exists(executable)) {
    stop("Cannot find persist_cmd.exe in: ", script_dir)
  }

  args <- c(
    "-par",  par_file,
    "-dat",  dat_file,
    "-inca", inca_stub,
    "-size", size
  )

  result <- processx::run(
    command         = executable,
    args            = args,
    wd              = script_dir,
    stdout          = "|",
    stderr          = "|",
    timeout         = timeout_secs,
    error_on_status = FALSE
  )

  # Any stdout output means something went wrong (PERSiST is silent on success)
  if (nchar(trimws(result$stdout)) > 0) {
    error_log <- file.path(script_dir, "error.lst")
    writeLines(result$stdout, error_log)
    stop(sprintf("PERSiST reported an error (exit %d). See: %s",
                 result$status, error_log))
  }

  if (result$status != 0) {
    stop(sprintf("PERSiST exited with status %d: %s",
                 result$status, trimws(result$stderr)))
  }

  invisible(result)
}


# ── Call ──────────────────────────────────────────────────────────────────────

run_persist()
