# Build outputs/export_for_partners from FAOSTAT and USDA PSD.
# Run from the repository root. Extra arguments are forwarded to the Python
# exporter, including --production, --trade, --psd, and --no-net-reexports.
#
#   Rscript main/export_inputs.R
#   Rscript main/export_inputs.R --production /path/to/new_production.csv --trade /path/to/new_trade.csv

args <- commandArgs(trailingOnly = TRUE)
py <- Sys.which("python3")
if (!nzchar(py)) stop("python3 was not found on PATH")
script <- "main/export_inputs.py"
if (!file.exists(script)) {
    stop("Run this from the repository root so ", script, " is visible")
}
status <- system2(py, c(script, args))
if (!is.null(status) && status != 0) {
    stop("export_inputs.py exited with status ", status)
}
