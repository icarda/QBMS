# =============================================================================
# QBMS Connection Wizard - Console Usage Example
# =============================================================================
#
# This example demonstrates how to use the QBMS Connection Wizard directly
# from the R console or an R script. The wizard opens in the RStudio Viewer
# pane (if available) or in the default web browser.
#
# After completing the wizard, the result is returned to your R session so
# you can immediately use QBMS data retrieval functions.
#
# NOTE: This approach uses qbms_wizard() which calls runGadget(). It is meant
# for console/script usage ONLY. For Shiny apps or R Markdown, use the module
# interface (qbms_wizard_ui / qbms_wizard_server) instead.
# =============================================================================

# remotes::install_github("icarda/QBMS")
# library(QBMS)
devtools::load_all(".")

# Launch the wizard - it opens interactively and blocks until Done or Cancel
result <- qbms_wizard()

# Check if the user completed the wizard (NULL means cancelled)
if (!is.null(result)) {
  cat("Connection configured successfully!\n")
  cat("Engine:", result$engine, "\n")
  cat("URL:", result$url, "\n")
  cat("Selections:\n")
  print(result$selections)

  # The QBMS session is now configured - you can use data retrieval functions
  # For example (depending on engine):
  # data <- get_germplasm_data()
  # data <- get_trial_data()
} else {
  cat("Wizard was cancelled. No changes were made.\n")
}

data <- get_study_data()
info <- get_study_info()
germplasm <- get_germplasm_list()
