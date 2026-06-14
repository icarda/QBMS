# =============================================================================
# QBMS Connection Wizard - Shiny Application Example
# =============================================================================
#
# This example demonstrates how to embed the QBMS Connection Wizard within
# a Shiny application using the module interface (qbms_wizard_ui / qbms_wizard_server).
#
# NOTE: You cannot use qbms_wizard() inside a Shiny app because it calls
# runGadget() which cannot nest inside a running Shiny session. Use the module
# interface instead.
# =============================================================================

# remotes::install_github("icarda/QBMS")
# library(QBMS)
devtools::load_all(".")

library(shiny)

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("QBMS Data Explorer"),

  sidebarLayout(
    sidebarPanel(
      # Embed the wizard directly as a module
      qbms_wizard_ui("my_wizard")
    ),

    mainPanel(
      h4("Connection Result"),
      verbatimTextOutput("result_display"),
      hr(),
      h4("Data Preview"),
      tableOutput("data_table")
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================
server <- function(input, output, session) {

  # Activate the wizard module - returns a reactive with the result

  wizard_result <- qbms_wizard_server("my_wizard")

  # Display the result once the wizard completes
  output$result_display <- renderPrint({
    result <- wizard_result()
    if (is.null(result)) {
      cat("Wizard in progress... complete all steps to see the result.")
    } else {
      cat("Connection configured!\n\n")
      cat("Engine:", result$engine, "\n")
      cat("URL:", result$url, "\n")
      cat("\nSelections:\n")
      print(result$selections)
    }
  })

  # Fetch and display data after connection is configured
  output$data_table <- renderTable({
    result <- wizard_result()
    req(result)  # Only proceed if wizard is complete

    # The QBMS session is now fully configured - call data retrieval functions
    tryCatch(
      {
        # Example: get_germplasm_data()
        data.frame(
          Message = "Connection configured! Replace this with actual QBMS data retrieval.",
          Engine = result$engine,
          URL = result$url
        )
      },
      error = function(e) {
        data.frame(Error = conditionMessage(e))
      }
    )
  })
}

# ==============================================================================
# Run the app
# ==============================================================================
shinyApp(ui = ui, server = server)
