#' Engine Workflow Step Definitions
#'
#' Maps engine types to their ordered navigation step definitions. Each step
#' definition contains: id, label, list_fn (QBMS function name to fetch options),
#' set_fn (QBMS function name to set selection), and selection_key (key name in
#' the result selections list).
#'
#' Engines using each workflow:
#' - standard: bms, breedbase, germinate (crop/program/trial/study hierarchy)
#' - ebs: ebs (no crop step)
#' - gigwa: gigwa (database/project/run hierarchy)
#'

engine_workflows <- list(
  standard = list(
    list(id = "crop", label = "Select Crop", list_fn = "list_crops", set_fn = "set_crop", selection_key = "crop"),
    list(id = "program", label = "Select Program", list_fn = "list_programs", set_fn = "set_program", selection_key = "program"),
    list(id = "trial", label = "Select Trial", list_fn = "list_trials", set_fn = "set_trial", selection_key = "trial"),
    list(id = "study", label = "Select Study", list_fn = "list_studies", set_fn = "set_study", selection_key = "study")
  ),
  ebs = list(
    list(id = "program", label = "Select Program", list_fn = "list_programs", set_fn = "set_program", selection_key = "program"),
    list(id = "trial", label = "Select Trial", list_fn = "list_trials", set_fn = "set_trial", selection_key = "trial"),
    list(id = "study", label = "Select Study", list_fn = "list_studies", set_fn = "set_study", selection_key = "study")
  ),
  gigwa = list(
    list(id = "database", label = "Select Database", list_fn = "gigwa_list_dbs", set_fn = "gigwa_set_db", selection_key = "database"),
    list(id = "project", label = "Select Project", list_fn = "gigwa_list_projects", set_fn = "gigwa_set_project", selection_key = "project"),
    list(id = "run", label = "Select Run", list_fn = "gigwa_list_runs", set_fn = "gigwa_set_run", selection_key = "run")
  )
)


#' Normalize QBMS List Function Results
#'
#' Converts heterogeneous return types from QBMS list functions into a clean
#' character vector suitable for dropdown options. Handles character vectors,
#' single-column data frames, multi-column data frames, NULL/empty inputs,
#' and removes NA values.
#'
#' @param result The return value from a QBMS list function. Can be a character
#'   vector, a data.frame (single or multi-column), NULL, or an empty object.
#'
#' @return A character vector with no NA values. Returns \code{character(0)} if
#'   the input is NULL or has length 0.
#'
#' @examples
#' if (interactive()) {
#'   normalize_options(c("wheat", "rice", NA, "maize"))
#'   # [1] "wheat" "rice"  "maize"
#'
#'   normalize_options(data.frame(crop = c("wheat", "rice")))
#'   # [1] "wheat" "rice"
#'
#'   normalize_options(NULL)
#'   # character(0)
#' }

normalize_options <- function(result) {
  # Handle NULL or length-0 inputs

  if (is.null(result) || length(result) == 0) {
    return(character(0))
  }

  # Extract values based on type
  if (is.data.frame(result)) {
    # For data frames (single or multi-column), use the first column
    values <- result[[1]]
  } else {
    # Character vectors (or other atomic vectors) used as-is
    values <- result
  }

  # Coerce to character
  values <- as.character(values)

  # Remove NA values
  values <- values[!is.na(values)]

  return(values)
}


#' Navigation Step UI Factory
#'
#' Produces the UI elements for a single navigation step in the wizard. Renders
#' a selectInput populated with the provided choices and Back/Next action buttons.
#' If choices is empty, displays an informational message and only the Back button.
#' The function can be used either standalone or within a Shiny module by supplying 
#' a namespace function.
#'
#' @param step_def A list with fields: id, label, list_fn, set_fn, selection_key.
#'   Used to generate unique input IDs and the dropdown label.
#' @param choices A character vector of options to display in the selectInput.
#'   Should already be normalized (e.g., via \code{normalize_options()}).
#' @param ns Optional Shiny namespace function created with \code{shiny::NS()}.
#'   Defaults to \code{base::identity}, allowing the UI to be used outside of a module.
#'
#' @return A \code{shiny::tagList} containing either a selectInput with Back/Next
#'   buttons, or an informational message with only a Back button when choices
#'   is empty.
#'
#' @examples
#' if (interactive()) {
#'   step <- list(id = "crop", label = "Select Crop",
#'                list_fn = "list_crops", set_fn = "set_crop",
#'                selection_key = "crop")
#'   navigation_step_ui(step, c("wheat", "rice", "maize"))
#'
#'   # Empty choices case
#'   navigation_step_ui(step, character(0))
#' }

navigation_step_ui <- function(step_def, choices, ns = NULL) {
  if (is.null(ns)) { ns <- function(id) id }
  
  if (length(choices) == 0) {
    # Empty choices: show info message and Back button only
    shiny::tagList(
      shiny::tags$div(class = "alert alert-info", "No items available at this level."),
      shiny::tags$div(shiny::actionButton(ns(paste0("nav_back_", step_def$id)), "Back", class = "btn-default"))
    )
  } else {
    # Normal case: selectInput with Back and Next buttons
    shiny::tagList(
      shiny::selectInput(inputId = ns(paste0("nav_select_", step_def$id)), label = step_def$label, choices = choices, selected = NULL),
      shiny::tags$div(
        shiny::actionButton(ns(paste0("nav_back_", step_def$id)), "Back", class = "btn-default"),
        shiny::actionButton(ns(paste0("nav_next_", step_def$id)), "Next", class = "btn-primary")
      )
    )
  }
}


#' Get Engine-Specific Configuration Parameters
#'
#' Returns engine-specific parameters to be passed to \code{set_qbms_config()}
#' in addition to the standard url, engine, and no_auth parameters.
#'
#' @param engine Character string specifying the engine type. One of:
#'   "bms", "breedbase", "ebs", "gigwa", "germinate".
#'
#' @return A named list of additional configuration parameters:
#'   \itemize{
#'     \item For "ebs": \code{list(brapi_ver = "v2")}
#'     \item For "gigwa": \code{list(time_out = 300)}
#'     \item For "germinate": \code{list(page_size = 9999)}
#'     \item For all others ("bms", "breedbase"): \code{list()} (empty, use defaults)
#'   }
#'
#' @examples
#' if (interactive()) {
#'   get_engine_config_params("ebs")
#'   # list(brapi_ver = "v2")
#'
#'   get_engine_config_params("bms")
#'   # list()
#' }

get_engine_config_params <- function(engine) {
  switch(engine,
    "ebs" = list(brapi_ver = "v2"),
    "gigwa" = list(time_out = 300),
    "germinate" = list(page_size = 9999),
    list()
  )
}

#' Backup Current QBMS Connection State
#'
#' Captures the current QBMS connection state for later restoration. This is
#' called when the wizard starts so that the prior state can be restored if the
#' user cancels.
#'
#' @return The current QBMS connection state object (a list), or NULL if no
#'   prior connection state exists or if retrieval fails.
#'
#' @keywords internal

backup_connection_state <- function() {
  tryCatch(
    get_qbms_connection(),
    error = function(e) NULL
  )
}


#' Restore QBMS Connection State
#'
#' Restores a previously backed-up QBMS connection state. This is called when
#' the user cancels the wizard to undo any partial configuration changes made
#' during the wizard session.
#'
#' @param backup The connection state object previously returned by
#'   \code{backup_connection_state()}. If NULL, no restoration is performed.
#'
#' @return Invisible NULL.
#'
#' @keywords internal

restore_connection_state <- function(backup) {
  if (!is.null(backup)) {
    set_qbms_connection(backup)
  }
  invisible(NULL)
}


#' Get Engine-to-Workflow Mapping
#'
#' Returns the workflow key for a given engine name.
#'
#' @param engine Character string: one of "bms", "breedbase", "ebs", "gigwa", "germinate".
#'
#' @return Character string: "standard", "ebs", or "gigwa".
#'
#' @keywords internal

get_workflow_key <- function(engine) {
  switch(engine,
    "bms" = "standard",
    "breedbase" = "standard",
    "germinate" = "standard",
    "ebs" = "ebs",
    "gigwa" = "gigwa",
    stop("Unknown engine: ", engine)
  )
}


#' Get Authentication Step Type
#'
#' Determines the type of authentication step required for a given engine and
#' no_auth combination.
#'
#' @param engine Character string: one of "bms", "breedbase", "ebs", "gigwa", "germinate".
#' @param no_auth Logical. If TRUE, authentication is skipped.
#'
#' @return Character string: "none" if no_auth is TRUE, "token" if engine is
#'   "ebs" and no_auth is FALSE, or "credentials" for all other engines when
#'   no_auth is FALSE.
#'
#' @examples
#' if (interactive()) {
#'   get_auth_step_type("bms", FALSE)
#'   # [1] "credentials"
#'
#'   get_auth_step_type("ebs", FALSE)
#'   # [1] "token"
#'
#'   get_auth_step_type("bms", TRUE)
#'   # [1] "none"
#' }

get_auth_step_type <- function(engine, no_auth) {
  if (isTRUE(no_auth)) {
    return("none")
  }

  if (engine == "ebs") {
    return("token")
  }

  return("credentials")
}


#' Get Full Step Sequence
#'
#' Computes the complete ordered list of wizard steps for a given engine and
#' no_auth combination. The sequence always starts with "config", optionally
#' includes an authentication step ("auth" or "auth_token"), followed by the
#' engine-specific navigation steps, and ends with "done".
#'
#' @param engine Character string: one of "bms", "breedbase", "ebs", "gigwa", "germinate".
#' @param no_auth Logical. If TRUE, the authentication step is omitted.
#'
#' @return Character vector of step IDs in order.
#'
#' @examples
#' if (interactive()) {
#'   get_step_sequence("bms", FALSE)
#'   # [1] "config" "auth" "crop" "program" "trial" "study" "done"
#'
#'   get_step_sequence("bms", TRUE)
#'   # [1] "config" "crop" "program" "trial" "study" "done"
#'
#'   get_step_sequence("ebs", FALSE)
#'   # [1] "config" "auth_token" "program" "trial" "study" "done"
#'
#'   get_step_sequence("gigwa", FALSE)
#'   # [1] "config" "auth" "database" "project" "run" "done"
#' }

get_step_sequence <- function(engine, no_auth) {
  # Start with config
  steps <- "config"

  # Add auth step if required
  auth_type <- get_auth_step_type(engine, no_auth)
  if (auth_type == "token") {
    steps <- c(steps, "auth_token")
  } else if (auth_type == "credentials") {
    steps <- c(steps, "auth")
  }

  # Get navigation steps from the engine workflow
  workflow_key <- get_workflow_key(engine)
  workflow_steps <- engine_workflows[[workflow_key]]
  nav_step_ids <- vapply(workflow_steps, function(s) s$id, character(1))
  steps <- c(steps, nav_step_ids)

  # End with done
  steps <- c(steps, "done")

  return(steps)
}


#' Get Next Step
#'
#' Returns the next step in the wizard sequence given the current step, engine,
#' and no_auth setting.
#'
#' @param current_step Character string: the ID of the current step.
#' @param engine Character string: one of "bms", "breedbase", "ebs", "gigwa", "germinate".
#' @param no_auth Logical. If TRUE, authentication is skipped.
#'
#' @return Character string of the next step ID, or NULL if the current step
#'   is the last step ("done").
#'
#' @examples
#' if (interactive()) {
#'   get_next_step("config", "bms", FALSE)
#'   # [1] "auth"
#'
#'   get_next_step("auth", "bms", FALSE)
#'   # [1] "crop"
#'
#'   get_next_step("done", "bms", FALSE)
#'   # NULL
#' }

get_next_step <- function(current_step, engine, no_auth) {
  steps <- get_step_sequence(engine, no_auth)
  idx <- match(current_step, steps)

  if (is.na(idx) || idx >= length(steps)) {
    return(NULL)
  }

  return(steps[idx + 1])
}


#' Get Previous Step
#'
#' Returns the previous step in the wizard sequence given the current step,
#' engine, and no_auth setting.
#'
#' @param current_step Character string: the ID of the current step.
#' @param engine Character string: one of "bms", "breedbase", "ebs", "gigwa", "germinate".
#' @param no_auth Logical. If TRUE, authentication is skipped.
#'
#' @return Character string of the previous step ID, or NULL if the current step
#'   is the first step ("config").
#'
#' @examples
#' if (interactive()) {
#'   get_prev_step("auth", "bms", FALSE)
#'   # [1] "config"
#'
#'   get_prev_step("config", "bms", FALSE)
#'   # NULL
#'
#'   get_prev_step("crop", "bms", TRUE)
#'   # [1] "config"
#' }

get_prev_step <- function(current_step, engine, no_auth) {

  steps <- get_step_sequence(engine, no_auth)
  idx <- match(current_step, steps)

  if (is.na(idx) || idx <= 1) {
    return(NULL)
  }

  return(steps[idx - 1])
}


#' Credentials Authentication UI
#'
#' Returns a tagList containing username and password inputs with a submit button
#' for engines that use credential-based authentication. The function can be used 
#' either standalone or within a Shiny module by supplying a namespace function.
#'
#' @param ns Optional Shiny namespace function created with \code{shiny::NS()}.
#'   Defaults to \code{base::identity}, allowing the UI to be used outside of a module.
#'
#' @return A \code{shiny::tagList} with textInput for username, passwordInput for
#'   password, and an actionButton to submit.
#'
#' @keywords internal

auth_credentials_ui <- function(ns = NULL) {
  if (is.null(ns)) { ns <- function(id) id }
  
  shiny::tagList(
    shiny::textInput(ns("auth_username"), "Username", value = ""),
    shiny::passwordInput(ns("auth_password"), "Password", value = ""),
    shiny::tags$div(
      shiny::actionButton(ns("auth_back"), "Back", class = "btn-default"),
      shiny::actionButton(ns("auth_submit"), "Login", class = "btn-primary")
    )
  )
}


#' Token Authentication UI
#'
#' Returns a tagList containing a token input field and a submit button for
#' engines that use token-based authentication (e.g., EBS). The function can be 
#' used either standalone or within a Shiny module by supplying a namespace function.
#'
#' @param ns Optional Shiny namespace function created with \code{shiny::NS()}.
#'   Defaults to \code{base::identity}, allowing the UI to be used outside of a module.
#'
#' @return A \code{shiny::tagList} with textInput for token and an actionButton to submit.
#'
#' @keywords internal

auth_token_ui <- function(ns = NULL) {
  if (is.null(ns)) { ns <- function(id) id }
  
  shiny::tagList(
    shiny::textInput(ns("auth_token"), "API Token", value = ""),
    shiny::tags$div(
      shiny::actionButton(ns("auth_token_back"), "Back", class = "btn-default"),
      shiny::actionButton(ns("auth_token_submit"), "Set Token", class = "btn-primary")
    )
  )
}


#' Authentication Step UI Dispatcher
#'
#' Returns the appropriate authentication UI based on the engine and no_auth
#' setting. Uses \code{get_auth_step_type()} to determine whether credentials,
#' token, or no authentication UI should be rendered.
#'
#' @param engine Character string: one of "bms", "breedbase", "ebs", "gigwa", "germinate".
#' @param no_auth Logical. If TRUE, no authentication UI is rendered.
#'
#' @return A \code{shiny::tagList} with the appropriate auth inputs, or NULL if
#'   no authentication is required.
#'
#' @examples
#' if (interactive()) {
#'   auth_step_ui("bms", FALSE)
#'   # Returns credentials UI (username + password + submit)
#'
#'   auth_step_ui("ebs", FALSE)
#'   # Returns token UI (token + submit)
#'
#'   auth_step_ui("bms", TRUE)
#'   # Returns NULL
#' }
#' 
#' @keywords internal

auth_step_ui <- function(engine, no_auth) {
  auth_type <- get_auth_step_type(engine, no_auth)

  switch(auth_type,
    "none" = NULL,
    "token" = auth_token_ui(),
    "credentials" = auth_credentials_ui()
  )
}


#' Gadget UI Definition
#'
#' Creates the main miniUI page layout for the Connection Wizard gadget.
#' Uses \code{miniUI::miniPage()} with a title bar (Done/Cancel), and a content panel
#' containing dynamic step rendering and error display areas.
#'
#' @return A \code{miniUI::miniPage} UI definition suitable for use with \code{shiny::runGadget()}.
#'
#' @keywords internal

gadget_ui <- miniUI::miniPage(
  miniUI::gadgetTitleBar("QBMS Connection Wizard"),
  miniUI::miniContentPanel(
    padding = 10,
    shiny::uiOutput("step_content"),
    shiny::uiOutput("step_error")
  )
)


#' Configuration Step UI
#'
#' Returns the UI elements for the server configuration step of the wizard.
#' Includes inputs for the server URL, engine selection, no_auth toggle, and
#' a submit button. The function can be used either standalone or within a
#' Shiny module by supplying a namespace function.
#'
#' @param ns Optional Shiny namespace function created with \code{shiny::NS()}.
#'   Defaults to \code{base::identity}, allowing the UI to be used outside of a module.
#'
#' @return A \code{shiny::tagList} containing the configuration step UI elements.
#'
#' @keywords internal

config_step_ui <- function(ns = NULL) {
  if (is.null(ns)) { ns <- function(id) id }
  
  shiny::tagList(
    shiny::textInput(inputId = ns("config_url"), label = "Server URL", value = ""),
    shiny::selectInput(inputId = ns("config_engine"), label = "Engine", choices = c("bms", "breedbase", "ebs", "gigwa", "germinate"), selected = "bms"),
    shiny::checkboxInput(inputId = ns("config_no_auth"), label = "Skip Authentication", value = FALSE),
    shiny::actionButton(inputId = ns("config_submit"), label = "Connect", class = "btn-primary btn-block")
  )
}


#' Completion Step UI
#'
#' Returns the UI for the wizard's completion step, showing a success message
#' and a summary of all selections made during the wizard session. The "Done"
#' button in the gadgetTitleBar returns the result list to the caller.
#'
#' @param selections A named list of navigation selections made by the user
#'   (e.g., \code{list(crop = "wheat", program = "ICARDA", trial = "IDYT39",
#'   study = "Site A")}).
#' @param engine Character string: the selected engine name.
#' @param url Character string: the configured server URL.
#'
#' @return A \code{shiny::tagList} containing:
#'   \itemize{
#'     \item A success header with a check-circle icon
#'     \item A success message instructing the user to click Done
#'     \item A summary panel with dt/dd pairs for engine, url, and each selection
#'   }
#'
#' @examples
#' if (interactive()) {
#'   completion_step_ui(
#'     selections = list(crop = "wheat", program = "ICARDA", trial = "IDYT39", study = "Site A"),
#'     engine = "bms",
#'     url = "https://example.org/bms"
#'   )
#' }
#' 
#' @keywords internal

completion_step_ui <- function(selections, engine, url, ns = NULL) {
  # Build dt/dd pairs for the summary
  summary_items <- shiny::tagList(
    shiny::tags$dt("Engine"),
    shiny::tags$dd(engine),
    shiny::tags$dt("URL"),
    shiny::tags$dd(url)
  )

  # Dynamically add each selection key/value pair
  for (key in names(selections)) {
    summary_items <- shiny::tagList(
      summary_items,
      shiny::tags$dt(key),
      shiny::tags$dd(selections[[key]])
    )
  }

  shiny::tagList(
    shiny::tags$h4(shiny::icon("check-circle"), " Connection Configured"),
    shiny::tags$p(paste("Your QBMS session is now configured.", ifelse(is.null(ns), "", "Click 'Done' to close the wizard."))),
    shiny::tags$h5("Summary"),
    shiny::tags$dl(summary_items),
    if(is.null(ns)) shiny::tags$p(class = "text-muted", "Use the 'Done' button in the title bar to return results.")
  )
}


#' Wizard Server Function
#'
#' The server-side logic for the QBMS Connection Wizard Shiny Gadget/Module. 
#' Manages reactive wizard state, renders the correct step UI based on the 
#' current step, and provides placeholder observers for configuration, 
#' authentication, navigation, and cancellation logic.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param as_module Logical. Indicates whether the server is being used as a
#'   Shiny module (TRUE) or as a standalone gadget (FALSE).
#'
#' @keywords internal

wizard_server <- function(input, output, session, as_module = FALSE) {
  
  if (as_module) { used_ns = session$ns } else { used_ns = NULL }
  
  # Backup connection state at startup for restore on cancel
  prior_connection <- backup_connection_state()
  
  # --------------------------------------------------------------------------
  # Reactive State
  # --------------------------------------------------------------------------
  
  wizard_state <- shiny::reactiveValues(
    current_step = "config",
    engine = "bms",
    no_auth = FALSE,
    selections = list(),
    error_msg = NULL,
    url = "",
    completed = FALSE,
    result = NULL
  )
  
  # --------------------------------------------------------------------------
  # Step Rendering - Only one step visible at a time
  # --------------------------------------------------------------------------
  
  output$step_content <- shiny::renderUI({
    step <- wizard_state$current_step
    
    # Clear error when step changes
    wizard_state$error_msg <- NULL
    
    if (step == "config") {
      config_step_ui(used_ns)
    } else if (step == "auth") {
      auth_credentials_ui(used_ns)
    } else if (step == "auth_token") {
      auth_token_ui(used_ns)
    } else if (step == "done") {
      completion_step_ui(selections = wizard_state$selections, engine = wizard_state$engine, url = wizard_state$url, ns = used_ns)
    } else {
      # Navigation step - find the step definition and render
      workflow_key <- get_workflow_key(wizard_state$engine)
      workflow_steps <- engine_workflows[[workflow_key]]
      step_def <- NULL
      for (s in workflow_steps) {
        if (s$id == step) {
          step_def <- s
          break
        }
      }
      
      if (!is.null(step_def)) {
        # Fetch available options for this navigation step
        choices <- tryCatch(
          {
            result <- do.call(step_def$list_fn, list())
            normalize_options(result)
          },
          error = function(e) {
            wizard_state$error_msg <- conditionMessage(e)
            character(0)
          }
        )
        navigation_step_ui(step_def, choices, used_ns)
      } else {
        # Fallback - should not occur in normal operation
        shiny::tags$div(class = "alert alert-warning", paste("Unknown step:", step))
      }
    }
  })
  
  # --------------------------------------------------------------------------
  # Error Display
  # --------------------------------------------------------------------------
  
  output$step_error <- shiny::renderUI({
    if (!is.null(wizard_state$error_msg)) {
      shiny::tags$div(class = "alert alert-danger", wizard_state$error_msg)
    }
  })
  
  # --------------------------------------------------------------------------
  # Configuration Step Event Observers
  # --------------------------------------------------------------------------
  
  shiny::observeEvent(input$config_submit, {
    # Get input values
    url <- trimws(input$config_url)
    engine <- input$config_engine
    no_auth <- input$config_no_auth
    
    # Validate URL is non-empty
    if (nchar(url) == 0) {
      wizard_state$error_msg <- "Server URL is required."
      return()
    }
    
    # Get engine-specific config parameters
    extra_params <- get_engine_config_params(engine)
    
    # Build the config call arguments
    config_args <- c(
      list(url = url, engine = engine, no_auth = no_auth),
      extra_params
    )
    
    # Call set_qbms_config with tryCatch
    result <- tryCatch(
      { do.call(set_qbms_config, config_args); TRUE },
      error = function(e) { wizard_state$error_msg <- conditionMessage(e); FALSE }
    )
    
    if (isTRUE(result)) {
      # Update state and advance
      wizard_state$engine <- engine
      wizard_state$no_auth <- no_auth
      wizard_state$url <- url
      wizard_state$error_msg <- NULL
      wizard_state$current_step <- get_next_step("config", engine, no_auth)
    }
  })
  
  # --------------------------------------------------------------------------
  # Authentication Step Event Observers
  # --------------------------------------------------------------------------
  
  # Credentials authentication (username + password)
  shiny::observeEvent(input$auth_submit, {
    username <- trimws(input$auth_username)
    password <- input$auth_password
    
    # Validate non-empty
    if (nchar(username) == 0 || nchar(password) == 0) {
      wizard_state$error_msg <- "Username and password are required."
      return()
    }
    
    # Call login() with tryCatch
    result <- tryCatch(
      { login(username, password); TRUE },
      error = function(e) { wizard_state$error_msg <- conditionMessage(e); FALSE }
    )
    
    if (isTRUE(result)) {
      wizard_state$error_msg <- NULL
      wizard_state$current_step <- get_next_step("auth", wizard_state$engine, wizard_state$no_auth)
    }
  })
  
  # Token authentication (OAuth2)
  shiny::observeEvent(input$auth_token_submit, {
    token <- trimws(input$auth_token)
    
    # Validate non-empty
    if (nchar(token) == 0) {
      wizard_state$error_msg <- "API Token is required."
      return()
    }
    
    # Call set_token() with tryCatch
    result <- tryCatch(
      { set_token(token); TRUE },
      error = function(e) { wizard_state$error_msg <- conditionMessage(e); FALSE }
    )
    
    if (isTRUE(result)) {
      wizard_state$error_msg <- NULL
      wizard_state$current_step <- get_next_step("auth_token", wizard_state$engine, wizard_state$no_auth)
    }
  })
  
  # Auth back button - return to config step
  shiny::observeEvent(input$auth_back, {
    wizard_state$error_msg <- NULL
    wizard_state$current_step <- "config"
  })
  
  # Token auth back button - return to config step
  shiny::observeEvent(input$auth_token_back, {
    wizard_state$error_msg <- NULL
    wizard_state$current_step <- "config"
  })
  
  # --------------------------------------------------------------------------
  # Navigation Step Event Observers
  # --------------------------------------------------------------------------
  
  # Create observers for all possible navigation steps across all workflows
  all_nav_steps <- unique(unlist(lapply(engine_workflows, function(wf) {
    vapply(wf, function(s) s$id, character(1))
  })))
  
  # For each possible navigation step, create a "Next" button observer
  lapply(all_nav_steps, function(step_id) {
    local({
      sid <- step_id
      
      shiny::observeEvent(input[[paste0("nav_next_", sid)]], {
        # Find the current step definition
        workflow_key <- get_workflow_key(wizard_state$engine)
        workflow_steps <- engine_workflows[[workflow_key]]
        step_def <- NULL
        for (s in workflow_steps) {
          if (s$id == sid) { step_def <- s; break }
        }
        if (is.null(step_def)) return()
        
        # Get the selected value
        selected <- input[[paste0("nav_select_", sid)]]
        
        if (is.null(selected) || nchar(trimws(selected)) == 0) {
          wizard_state$error_msg <- "Please select an item before proceeding."
          return()
        }
        
        # Call the set function with tryCatch
        result <- tryCatch(
          { do.call(step_def$set_fn, list(selected)); TRUE },
          error = function(e) { wizard_state$error_msg <- conditionMessage(e); FALSE }
        )
        
        if (isTRUE(result)) {
          # Store selection
          wizard_state$selections[[step_def$selection_key]] <- selected
          wizard_state$error_msg <- NULL
          
          # Advance to next step
          wizard_state$current_step <- get_next_step(sid, wizard_state$engine, wizard_state$no_auth)
        }
      })
    })
  })
  
  # --------------------------------------------------------------------------
  # Back-Navigation and Cancel Handlers
  # --------------------------------------------------------------------------
  
  # Back button observers - reuses all_nav_steps computed before
  lapply(all_nav_steps, function(step_id) {
    local({
      sid <- step_id
      shiny::observeEvent(input[[paste0("nav_back_", sid)]], {
        prev_step <- get_prev_step(sid, wizard_state$engine, wizard_state$no_auth)
        if (!is.null(prev_step)) {
          wizard_state$error_msg <- NULL
          wizard_state$current_step <- prev_step
        }
      })
    })
  })
  
  # Cancel handler - restore prior state and close
  shiny::observeEvent(input$cancel, {
    restore_connection_state(prior_connection)
    if (is.na(used_ns)) { shiny::stopApp(NULL) }
  })
  
  # Done handler - return result list
  shiny::observeEvent(input$done, {
    result <- build_wizard_result(
      engine = wizard_state$engine,
      url = wizard_state$url,
      selections = wizard_state$selections
    )
    shiny::stopApp(result)
  })
  
  # When wizard reaches "done", build and store result
  shiny::observe({
    if (wizard_state$current_step == "done" && !wizard_state$completed) {
      wizard_state$completed <- TRUE
      wizard_state$result <- build_wizard_result(
        engine = wizard_state$engine,
        url = wizard_state$url,
        selections = wizard_state$selections
      )
    }
  })
  
  # Return a reactive with the result
  shiny::reactive({wizard_state$result})
}

#' Build Wizard Result
#'
#' Constructs the return value for a completed wizard session. Filters the
#' selections to include only the keys appropriate for the given engine's
#' workflow (standard engines get crop/program/trial/study, EBS gets
#' program/trial/study, GIGWA gets database/project/run).
#'
#' @param engine Character string: one of "bms", "breedbase", "ebs", "gigwa", "germinate".
#' @param url Character string: the configured server URL.
#' @param selections Named list of all selections made during the wizard session.
#'
#' @return A named list with three elements:
#'   \itemize{
#'     \item \code{engine}: the engine name
#'     \item \code{url}: the server URL
#'     \item \code{selections}: a named list containing only the keys
#'       appropriate for the engine's workflow
#'   }
#'
#' @examples
#' if (interactive()) {
#'   build_wizard_result("bms", "https://example.org/bms",
#'     list(crop = "wheat", program = "ICARDA", trial = "IDYT39", study = "Site A"))
#'   # list(engine = "bms", url = "https://example.org/bms",
#'   #      selections = list(crop = "wheat", program = "ICARDA", trial = "IDYT39", study = "Site A"))
#'
#'   build_wizard_result("ebs", "https://ebs.example.org",
#'     list(program = "Maize", trial = "IDYT39", study = "Loc1", extra = "ignored"))
#'   # list(engine = "ebs", url = "https://ebs.example.org",
#'   #      selections = list(program = "Maize", trial = "XYZ", study = "Loc1"))
#' }
#' 
#' @keywords internal

build_wizard_result <- function(engine, url, selections) {
  # Get expected selection keys from the engine's workflow definition
  workflow_key   <- get_workflow_key(engine)
  workflow_steps <- engine_workflows[[workflow_key]]
  expected_keys  <- vapply(workflow_steps, function(s) s$selection_key, character(1))

  # Filter selections to only include expected keys
  filtered_selections <- selections[intersect(names(selections), expected_keys)]

  list(engine = engine, url = url, selections = filtered_selections)
}


#' QBMS Wizard Module UI
#'
#' Creates the UI for embedding the QBMS Connection Wizard as a Shiny module
#' within an existing Shiny application or interactive R Markdown document.
#' Unlike \code{qbms_wizard()}, this does NOT call \code{runGadget()} and can
#' safely be used inside a running Shiny app.
#'
#' @param id Character string. The module namespace ID.
#' @param width Character string specifying the panel width as a valid CSS
#'   value (e.g., "400px" or "100%"). Defaults to "400px".
#'
#' @return A Shiny UI element (tagList) containing the wizard interface.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # In your Shiny app UI, embed the wizard directly as a module
#'   qbms_wizard_ui("my_wizard")
#' }
#'
#' @export

qbms_wizard_ui <- function(id, width = "400px") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      class = "panel panel-default",
      style = paste("width:", width, ";"),
      shiny::tags$div(class = "panel-heading", shiny::tags$h4(class = "panel-title", "QBMS Connection Wizard")),
      shiny::tags$div(class = "panel-body", style = "min-height: 400px;", shiny::uiOutput(ns("step_content")), shiny::uiOutput(ns("step_error")))
    )
  )
}

#' QBMS Wizard Module Server
#'
#' Server logic for the QBMS Connection Wizard Shiny module. Call this in your
#' app's server function to activate the wizard. Returns a reactive containing
#' the wizard result (NULL until the user completes the wizard).
#'
#' @param id Character string. The module namespace ID (must match the UI).
#'
#' @return A \code{shiny::reactive} that returns NULL while the wizard is in
#'   progress, and a named list with \code{engine}, \code{url}, and
#'   \code{selections} once the user completes all steps.
#'
#' @examples
#' if (interactive()) {
#'   # In your Shiny app server, activate the wizard module and returns a reactive with the result
#'   result <- qbms_wizard_server("my_wizard")
#'   observe({ req(result()); print(result()) })
#' }
#'
#' @export

qbms_wizard_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    gadget_server(input, output, session, as_module = TRUE)
  })
}


#' Check if RStudio is Available
#'
#' @return Logical TRUE if running in RStudio, FALSE otherwise.
#' @keywords internal

rstudio_is_available <- function() {
  tryCatch(
    rstudioapi::isAvailable(),
    error = function(e) FALSE
  )
}


#' QBMS Connection Wizard (Console / Gadget mode)
#'
#' Launches an interactive wizard as a Shiny Gadget for configuring QBMS server
#' connections. This function is intended for use from the RStudio console or
#' standalone R scripts. It calls \code{runGadget()} and blocks until the user
#' completes or cancels the wizard.
#'
#' \strong{Do NOT call this from within a running Shiny app or R Markdown document.}
#' For those use cases, use \code{qbms_wizard_ui()} and \code{qbms_wizard_server()}
#' instead (the Shiny module interface).
#'
#' @param viewer Function to use for displaying the gadget. If NULL (default),
#'   automatically selects: \code{paneViewer()} in RStudio, \code{browserViewer()}
#'   otherwise.
#'
#' @return A named list with \code{engine}, \code{url}, and \code{selections} on
#'   successful completion, or NULL if cancelled.
#'
#' @export

qbms_wizard <- function(viewer = NULL) {
  # Auto-select viewer if not provided
  if (is.null(viewer)) {
    viewer <- if (rstudio_is_available()) {
      shiny::paneViewer(minHeight = 500)
    } else {
      shiny::browserViewer()
    }
  }

  # Run the gadget and return result
  shiny::runGadget(gadget_ui, wizard_server, viewer = viewer)
}
