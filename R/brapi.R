#' BrAPI Mapping Table
#'
#' @description
#' This internal table maps QBMS function names to corresponding BrAPI calls for different versions 
#' of the BrAPI standard. It is used internally by the QBMS system to translate function calls into 
#' BrAPI-compliant API requests.
#'
#' @details
#' This table supports both BrAPI v1 and v2 endpoints. It is regularly updated to reflect changes 
#' in the BrAPI standard and the inclusion of new API calls.

# /brapi/{brapi_ver}/{brapi_call}
brapi_map <- data.frame(func_name  = character(), 
                        brapi_ver  = character(),
                        brapi_call = character())

# need to be deprecated in favor of list_dbs() and set_db() functions
# https://github.com/plantbreeding/BrAPI/issues/495
brapi_map <- rbind(brapi_map, c("list_crops", "v1", "crops"))
brapi_map <- rbind(brapi_map, c("list_crops", "v2", "commoncropnames"))

brapi_map <- rbind(brapi_map, c("list_programs", "v1", "programs"))
brapi_map <- rbind(brapi_map, c("list_programs", "v2", "programs"))

brapi_map <- rbind(brapi_map, c("get_program_trials", "v1", "trials?programDbId={programDbId}"))
brapi_map <- rbind(brapi_map, c("get_program_trials", "v2", "trials?programDbId={programDbId}"))

brapi_map <- rbind(brapi_map, c("list_studies", "v1", "studies?trialDbId={trialDbId}"))
brapi_map <- rbind(brapi_map, c("list_studies", "v2", "studies?trialDbId={trialDbId}"))

brapi_map <- rbind(brapi_map, c("get_study_info", "v1", "studies/{studyDbId}"))
brapi_map <- rbind(brapi_map, c("get_study_info", "v2", "studies/{studyDbId}"))

brapi_map <- rbind(brapi_map, c("get_study_data", "v1", "studies/{studyDbId}/table"))
brapi_map <- rbind(brapi_map, c("get_study_data", "v2", "observations/table?studyDbId={studyDbId}"))

brapi_map <- rbind(brapi_map, c("get_germplasm_list", "v1", "studies/{studyDbId}/germplasm"))
brapi_map <- rbind(brapi_map, c("get_germplasm_list", "v2", "germplasm?studyDbId={studyDbId}"))

brapi_map <- rbind(brapi_map, c("list_locations", "v1", "locations"))
brapi_map <- rbind(brapi_map, c("list_locations", "v2", "locations"))

brapi_map <- rbind(brapi_map, c("get_trial_obs_ontology", "v1", "variables"))
brapi_map <- rbind(brapi_map, c("get_trial_obs_ontology", "v2", "search/variables"))

brapi_map <- rbind(brapi_map, c("get_germplasm_id", "v1", "germplasm?germplasmName={germplasmName}"))
brapi_map <- rbind(brapi_map, c("get_germplasm_id", "v2", "germplasm?germplasmName={germplasmName}"))

brapi_map <- rbind(brapi_map, c("get_trial_pedigree", "v2", "pedigree?trialDbId={trialDbId}&includeFullTree=true&includeParents=true"))

brapi_map <- rbind(brapi_map, c("list_runs", "v2", "variantsets?programDbId={programDbId}&studyDbId={studyDbId}"))
brapi_map <- rbind(brapi_map, c("get_variants", "v2", "variantsets/{variantSetDbId}/calls"))
brapi_map <- rbind(brapi_map, c("get_variant_set", "v2", "variantsets/{variantSetDbId}"))

brapi_map <- rbind(brapi_map, c("get_map", "v2", "maps?studyDbId={studyDbId}"))
brapi_map <- rbind(brapi_map, c("get_marker_map", "v2", "markerpositions?mapDbId={mapDbId}"))


# POST: germplasmDbIds, observationLevel = "PLOT"
brapi_map <- rbind(brapi_map, c("get_germplasm_data", "v1", "phenotypes-search"))

brapi_map <- rbind(brapi_map, c("get_germplasm_attributes", "v1", "germplasm/{germplasmDbId}/attributes"))
brapi_map <- rbind(brapi_map, c("get_germplasm_attributes", "v2", "attributes?germplasmDbId={germplasmDbId}"))

# gigwa brapi_get_call(s)
brapi_map <- rbind(brapi_map, c("gigwa_list_dbs", "v2", "programs"))
brapi_map <- rbind(brapi_map, c("gigwa_list_projects", "v2", "studies?programDbId={programDbId}"))

# gigwa brapi_post_search_call(s)
brapi_map <- rbind(brapi_map, c("gigwa_list_runs", "v2", "search/variantsets"))
brapi_map <- rbind(brapi_map, c("gigwa_get_samples", "v2", "search/samples"))
brapi_map <- rbind(brapi_map, c("gigwa_get_sequences", "v2", "search/references"))
brapi_map <- rbind(brapi_map, c("gigwa_get_allelematrix", "v2", "search/allelematrix"))
brapi_map <- rbind(brapi_map, c("gigwa_get_markers", "v2", "search/variants"))
brapi_map <- rbind(brapi_map, c("gigwa_get_metadata", "v2", "search/attributevalues"))

colnames(brapi_map) <- c("func_name", "brapi_ver", "brapi_call")


# List of non-BrAPI calls in QBMS functions
#
# get_program_studies()
# /crops/{cropName}/programs/{programUUID}/studies/{studyId}/entries/metadata (BMS: get study entries metadata)
#
# get_germplasm_list()
# /crops/{cropName}/programs/{programUUID}/studies/{studyId}/entries (BMS: get entry type) (POST: body = "")
#
# gigwa_get_variants()
# /ga4gh/variants/search 
# 
# dancing steps: 
# - searchMode = 0 to get total
# - then searchMode = 3 to request actual results
# - keep checking progress status /gigwa/progress
# - then call the same /ga4gh/variants/search to get the ready results
# 
# GA4GH: https://rest.ensembl.org/documentation/info/gavariants
# BrAPI: https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.1#/Allele%20Matrix


#' Get the BrAPI Endpoint URL for a given QBMS function
#'
#' @description
#' Constructs the BrAPI endpoint URL for a given QBMS function based on the configured server,
#' crop, and BrAPI version. The function name is mapped to the corresponding BrAPI call using
#' internal mapping.
#'
#' @param func_name (string) The name of the QBMS function for which the BrAPI endpoint URL is required.
#'
#' @return
#' A string representing the BrAPI endpoint URL.
#'
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})

get_brapi_url <- function(func_name) {
  call_url <- paste0(qbms_globals$config$base_url, 
                     ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)), 
                     "/brapi/", qbms_globals$config$brapi_ver, "/", 
                     brapi_map[brapi_map$func_name == func_name & brapi_map$brapi_ver == qbms_globals$config$brapi_ver, "brapi_call"])
  
  return(call_url)
}


#' Scan BrAPI Endpoints
#'
#' @description
#' Scans the available BrAPI endpoints on the configured source server and checks their accessibility. 
#' This function allows users to verify which BrAPI endpoints are available based on the provided IDs.
#'
#' @param programDbId (numeric) The programDbId to scan specific program-related endpoints (default is 0).
#' @param trialDbId (numeric) The trialDbId to scan specific trial-related endpoints (default is 0).
#' @param studyDbId (numeric) The studyDbId to scan specific study-related endpoints (default is 0).
#'
#' @return
#' A data frame listing the QBMS function, BrAPI endpoint URL, and availability status for each endpoint.
#'
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#'
#' @export

scan_brapi_endpoints <- function(programDbId = 0, trialDbId = 0, studyDbId = 0) {
  if (is.null(qbms_globals$config$base_url)) {
    stop("No server has been defined yet! You have to set your server configurations first using the `set_qbms_config()` function")
  }

  # need to improve this functionality using the BrAPI /brapi/v2/serverinfo endpoint response
  
  call_url <- paste0(
    qbms_globals$config$base_url,
    ifelse(qbms_globals$config$crop == "", "", paste0("/", qbms_globals$config$crop)),
    "/brapi/", brapi_map$brapi_ver, "/",
    brapi_map$brapi_call
  )

  call_url <- sub("\\{programDbId\\}", programDbId, call_url)
  call_url <- sub("\\{trialDbId\\}", trialDbId, call_url)
  call_url <- sub("\\{studyDbId\\}", studyDbId, call_url)
  call_url <- sub("\\{.*\\}", "1", call_url)

  # Ensure getting the minimum data while scanning BrAPI endpoints
  call_url <- ifelse(
    grepl("\\?", call_url),
    paste0(call_url, "&pageSize=1"),
    paste0(call_url, "?pageSize=1")
  )

  scan_result <- sapply(call_url, function(url) {
    # Create the request
    req <- httr2::request(url)
    req <- httr2::req_headers(req, "accept" = "application/json")
    req <- httr2::req_headers(req, "Accept-Encoding" = "gzip, deflate")
    req <- httr2::req_headers(req, "Authorization" = paste0("Bearer ", qbms_globals$state$token))
    
    # Perform the request
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) NULL
    )

    # Check if the response indicates an error
    if (is.null(resp)) {
      FALSE  # Endpoint is not available
    } else {
      !httr2::resp_is_error(resp)  # TRUE if available, FALSE otherwise
    }
  })

  scan_result <- as.data.frame(cbind(brapi_map$func_name, call_url, scan_result))

  rownames(scan_result) <- NULL
  colnames(scan_result) <- c("QBMS Function", "BrAPI endpoint", "Available")

  return(scan_result)
}


# Internal state variables/lists
qbms_globals <- new.env()
qbms_globals$config <- list(crop = NULL)
qbms_globals$state  <- list(token = NULL)


#' Debug Internal QBMS Status Object
#'
#' @description
#' Retrieves the internal QBMS status object for debugging purposes. This object contains
#' the current configuration and state of the QBMS session, including connection settings
#' and active tokens.
#'
#' @return
#' An environment object that holds the current QBMS configuration and state.
#'
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#'
#' @examples
#' if (interactive()) {
#'   obj <- debug_qbms()
#'   obj$config
#'   obj$state
#' }
#'
#' @export

debug_qbms <- function() {
  return(qbms_globals)
}


#' Get the QBMS Connection
#'
#' @description
#' Retrieves the current QBMS connection object, which contains the server's configuration 
#' and state, including any active sessions and tokens. This can be used to save and restore 
#' connections between sessions.
#'
#' @return
#' A list containing the current QBMS configuration and state.
#'
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#'
#' @seealso \code{\link{set_qbms_connection}}
#'
#' @examples
#' if(interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   
#'   df1  <- get_germplasm_data("Jabal")
#'   con1 <- get_qbms_connection()
#'   
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", engine = "gigwa", no_auth = TRUE)
#'   
#'   gigwa_set_db("DIVRICE_NB")
#'   gigwa_set_project("refNB")
#'   gigwa_set_run("03052022")
#'   
#'   df2  <- gigwa_get_metadata()
#'   con2 <- get_qbms_connection()
#'   
#'   set_qbms_connection(con1)
#'   df3 <- get_germplasm_attributes("Jabal")
#' }
#'
#' @export

get_qbms_connection <- function() {
  return(as.list(qbms_globals))
}


#' Set the QBMS Connection
#'
#' @description
#' Sets the QBMS connection object in the current environment, allowing users to restore
#' a saved connection, including configuration settings and session tokens.
#'
#' @param env A list containing the saved connection configuration and state.
#' 
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#'
#' @seealso \code{\link{get_qbms_connection}}
#'
#' @examples
#' if(interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   
#'   df1  <- get_germplasm_data("Jabal")
#'   con1 <- get_qbms_connection()
#'   
#'   set_qbms_config("https://gigwa.southgreen.fr/gigwa/", engine = "gigwa", no_auth = TRUE)
#'   
#'   gigwa_set_db("DIVRICE_NB")
#'   gigwa_set_project("refNB")
#'   gigwa_set_run("03052022")
#'   
#'   df2  <- gigwa_get_metadata()
#'   con2 <- get_qbms_connection()
#'   
#'   set_qbms_connection(con1)
#'   df3 <- get_germplasm_attributes("Jabal")
#' }
#'
#' @export

set_qbms_connection <- function(env) {
  qbms_globals$config <- list(crop = NULL)
  qbms_globals$state  <- list(token = NULL)
  
  if (!is.null(env$config)) {
    qbms_globals$config <- env$config
  }
  
  if (!is.null(env$state)) {
    qbms_globals$state <- env$state
  }
}


#' Configure BMS Server Settings
#'
#' @description
#' Configures the BMS server connection settings, including URL, API path, page size, and timeout.
#' This function allows you to set up the connection for different server backends like BMS, Gigwa, 
#' EBS, and Breedbase, and choose the appropriate BrAPI version.
#'
#' @param url The URL of the BMS login page or API base (default is "http://localhost").
#' @param path The API path to use (default is NULL, which sets a path based on the engine).
#' @param page_size The number of records per page when making API calls (default is 1000).
#' @param time_out The maximum number of seconds to wait for a response (default is 120).
#' @param no_auth Logical, whether the server requires authentication (default is FALSE).
#' @param engine The backend system (default is "bms"). Options include "bms", "gigwa", "breedbase", "ebs", "germinate".
#' @param brapi_ver The version of BrAPI to use, either "v1" or "v2" (default is "v1").
#' @param verbose Logical, indicating whether to display progress information when making API calls (default is TRUE).
#'
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#'
#' @return
#' No return value.
#'
#' @examples
#' set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'
#' @export

set_qbms_config <- function(url = "http://localhost",
                            path = NULL, page_size = 1000, time_out = 120, no_auth = FALSE, 
                            engine = "bms", brapi_ver = "v1", verbose = TRUE) {
  
  if (is.null(path)) {
    if (engine == "bms") { path = "bmsapi" }
    if (engine == "breedbase") { path = "" }
    if (engine == "gigwa") { path = paste0(sub("^https?://[^/]+/([^/]+).*", "\\1", url), "/rest"); brapi_ver = "v2"}
    if (engine == "ebs") { path = ""; brapi_ver = "v2" }
    if (engine == "germinate") { path = sub("^https?://[^/]+/?", "", sub("(/#.*|/*$)", "/api", url)); brapi_ver = "v2"}
  }
  
  qbms_globals$config <- list(crop = "")
  qbms_globals$state  <- list(token = NULL)
  
  if (engine == "bms") { qbms_globals$config$crop <- NULL }
  
  qbms_globals$config$server    <- regmatches(url, regexpr("^(?://|[^/]+)*", url))
  qbms_globals$config$path      <- path
  qbms_globals$config$page_size <- page_size
  qbms_globals$config$time_out  <- time_out
  qbms_globals$config$base_url  <- paste0(qbms_globals$config$server, ifelse(path == "", "", ifelse(substr(path, 1, 1) == "/", path, paste0("/", path))))
  qbms_globals$config$engine    <- engine
  qbms_globals$config$brapi_ver <- brapi_ver
  qbms_globals$config$verbose   <- verbose
  qbms_globals$config$sleep     <- 1
  
  if (no_auth == TRUE) {
    qbms_globals$state$token <- NA
  }
  
  qbms_globals$state$crops <- NULL
}
