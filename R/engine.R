
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})

engine_pre_process <- function(call_url, engine, func_name) {
  if (engine == "breedbase" & func_name == "list_studies") {

    # handle the case of BreedBase trials (studies) listed in the root program folder (trial)
    if (qbms_globals$state$trial_db_id == qbms_globals$state$program_db_id) {
      call_url <- sub("\\?trialDbId\\=", '?programDbId=', call_url)
    }
  }
  
  # if (engine == "germinate" & func_name == "get_study_data") {
  #   # currently no pagination info returns in the metadata block of the sponse
  #   call_url <- gsub("&pageSize=[0-9]+", "", call_url)
  # }
  
  call_url
}

#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})

engine_post_process <- function(results, engine, func_name) {
  if (engine == "breedbase" & func_name == "get_program_trials") {
    results$data$studies_no <- sapply(results$data$studies, nrow)
    results$data <- results$data[results$data$studies_no > 0,]
  }
  
  if (engine == "breedbase" & func_name == "get_study_data") {
    results$data <- results$data[-1, ]
  }

  if (engine == "breedbase" & func_name == "list_studies") {

    # handle the case of BreedBase trials (studies) listed in the root program folder (trial)
    if (qbms_globals$state$trial_db_id == qbms_globals$state$program_db_id) {
      results$data <- results$data[is.na(results$data$trialName), ]
      rownames(results$data) <- NULL
    }
  }
  
  if (engine == "breedbase" & func_name == "get_germplasm_list") {
    results$data$check <- NA
    results$data[, c("synonyms")] <- list(NULL)
  }
  
  if (engine == "ebs" & func_name == "get_germplasm_list") {
    results$data$check <- 0

    nested_lists <- c("synonyms", "donors", "externalReferences", "germplasmOrigin",
                      "storageTypes", "taxonIds", "documentationURL", "additionalInfo")

    results$data[, nested_lists] <- NULL
    results$data <- results$data[, colSums(is.na(results$data)) != nrow(results$data)]
  }
  
  if (engine == "bms" & func_name == "list_programs") {
    names(results$data)[names(results$data) == "name"] <- "programName"
  }
  
  if (engine == "germinate" & func_name == "get_variants") {
    results$data$genotypeValue <- gsub("^(.+)null(.+)$", "\\1/\\2", results$data$genotypeValue)
  }

  results
}

