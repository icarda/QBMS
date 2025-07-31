# QBMS 2.0.0 _2025-08-02_
  * Add [Germinate](https://germinateplatform.github.io/get-germinate/) support using BrAPI v2 calls.
  * Add new generic genotyping functions including `list_variantsets()`, `set_variantset()`, `get_variantset()`, `get_variants()`, and `get_marker_map()`.
  * Add new `get_trial_pedigree()` function supports db compliance with BrAPI v2.
  * Add new generic `login()` function wraps around all engine specific `login_*` functions.
  * Fix a BreedBase login bug caused by incorrect httr2 form encoding when sending credentials.
  * Move special-case processing from qbms.R (before/after API calls) into the new internal engine.R module script.

# QBMS 1.5.0 _2024-09-19_
  * Updated the core QBMS script to replace the non-CRAN [async](https://github.com/gaborcsardi/async) R package with the CRAN-standard [future](https://cran.r-project.org/package=future) and [future.apply](https://cran.r-project.org/package=future.apply) packages.
  * Updated the core QBMS script to substitute the deprecated [httr](https://httr.r-lib.org/) R package with the recommended [httr2](https://cran.r-project.org/package=httr2) package.
  * Add new `gigwa_get_allelematrix()` function that boosting the marker data retrieval speed by more than 10 times.
  * Add new `gigwa_get_markers()` function to retrieve the marker map that describe the physical positions of the markers on the chromosomes.
  * Add new `gigwa_get_sequences()` function to retrieve the list of sequences in the selected GIGWA project.
  * Improve `get_trial_obs_ontology()` function performance for systems supports BrAPI v2 /search/variables endpoint.
  * Improve package documentation with enhanced details.
  * Implement an internal central mechanism to handle all [BrAPI POST /search](https://plant-breeding-api.readthedocs.io/en/latest/docs/best_practices/Search_Services.html#post-search-entity) endpoints, including the 202 status code or searchResultsDbId response.

# QBMS 1.0.0 _2024-03-07_
  * Add [EBS](https://ebs.excellenceinbreeding.org/) support using BrAPI v2 calls.
  * Implement a new generic internal mechanism that supports both BrAPI versions 1 and 2 calls for all core functions.
  * Support the OAuth 2.0 authentication (Authorization Code Grant flow) using the new `login_oauth2()` function.
  * Support external authorization option and set access token value manually using the new `set_token()` function.
  * Support downloading TerraClimate netCDF data files to extract their data offline using the new `ini_terraclimate()` function.
  * Add the new `scan_brapi_endpoints()` utility function to scan available BrAPI endpoints in the configured data source server.
  * Enhance the `gigwa_get_variants()` function by adding 3 extra filtering parameters: start position, end position, and reference sequence name.
  * Fix the BreedBase login problem due to the form encoding issue by adding the new function `login_breedbase()` to pass the correct/expected encoding.
  * Add [HWSD v2.0](https://gaez.fao.org/pages/hwsd) support to query and retrieve the FAO harmonized soil data.
  * Resolved several minor bugs reported by [EBS](https://ebs.excellenceinbreeding.org/), [BreedBase](https://breedbase.org/), and [DeltaBreed](https://app.breedinginsight.net/) users for improved stability.

# QBMS 0.9.1 _2023-03-28_
  * Hot fix the reported validation issue in the `gigwa_set_db()` function.

# QBMS 0.9.0 _2023-03-20_
  * Implement caching techniques wherever applicable for faster response times to improve user experience. Thanks to [Johan Steven Aparicio](https://github.com/AparicioJohan) for initiating this.
  * Improve the performance of multi-page API calls by [optionally enabling asynchronous calls](https://github.com/gaborcsardi/async) to prevent blocking behavior by fetching all requested pages simultaneously.
  * Add TerraClimate support to query and retrieve climate data (including the [19 BIOCLIM variables](https://www.worldclim.org/data/bioclim.html)).
  * Add a new `get_germplasm_attributes()` function to retrieve germplasm attributes. Thanks to [Johan Steven Aparicio](https://github.com/AparicioJohan) for his contribution.
  * Add a new `gigwa_get_metadata()` function to retrieve metadata from GIGWA database.
  * Add new `get_qbms_connection()` and `set_qbms_connection()` functions to support connecting to multiple providers. Thanks to [Francisco Agosto-Perez](https://github.com/agostof) for the suggestions.
  * The `get_pedigree_table()` function can properly detect and handle backcross cases.
  * Fix the error message when calling the `get_pedigree_table()` function if there is no case of similar genotype names. Thanks to [Johan Steven Aparicio](https://github.com/AparicioJohan) for reporting this.

# QBMS 0.8.0 _2022-05-18_ 
  * Add GIGWA support (required version 2.4.1 or higher) to query and retrieve SNPs data for GWAS and GS analysis pipelines.
  * Tiny usability enhancements (e.g., automate path parameter setting, show the server name in the login window, add progress bar to the get_program_studies function).
  * Fix mistakenly redundant location names/info returned by the get_program_studies function.

# QBMS 0.7.0 _2022-03-03_ 
  * Add BreedBase support using BrAPI v1 calls.
  * Add functionality to get the pedigree table starting from germplasm dataset.
  * Improve set_qbms_config to generalize the way of getting the server domain from the URL.
  * Default timeout become 120 sec instead of 10.
  * Set default encoding for HTTP content to UTF-8.

# QBMS 0.6.0 _2021-10-08_ 
  * Fix filter by year functionality in the list_trials function.
  * Fix get_germplasm_data by replaced the deprecated germplasm-search call.
  * Minimize package dependencies (rbindx replaced plyr::rbind.fill, rbindlistx replaced data.table::rbindlist, and use merge to replace dplyr::left_join).
  * Resolve compatibility issues with BrAPI changes in BMS version 19.
  * Enable to set the connection time_out in the set_qbms_config function.
  * Get entry type (test or check) in the get_germplasm_list returned data frame.

# QBMS 0.5.0 _2021-07-08_ 
  * Fix the issue of empty list in get_germplasm_data returned results.
  * Fix retrieving error when the study has no data!
  * Enhance returned info by the get_program_studies function to include study settings and number of test/check entries.

# QBMS 0.4.1 _2020-10-16_ 
  * Simplify configuration by required only the URL of the BMS login page.
  * Improve the performance of the internal get_program_trials function by passing the programDbId in the /trials GET call.
  * Add debug_qbms function to get the internal config/state object.

# QBMS 0.4.0 _2020-07-03_ 
  * Convert it into an R package.
  * Add set_qbms_config function to setup connection configuration variables.
  * Use the double colon approach for functions from external packages.
  * Fix the deprecated API call in the get_trial_obs_ontology function.

# QBMS 0.3.1 _2020-06-09_ 
  * Fix the "get_trial_data" function bug when you have more than one study in the same location. 
  * Function "list_studies" returns studyName also, and function "set_study" input is studyName now.
  * Simplify the "get_germplasm_list" function output by getting rid of nested lists.
  * Deprecate the "list_all_studies" function in favor of "get_program_studies" function.

# QBMS 0.3.0 _2020-06-02_ 
  * Call BrAPI directly (i.e. not required "CIP-RIU/brapi" from GitHub anymore).
  * Add a function to get all data of the current active trial (combined all studies).
  * Add a function to get a list of studies where given germplasm has been used.
  * Add a function to get a specific germplasm data from all program trials.
  * Handle BrAPI pagination in a proper way.

# QBMS 0.2.0 _2019-08-20_ 
  * Adopt tidyverse style guide https://style.tidyverse.org/
  * Add functions documentation using roxygen2 format.
  * Add basic error handling to the functions.
  * Add a function to retrieve the traits ontology of a trial.

# QBMS 0.1.0 _2019-07-24_ 
  * Initial version.
