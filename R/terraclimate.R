### TerraClimate ###############################################################

#' Get TerraClimate Data for a Given Coordinate(s)
#'
#' @description
#' This function allows you to extract climate variables from the 
#' \href{https://www.climatologylab.org/terraclimate.html}{TerraClimate} dataset for specific geographic coordinates. 
#' TerraClimate is a global dataset of monthly climate data covering the years 1950-present. The function retrieves 
#' \href{https://www.climatologylab.org/terraclimate-variables.html}{climate variables} directly from the hosting server provided by the 
#' \href{https://hpc.uidaho.edu/}{University of Idaho}, avoiding the need to download large raster files in netCDF format.
#' Additionally, the function calculates \href{https://www.worldclim.org/data/bioclim.html}{bioclimatic variables} 
#' using the \code{\link{calc_biovars}} function, derived from the \href{https://github.com/rspatial/dismo/blob/master/R/biovars.R}{dismo R package}.
#'
#' The TerraClimate dataset is compared with \href{https://www.worldclim.org/data/worldclim21.html}{WorldClim} in several aspects:
#' \itemize{
#'   \item TerraClimate: 1950-present vs. WorldClim: 1970-2000
#'   \item 14 climate variables vs. 7 climate variables in WorldClim
#'   \item Spatial resolution: ~4 km (TerraClimate) vs. ~1 km (WorldClim)
#'   \item Need to calculate bioclimatic variables (TerraClimate) vs. pre-calculated (WorldClim)
#' }
#'
#' @references 
#' Abatzoglou, J., Dobrowski, S., Parks, S. \emph{et al.} TerraClimate, a high-resolution 
#' global dataset of monthly climate and climatic water balance from 1958-2015. 
#' \emph{Sci Data} \strong{5}, 170191 (2018). \doi{10.1038/sdata.2017.191}
#' 
#' @param lat Vector of Latitude(s) in decimal degree format. Each entry corresponds to a location of interest.
#' @param lon Vector of Longitude(s) in decimal degree format. Each entry corresponds to a location of interest.
#' @param from Start date as a string in 'YYYY-MM-DD' format. Defines the beginning of the time range for data extraction.
#' @param to End date as a string in 'YYYY-MM-DD' format. Defines the end of the time range for data extraction.
#' @param clim_vars List of climate variables to extract. Valid options include: \emph{aet, def, pet, ppt, q, soil, 
#'                  srad, swe, tmax, tmin, vap, ws, vpd, and PDSI}. Default is \code{NULL}, which retrieves all variables.
#' @param month_mask A vector specifying the months of interest, e.g., for specific seasons (e.g., planting season: 
#'                   \code{c(10:12, 1:5)}). Default is \code{NULL}, which retrieves data for all months.
#' @param offline Logical value indicating whether to extract TerraClimate data from pre-downloaded netCDF files. 
#'                Default is \code{FALSE}, meaning data is fetched from the remote server.
#' @param data_path String specifying the directory path where downloaded netCDF files are stored when working offline. 
#'                  Default is './data/'.
#' 
#' @return 
#' A list of two data frames for each coordinate pair (latitude and longitude):
#' \itemize{
#'   \item \strong{climate:} A data frame containing the requested climate variables for each month and location.
#'   \item \strong{biovars:} A data frame with calculated bioclimatic variables, based on the extracted climate data.
#' }
#' Each data frame is in a format ready for further analysis in R.
#' 
#' @author 
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @seealso 
#' \code{\link{ini_terraclimate}}, \code{\link{calc_biovars}}
#' 
#' @examples
#' if (interactive()) {
#'   # data <- get_terraclimate(36.016, 36.943, '1979-09-01', '2012-06-30', 
#'   #                          c('ppt', 'tmin', 'tmax'), c(10:12,1:5))
#'   data <- get_terraclimate(36.016, 36.943, '1979-09-01', '2012-06-30')
#' 
#'   View(data$climate[[1]])  # View the climate data
#'   View(data$biovars[[1]])  # View the bioclimatic variables
#' }
#' 
#' @export

get_terraclimate <- function(lat, lon, from = '1950-01-01', to = '2025-12-31', clim_vars = NULL, month_mask = NULL, offline = FALSE, data_path = './data/'){
  # check if the given lat coordinate values are valid
  lat <- as.numeric(lat)
  if (!all(lat >= -90 & lat <= 90)) {
    stop("The Latitude should be in decimal degree format and range from -90 to +90")
  }
  
  # check if the given lon coordinate values are valid
  lon <- as.numeric(lon)
  if (!all(lon >= -180 & lon <= 180)) {
    stop("The Longitude should be in decimal degree format and range from -180 to +180")
  }
  
  if (length(lat) == length(lon)) {
    loc_num <- length(lat)
  } else {
    stop("The number of Latitude/Longitude values should be the same!")
  }
  
  # check if the given from and to date values are valid
  start_date <- as.Date(from)
  final_date <- as.Date(to)   
  
  terraclimate_vars <- c('aet', 'def', 'pet', 'ppt', 'q', 'soil', 'srad', 'swe', 'tmax', 'tmin', 'vap', 'ws', 'vpd', 'PDSI')
  
  if (is.null(clim_vars)) {
    clim_vars <- terraclimate_vars
  } else {
    if (!all(clim_vars %in% terraclimate_vars)) stop("The given list of climatic variables is supported/existed in TerraClimate dataset!")
  }
  
  if (is.null(month_mask)) {
    if (is.null(month_mask)) month_mask <- 1:12
  } else {
    if (!all(month_mask %in% 1:12)) stop("The given month_mask values are valid!")
  }
  
  clim_data <- list()
  biovars   <- list()
  
  start_month <- as.numeric(format(start_date, '%m'))
  start_year  <- as.numeric(format(start_date, '%Y'))
  start_row   <- (start_year - 1950) * 12 + start_month
  
  final_month <- as.numeric(format(final_date, '%m'))
  final_year  <- as.numeric(format(final_date, '%Y'))
  final_row   <- (final_year - 1950) * 12 + final_month
  
  if (offline == TRUE) {
    years_num <- final_year - start_year + 1
    
    loc_matrix <- matrix(data = NA, nrow = 12 * years_num, ncol = length(clim_vars) + 2)
    
    loc_matrix[, 1] <- rep(start_year:final_year, each = 12)
    loc_matrix[, 2] <- rep(1:12, times = years_num)
    
    colnames(loc_matrix) <- c('year', 'month', clim_vars)
    
    for (i in 1:loc_num) clim_data[[i]] <- data.frame(loc_matrix)
    
    pb <- utils::txtProgressBar(min = 0, max = length(clim_vars) * years_num, initial = 0, style = 3) 
    
    for (var in clim_vars) {
      for (year in start_year:final_year) {
        file_path <- paste0(data_path, 'TerraClimate_', var, '_', year, '.nc')
        
        nc_metadata   <- RNetCDF::open.nc(file_path)
        
        lat_available <- RNetCDF::var.get.nc(nc_metadata, 'lat')
        lon_available <- RNetCDF::var.get.nc(nc_metadata, 'lon')
        
        obs_available <- length(RNetCDF::var.get.nc(nc_metadata, 'time'))
        
        for (i in 1:loc_num) {
          lat_index <- which.min(abs(lat_available - lat[i]))
          lon_index <- which.min(abs(lon_available - lon[i]))
          
          start <- c(lon_index, lat_index, 1)
          count <- c(1, 1, NA)
          
          values <- as.numeric(RNetCDF::var.get.nc(nc_metadata, variable = var, start = start, count = count, unpack = TRUE))
          
          col_index <- which(var == clim_vars) + 2
          row_index <- 12 * (year - start_year) + 1
          
          clim_data[[i]][row_index:(row_index + 11), col_index] <- values
        }
        
        utils::setTxtProgressBar(pb, years_num * (which(var == clim_vars) - 1) + (year - start_year + 1))
      }
    }
    
    close(pb)
    
    for (i in 1:loc_num) {
      if (all(c('ppt', 'tmin', 'tmax') %in% clim_vars)) {
        biovars[[i]] <- calc_biovars(clim_data[[i]])
      } else {
        biovars[[i]] <- NULL
      }
      
      clim_data[[i]] <- clim_data[[i]][start_month:(nrow(clim_data[[i]]) - final_month), ]
      clim_data[[i]] <- clim_data[[i]][clim_data[[i]]$month %in% month_mask, ]
      
      rownames(clim_data[[i]]) <- 1:nrow(clim_data[[i]])
    }
  } else {
    # setup the progress bar
    pb <- utils::txtProgressBar(min = 0, max = loc_num * length(clim_vars), initial = 0, style = 3) 
    
    for (var in clim_vars) {
      aggregate_url <- paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_', var, '_1950_CurrentYear_GLOBE.nc')
      nc_metadata   <- RNetCDF::open.nc(aggregate_url)
      
      lat_available <- RNetCDF::var.get.nc(nc_metadata, 'lat')
      lon_available <- RNetCDF::var.get.nc(nc_metadata, 'lon')
      
      obs_available <- length(RNetCDF::var.get.nc(nc_metadata, 'time'))
      
      for (i in 1:loc_num) {
        if (length(clim_data) < i) clim_data[[i]] <- data.frame(matrix(nrow = obs_available, ncol = 0))
        
        lat_index <- which.min(abs(lat_available - lat[i]))
        lon_index <- which.min(abs(lon_available - lon[i]))
        
        start <- c(lon_index, lat_index, 1)
        count <- c(1, 1, NA)
        
        # read in the full period of record using aggregated files
        values <- as.numeric(RNetCDF::var.get.nc(nc_metadata, variable = var, start = start, count = count, unpack = TRUE))
        
        clim_data[[i]] <- cbind(clim_data[[i]], values)
        
        # update the progress bar
        utils::setTxtProgressBar(pb, loc_num * which(clim_vars == var) - (loc_num - i))
      }
    }
    
    close(pb)
    
    for (i in 1:loc_num) {
      colnames(clim_data[[i]]) <- clim_vars
      
      years <- obs_available / 12
      last  <- 1950 + years - 1
      month <- rep(1:12, times = years)
      year  <- rep(1950:last, each = 12)
      
      clim_data[[i]] <- cbind(year, month, clim_data[[i]])
      
      if (all(c('ppt', 'tmin', 'tmax') %in% clim_vars)) {
        biovars[[i]] <- calc_biovars(clim_data[[i]][(start_row - (start_month - 1)):(final_row + (12 - final_month)), ])
      } else {
        biovars[[i]] <- NULL
      }
      
      clim_data[[i]] <- clim_data[[i]][start_row:final_row, ]
      clim_data[[i]] <- clim_data[[i]][clim_data[[i]]$month %in% month_mask, ]
      
      rownames(clim_data[[i]]) <- 1:nrow(clim_data[[i]])
    }
  }
  
  return(list(climate = clim_data, biovars = biovars))
}


#' Calculate the Bioclimatic Variables
#' 
#' @description
#' This function calculates the 19 standard bioclimatic variables derived from 
#' monthly temperature and precipitation data. Bioclimatic variables are often 
#' used in ecological modeling and species distribution modeling to capture biologically 
#' meaningful patterns in climate data, including annual trends, seasonality, and 
#' extreme environmental factors. 
#' 
#' The bioclimatic variables represent metrics such as the annual mean temperature, 
#' temperature seasonality, and precipitation patterns (e.g., wettest or driest quarter).
#' These metrics help to model species distributions and analyze ecological dynamics.
#'
#' The bioclimatic variables are coded as follows:
#' \itemize{
#'   \item \strong{BIO1} = Annual Mean Temperature
#'   \item \strong{BIO2} = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#'   \item \strong{BIO3} = Isothermality (BIO2/BIO7) (* 100)
#'   \item \strong{BIO4} = Temperature Seasonality (standard deviation * 100)
#'   \item \strong{BIO5} = Max Temperature of Warmest Month
#'   \item \strong{BIO6} = Min Temperature of Coldest Month
#'   \item \strong{BIO7} = Temperature Annual Range (BIO5 - BIO6)
#'   \item \strong{BIO8} = Mean Temperature of Wettest Quarter
#'   \item \strong{BIO9} = Mean Temperature of Driest Quarter
#'   \item \strong{BIO10} = Mean Temperature of Warmest Quarter
#'   \item \strong{BIO11} = Mean Temperature of Coldest Quarter
#'   \item \strong{BIO12} = Annual Precipitation
#'   \item \strong{BIO13} = Precipitation of Wettest Month
#'   \item \strong{BIO14} = Precipitation of Driest Month
#'   \item \strong{BIO15} = Precipitation Seasonality (Coefficient of Variation)
#'   \item \strong{BIO16} = Precipitation of Wettest Quarter
#'   \item \strong{BIO17} = Precipitation of Driest Quarter
#'   \item \strong{BIO18} = Precipitation of Warmest Quarter
#'   \item \strong{BIO19} = Precipitation of Coldest Quarter
#' }
#' 
#' These variables are computed using temperature and precipitation data in a standard format, 
#' and are critical for understanding species habitats and the effects of climate on ecosystems.
#' 
#' This work is derived from the \href{https://github.com/rspatial/dismo/blob/master/R/biovars.R}{dismo R package}.
#' 
#' @references 
#' Nix, 1986. A biogeographic analysis of Australian elapid snakes. In: R. Longmore (ed.). 
#' Atlas of elapid snakes of Australia. Australian Flora and Fauna Series 7. 
#' Australian Government Publishing Service, Canberra.
#'
#' @author
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @author
#' Robert Hijmans, Museum of Vertebrate Zoology, UC Berkeley
#' 
#' @param data A data frame containing monthly climate data. The data frame must include:
#' \itemize{
#'   \item \strong{year:} The year for each set of monthly data.
#'   \item \strong{ppt:} Monthly precipitation values (in mm).
#'   \item \strong{tmin:} Monthly minimum temperature values (in degrees Celsius).
#'   \item \strong{tmax:} Monthly maximum temperature values (in degrees Celsius).
#' }
#' The data should contain 12 rows (one for each month from January to December) per year, 
#' with the columns sorted in the order of year, ppt, tmin, and tmax.
#' 
#' @return 
#' A data frame with 19 columns representing the bioclimatic variables (BIO1 to BIO19) and 
#' an additional column for the year. The output data frame provides one row per year, with 
#' each column corresponding to one of the bioclimatic variables described above.
#' 
#' @export

calc_biovars <- function(data) {
  year <- unique(data$year)
  
  if (length(which(table(data$year) != 12)) != 0) { 
    stop(paste('You have to have 12 rows (months) for each year in your dataset.',  
               'That is not the case for:', paste(attributes(which(table(data$year) != 12))$names, collapse = ', ')))
  }
  
  required_vars <- c('ppt', 'tmin', 'tmax')
  var_not_found <- required_vars[!(required_vars %in% colnames(data))]
  
  if(length(var_not_found) != 0) {
    stop(paste('Missing required column(s) in your dataset:', paste(var_not_found, collapse = ', ')))
  }
  
  prec <- matrix(data$ppt,  ncol = 12, byrow = TRUE)
  tmin <- matrix(data$tmin, ncol = 12, byrow = TRUE)
  tmax <- matrix(data$tmax, ncol = 12, byrow = TRUE)
  
  # can't have missing values in a row
  nas <- apply(prec, 1, function(x){ any(is.na(x)) } )
  nas <- nas | apply(tmin, 1, function(x){ any(is.na(x)) } )
  nas <- nas | apply(tmax, 1, function(x){ any(is.na(x)) } )
  
  p <- matrix(nrow = nrow(prec), ncol = 20)
  colnames(p) = c(paste0('bio', 1:19), 'year')
  
  if (all(nas)) { return(p) }
  
  prec[nas,] <- NA
  tmin[nas,] <- NA
  tmax[nas,] <- NA
  
  window <- function(x) { 
    lng <- length(x)
    x <- c(x,  x[1:3])
    m <- matrix(ncol = 3, nrow = lng)
    for (i in 1:3) { m[,i] <- x[i:(lng+i-1)] }
    apply(m, MARGIN = 1, FUN = sum)
  }
  
  cv <- function(x) {
    return(stats::sd(x) / mean(x) * 100)
  }
  
  tavg <- (tmin + tmax) / 2
  
  # P1. Annual Mean Temperature 
  p[, 1] <- apply(tavg, 1, mean)
  
  # P2. Mean Diurnal Range(Mean(period max-min)) 
  p[, 2] <- apply(tmax - tmin, 1, mean)
  
  # P4. Temperature Seasonality (standard deviation) 
  p[,4] <- 100 * apply(tavg, 1, stats::sd)
  
  # P5. Max Temperature of Warmest Period 
  p[, 5] <- apply(tmax, 1, max)
  
  # P6. Min Temperature of Coldest Period 
  p[, 6] <- apply(tmin, 1, min)
  
  # P7. Temperature Annual Range (P5-P6) 
  p[, 7] <- p[, 5] - p[, 6]
  
  # P3. Isothermality (P2 / P7) 
  p[, 3] <- 100 * p[, 2] / p[, 7]
  
  # P12. Annual Precipitation 
  p[, 12] <- apply(prec, 1, sum)
  
  # P13. Precipitation of Wettest Period 
  p[, 13] <-  apply(prec, 1, max)
  
  # P14. Precipitation of Driest Period 
  p[, 14] <-  apply(prec, 1, min)
  
  # P15. Precipitation Seasonality(Coefficient of Variation) 
  # the "1 +" is to avoid strange CVs for areas where mean rainfall is < 1)
  p[, 15] <- apply(prec+1, 1, cv)
  
  # precipitation by quarter (3 months)		
  wet <- t(apply(prec, 1, window))
  
  # P16. Precipitation of Wettest Quarter 
  p[, 16] <- apply(wet, 1, max)
  
  # P17. Precipitation of Driest Quarter 
  p[, 17] <- apply(wet, 1, min)
  
  tmp <- t(apply(tavg, 1, window)) / 3
  
  if (all(is.na(wet))) {
    p[, 8] <- NA		
    p[, 9] <- NA		
  } else {
    # P8. Mean Temperature of Wettest Quarter 
    wetqrt <- cbind(1:nrow(p), as.integer(apply(wet, 1, which.max)))
    p[, 8] <- tmp[wetqrt]
    
    # P9. Mean Temperature of Driest Quarter 
    dryqrt <- cbind(1:nrow(p), as.integer(apply(wet, 1, which.min)))
    p[, 9] <- tmp[dryqrt]
  }
  
  # P10. Mean Temperature of Warmest Quarter 
  p[, 10] <- apply(tmp, 1, max)
  
  # P11. Mean Temperature of Coldest Quarter
  p[, 11] <- apply(tmp, 1, min) 
  
  if (all(is.na(tmp))) {
    p[, 18] <- NA		
    p[, 19] <- NA
  } else {
    # P18. Precipitation of Warmest Quarter 
    hot <- cbind(1:nrow(p), as.integer(apply(tmp, 1, which.max)))
    p[, 18] <- wet[hot]
    
    # P19. Precipitation of Coldest Quarter 
    cold <- cbind(1:nrow(p), as.integer(apply(tmp, 1, which.min)))
    p[, 19] <- wet[cold]
  }
  
  p[, 20] <- year
  
  return(as.data.frame(p))
}


#' Download TerraClimate netCDF Data Files to Extract their Data Offline
#'
#' @description
#' This function facilitates the download of TerraClimate netCDF files for a specified 
#' time period and climate variables. TerraClimate data provides monthly climate data 
#' for global terrestrial surfaces, and this function allows you to store the data locally 
#' for offline extraction and analysis without the need to download the entire dataset.
#' 
#' Users can specify a range of climate variables such as precipitation, temperature, 
#' evapotranspiration, soil moisture, and more. The downloaded files are saved in 
#' netCDF format, making them accessible for subsequent offline analysis.
#' 
#' @param from Start date as a string in the 'YYYY-MM-DD' format. This defines the beginning 
#'             of the time period for which you want to download the climate data.
#' @param to End date as a string in the 'YYYY-MM-DD' format. This defines the end of the time 
#'           period for which you want to download the climate data.
#' @param clim_vars A list of climate variables to download. Valid options include:
#' \emph{aet} (Actual Evapotranspiration), \emph{def} (Climate Water Deficit), \emph{pet} (Potential 
#' Evapotranspiration), \emph{ppt} (Precipitation), \emph{q} (Runoff), \emph{soil} (Soil Moisture), 
#' \emph{srad} (Solar Radiation), \emph{swe} (Snow Water Equivalent), \emph{tmax} (Maximum Temperature), 
#' \emph{tmin} (Minimum Temperature), \emph{vap} (Vapor Pressure), \emph{ws} (Wind Speed), 
#' \emph{vpd} (Vapor Pressure Deficit), and \emph{PDSI} (Palmer Drought Severity Index).
#' If NULL (default), all available variables will be downloaded.
#' @param data_path A string containing the directory path where the downloaded netCDF files 
#'                  will be stored. The default path is './data/'.
#' @param timeout Timeout in seconds for downloading each netCDF raster file. The default value is 300 seconds.
#' 
#' @return 
#' No explicit return value. The downloaded netCDF files are saved to the specified directory for 
#' offline use with the \code{\link{get_terraclimate}} function to extract data for a given 
#' coordinate or set of coordinates.
#'  
#' @author 
#' Khaled Al-Shamaa (\email{k.el-shamaa@cgiar.org})
#' 
#' @seealso 
#' \code{\link{get_terraclimate}}
#' 
#' @examples
#' if (interactive()) {
#'   # Initialize TerraClimate data download for specific climate variables between two dates
#'   ini_terraclimate('2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'))
#'   
#'   # Coordinates for the location(s) of interest
#'   x <- c(-6.716, 35.917, 76.884)
#'   y <- c(33.616, 33.833, 23.111)
#'   
#'   # Extract TerraClimate data for the specified coordinates (online mode)
#'   a <- get_terraclimate(y, x, '2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'))
#'   
#'   # View the extracted climate data and bioclimatic variables
#'   View(a$climate[[1]])
#'   View(a$biovars[[1]])
#'   
#'   # Extract TerraClimate data for the specified coordinates (offline mode)
#'   b <- get_terraclimate(y, x, '2018-09-01', '2019-06-30', c('ppt', 'tmin', 'tmax'), offline = TRUE)
#'   
#'   # View the offline-extracted data
#'   View(b$climate[[1]])
#'   View(b$biovars[[1]])
#' }
#' 
#' @export

ini_terraclimate <- function(from = '2018-09-01', to = '2019-06-30', clim_vars = c('ppt', 'tmin', 'tmax'), data_path = './data/', timeout = 300) {
  options(timeout = timeout)
  
  download_url <- 'https://climate.northwestknowledge.net/TERRACLIMATE-DATA/'
  
  start_year <- as.numeric(format(as.Date(from), '%Y'))
  final_year <- as.numeric(format(as.Date(to), '%Y'))
  
  if (!dir.exists(data_path)) dir.create(data_path)
  
  for (var in clim_vars) {
    for (year in start_year:final_year) {
      file_path <- paste0(data_path, 'TerraClimate_', var, '_', year, '.nc')
      file_url  <- paste0(download_url, 'TerraClimate_', var, '_', year, '.nc')
      
      if (!file.exists(file_path)) {
        utils::download.file(file_url, file_path, mode = 'wb', method = 'libcurl')
      }
    }
  }
}
