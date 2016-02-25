#' Download maps for countries included in the database
#'
#' \code{download_maps} fetch maps from \url{http://www.gadm.org/}.
#'
#' This function fetch maps from \url{http://www.gadm.org/} if the map it's not
#' already present in the maps folder (by default, the working directory).
#'
#' @family Quality Check Functions
#'
#' @param data Data frame where the countries ISO codes are. Must contain a
#'   variable called \code{country} where the ISO code resides.
#'
#' @param folder Folder route where the maps are stored or where they will be
#'   stored, by default the working directory. It must be a character object and
#'   it must end without \code{/}.
#'
#' @return Maps are downloaded (if needed) and a summary is returned indicating
#'   number of maps downloaded and number of maps present in the map folder.

# START
# function declaration

download_maps <- function(data, folder = getwd()) {

  # STEP 0
  # Argument checks
  #   if data is a data.frame
  if (!is.data.frame(data)) {
    stop('Provided data object is not a data.frame.\n
         Please check if it is the correct object\n')
  }
  #   if data contains a country variable
  if (is.null(data$country)) {
    stop('There is no country variable in this dataset\n')
  }
  #   if folder exists and is accesible
  if (!file_test("-d", folder)) {
    stop('Destination folder does not exist.\n
         Please create destination folder before using this function\n')
  }

  # STEP 0.a
  # Initialise maps count and download count
  existent_maps <- length(list.files(folder, pattern = '.rds'))
  downloaded_maps <- 0

  # STEP 1
  # Begin for loop, and check if country code is NA, and if it is, don't do
  # anything with that value
  for (code in data$country) {
    if (!is.na(code)) {

      # STEP 2
      # Create file name
      file_name <- paste(code, '_adm0.rds', sep = '')

      # STEP 3
      # Check if file exists, and if it exists, don't download the map
      if (!file_name %in% list.files(path = folder, pattern = '.rds')) {

        # STEP 4
        # Create url name
        url_name <- paste('http://biogeo.ucdavis.edu/data/gadm2.8/rds/',
                          file_name,
                          sep = '')

        # STEP 5
        # Dowload file (In case of download error, indicate it and
        # try to skip to the next country)
        possibleError <- tryCatch({
          download.file(url_name,
                        paste(folder, '/', file_name, sep = ''),
                        cacheOK = FALSE)
          # STEP 5.a
          # Update downloaded maps count
          downloaded_maps <- downloaded_maps + 1
        },
        error = function(e) {
          message('Download for ', file_name,
                  ' failed, check if ISO code are correct and/or if
                  network connection is active
                  An empty file has been created with the bad iso code.')
        },
        warning = function(e) {
          message('Download for ', file_name,
                  ' failed, check if ISO code are correct and/or if
                  network connection is active
                  An empty file has been created with the bad iso code.')
        }
        )

        if (inherits(possibleError, "error")) {
          next
        }
      }
    }
  }

  # STEP 6
  # Return a summary of downloaded maps and existent maps
  message(existent_maps, ' maps already downloaded and saved in ', folder)
  message(downloaded_maps, ' new maps downloaded')
  message(length(list.files(folder, pattern = '.rds')) - (existent_maps + downloaded_maps),
          ' empty maps created due to download error')
  message(length(list.files(folder, pattern = '.rds')),
          ' maps now in ', folder)

  # END function
}

################################################################################

#' Site coordinates checking
#'
#' \code{check_coordinates} verifies if provided coordinates are within
#' country declared in metadata form.
#'
#' This function uses maps previously downloaded with
#' \code{\link{download_maps}} to check if the provided coordinates are within
#' the country limits. It only creates a data frame containing country, site
#' and a logical variable indicating if coordinates are correct.
#'
#' @family Quality Check Functions
#'
#' @param data Data frame with data. At least, longitude, latitude, country and
#'   site name variables must be present in the data object.
#'
#' @param maps_folder Folder route where the maps are stored, by default the
#'   working directory. It must be a character object and it must end
#'   without \code{/}.
#'
#' @param plot Logical indicating if plots for coordinate are created and saved
#'   in the working directory. By default, plot are not saved.
#'
#' @param text_report Logical indicating if a text report is showed in the
#'   console after checking coordinates. By default, a report is showed in the
#'   console.
#'
#' @return A data frame containing country, site name and is_inside_country
#'   logical variable indicating those sites with wrong coordinates.
#'
#' @import ggplot2


# START
# Function declaration

check_coordinates <- function(data, maps_folder,
                              plot = FALSE, text_report = TRUE){

  # STEP 0
  # Argument checks
  #   if data is a data.frame
  if (!is.data.frame(data)) {
    stop('Provided data object is not a data.frame.\n
         Please check if it is the correct object\n')
  }
  #   if data contains a longitude variable
  if (is.null(data$longitude)) {
    stop('There is no longitude variable in this dataset\n')
  }
  #   if data contains a latitude variable
  if (is.null(data$latitude)) {
    stop('There is no latitude variable in this dataset\n')
  }
  #   if data contains a country variable
  if (is.null(data$country)) {
    stop('There is no country variable in this dataset\n')
  }
  #   if data contains a site_name variable
  if (is.null(data$site_name)) {
    stop('There is no site_name variable in this dataset\n')
  }
  #   if folder exists and is accesible
  if (!file_test("-d", folder)) {
    stop('Destination folder does not exist.\n
         Please create destination folder before using this function\n')
  }

  # STEP 1
  # Initialise results object
  results <- data.frame(
    longitude = double(),
    latitude = double(),
    country = character(0),
    site_name = character(0),
    is_inside_country = logical(0),
    stringsAsFactors = FALSE
  )

  # STEP 2
  # Begin the for loop and read the map file
  for (i in 1:length(data[,1])) {
    map_data <- readRDS(paste(folder, '/', data$country, '_adm0.rds', sep = ''))

    # STEP 3
    # Get coordinates and transform them in SpatialPoints object
    sp_points <- sp::SpatialPoints(
      data[i, c('longitude', 'latitude')],
      proj4string = sp::CRS(sp::proj4string(map_data))
    )

    # STEP 4
    # Update results object, including the output of rgeos::gContains
    res_tmp <- data.frame(
      longitude = data$longitude[i],
      latitude = data$latitude[i],
      country = data$country[i],
      site_name = site_name[i],
      is_inside_country = rgeos::gContains(map_data, sp_points),
      stringsAsFactors = FALSE
    )

    results <- rbind(results, res_tmp)

    # STEP 5
    # Create and saving the plot if plot = TRUE and is_inside_country = FALSE

    if (plot && !res_tmp$is_inside_country[1]) {
      # 5.1 map data in adequate format to be able to plot
      plot_data <- broom::tidy(map_data)
      # 5.2 ggplot2 object
      plot_map <- ggplot(plot_data, aes(x = long, y = lat)) +
        geom_polygon(aes(group = group)) +
        geom_point(aes(x = longitude, y = latitude),
                   data = results[i,], size = 2, color = 'red', alpha = 0.7) +
        coord_map() +
        labs(title = paste(results[i, c('country')],
                           results[i, c('site_name')], sep = ' - '))
      # 5.3 see plot
      print(plot_map)
      # 5.4 save plot in working directory
      ggsave(filename = paste(results[i, c('country')],
                              results[i, c('site_name')], 'pdf', sep = '.'),
             plot = plot_map, width = 6, height = 4, units = 'cm')
    }
  }

  # STEP 6
  # Create a console report with message if text_report is TRUE

  if (text_report) {

    # 6.1 Sum of wrong, correct and total coordinates checked
    wrong_coordinates <- sum(!results$is_inside_country, na.rm = TRUE)
    correct_coordinates <- sum(results$is_inside_country, na.rm = TRUE)
    total_coordinates <- wrong_coordinates + correct_coordinates

    # 6.2 messages
    message(wrong_coordinates, ' wrong coordinates in data')
    message(correct_coordinates, ' correct coordinates in data')
    message(total_coordinates, ' coordinates checked')
  }
}
