#' Download maps for countries included in the database
#'
#' \code{download_maps} fetch maps from \url{http://www.gadm.org/}.
#'
#' This function fetch maps from \url{http://www.gadm.org/} if the map is not
#' already present in the maps folder (by default, the working directory).
#'
#' @family Quality Check Functions
#'
#' @param data Data frame where the countries ISO codes are. Must contain a
#'   variable called \code{country} where the ISO code resides.
#'
#' @param folder Folder route where the maps are stored or where they will be
#'   stored, by default the working directory. It must be a character object and
#'   it must end \bold{without} \code{/}.
#'
#' @return Maps are downloaded (if needed) and a summary is returned indicating
#'   number of maps downloaded and number of maps present in the map folder.
#'
#' @export

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
                        file.path(folder, file_name),
                        cacheOK = FALSE, quiet = TRUE)
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
#'   \bold{without} \code{/}.
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
#'
#' @export


# START
# Function declaration

check_coordinates <- function(data, maps_folder = getwd(),
                              plot = FALSE, text_report = TRUE){

  # STEP 0
  # Argument checks
  #   if data is a data.frame
  if (!is.data.frame(data)) {
    stop('Provided data object is not a data.frame.\n
         Please verify if it is the correct object\n')
  }
  #   if data contains a longitude variable
  if (is.null(data$longitude)) {
    stop('There is no longitude variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a latitude variable
  if (is.null(data$latitude)) {
    stop('There is no latitude variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a country variable
  if (is.null(data$country)) {
    stop('There is no country variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a site_name variable
  if (is.null(data$site_name)) {
    stop('There is no site_name variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if folder exists and is accesible
  if (!file_test("-d", maps_folder)) {
    stop('Maps folder does not exist, please verify the folder provided')
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

    file_name <- paste(data$country[i], '_adm0.rds', sep = '')
    map_data <- readRDS(file.path(maps_folder, file_name))

    # 2.1 message to indicate status of loop, to avoid confussion if it takes
    #     a long time
    message('Checking ', data$country[i], '-', data$site_name[i])


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
      site_name = data$site_name[i],
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
      ggsave(filename = paste(results[i, c('country')], '_',
                              results[i, c('site_name')], '.pdf', sep = ''),
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

  return(results)

# END FUNCTION
}

################################################################################

#' Coordinates sign test
#'
#' \code{coord_sign_test} is an internal function to test if site coordinates
#' signs are interchanged. It's needed by \code{\link{fix_latlong_errors}}
#' function.
#'
#' Country coordinates sign is established by this function and testing if
#' provided site coordinates are correct is made
#'
#' @family Quality Check Functions
#'
#' @param data Data frame with data coming from \code{\link{check_coordinates}}
#'   (with latitude, longitude, country and is_inside_country variables).
#'
#' @param maps_folder Folder route where the maps are stored, by default the
#'   working directory. It must be a character object and it must end
#'   \bold{without} \code{/}.
#'
#' @return Same data frame provided, with a two new columns, \code{lat_changed}
#'   and \code{long_changed}, two logicals indicating if the coordinates are
#'   sign exchanged
#'
#' @export

# START
# Function declaration

coord_sign_test <- function(data, maps_folder = getwd()) {

  # STEP 0
  # Arguments check
  #   if data is a data.frame
  if (!is.data.frame(data)) {
    stop('Provided data object is not a data.frame.\n
         Please verify if it is the correct object\n')
  }
  #   if data contains a longitude variable
  if (is.null(data$longitude)) {
    stop('There is no longitude variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a latitude variable
  if (is.null(data$latitude)) {
    stop('There is no latitude variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a country variable
  if (is.null(data$country)) {
    stop('There is no country variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a is_inside_country variable
  if (is.null(data$is_inside_country)) {
    stop('There is no is_inside_country variable in this dataset\n
         Please verify if it is the correct data\n')
  }

  # STEP 1
  # Initialise result vectors, and start for loop
  lat_changed <- logical()
  long_changed <- logical()

  for (i in 1:length(data[,1])) {

    # STEP 2
    # Check if is_inside_country is FALSE, and if it is, read the map data
    if (!data$is_inside_country[i]) {
      file_name <- paste(data$country[i], '_adm0.rds', sep = '')

      # 2.1 map data is read and transformed to tidy format, to easy check signs
      country_map <- broom::tidy(readRDS(file.path(maps_folder, file_name)))

      # STEP 3
      # Establish the main sign of country latitude and longitude

      # 3.1 latitude
      if (all(country_map$lat < 0)) {
        country_lat <- 'negative'
      } else {
        if (all(country_map$lat >= 0)) {
          country_lat <- 'positive'
        } else {
          country_lat <- 'mixed'
        }
      }

      # 3.2 longitude
      if (all(country_map$long < 0)) {
        country_long <- 'negative'
      } else {
        if (all(country_map$long >= 0)) {
          country_long <- 'positive'
        } else {
          country_long <- 'mixed'
        }
      }

      # STEP 4
      # Testing if provided coordinates are sign exchanged

      # 4.1 latitude
      if ( data$latitude[i] < 0) {
        if (country_lat == 'positive') {
          lat_changed <- c(lat_changed, TRUE)
        }
        if (country_lat == 'negative') {
          lat_changed <- c(lat_changed, FALSE)
        }
        if (country_lat == 'mixed') {
          lat_changed <- c(lat_changed, NA)
        }
      } else {
        if (country_lat == 'positive') {
          lat_changed <- c(lat_changed, FALSE)
        }
        if (country_lat == 'negative') {
          lat_changed <- c(lat_changed, TRUE)
        }
        if (country_lat == 'mixed') {
          lat_changed <- c(lat_changed, NA)
        }
      }

      # 4.2 longitude
      if ( data$longitude[i] < 0) {
        if (country_long == 'positive') {
          long_changed <- c(long_changed, TRUE)
        }
        if (country_long == 'negative') {
          long_changed <- c(long_changed, FALSE)
        }
        if (country_long == 'mixed') {
          long_changed <- c(long_changed, NA)
        }
      } else {
        if (country_long == 'positive') {
          long_changed <- c(long_changed, FALSE)
        }
        if (country_long == 'negative') {
          long_changed <- c(long_changed, TRUE)
        }
        if (country_long == 'mixed') {
          long_changed <- c(long_changed, NA)
        }
      }
    } else {

      # STEP 5
      # If is_inside_country is not FALSE, coordinates are ok
      lat_changed <- c(lat_changed, FALSE)
      long_changed <- c(long_changed, FALSE)
    }
  }

  # STEP 6
  # Create the returned data frame
  res_data <- cbind(data, lat_changed, long_changed)

  # STEP 7
  # Return the results
  return(res_data)

# END FUNCTION
}


################################################################################

#' Fixing sign errors in coordinates
#'
#' \code{fix_latlong_errors} makes possible to fix known errors in latitude and
#' longitude coordinates, as exchanged signs.
#'
#' This function calls to other internal functions in order to fix different
#' kinds of coordinates errors. At the moment, only exchanged signs in
#' coordinates errors are allowed to be fixed.
#' If \code{sign_errors = TRUE} is specified, \code{\link{coord_sign_test}} is
#' called to establish possible sign error and, if any, they are fixed.
#'
#' @family Quality Check Functions
#'
#' @param data Data frame with data coming from \code{\link{check_coordinates}}
#'   (with latitude, longitude, country and is_inside_country variables).
#'
#' @param maps_folder Folder route where the maps are stored, by default the
#'   working directory. It must be a character object and it must end
#'   \bold{without} \code{/}.
#'
#' @param sign_errors Logical indicating if sign errors must be checked and
#'   fixed. If TRUE (default), \code{\link{coord_sign_test}} is internally
#'   called.
#'
#' @return Same data frame provided, with coordinates tested and fixed
#'
#' @export

# START
# Function declaration
fix_latlong_errors <- function(data, maps_folder, sign_errors = TRUE) {

  # STEP 0
  # Argument checks
  #   if data is a data.frame
  if (!is.data.frame(data)) {
    stop('Provided data object is not a data.frame.\n
         Please verify if it is the correct object\n')
  }
  #   if data contains a longitude variable
  if (is.null(data$longitude)) {
    stop('There is no longitude variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a latitude variable
  if (is.null(data$latitude)) {
    stop('There is no latitude variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a country variable
  if (is.null(data$country)) {
    stop('There is no country variable in this dataset\n
         Please verify if it is the correct data\n')
  }
  #   if data contains a is_inside_country variable
  if (is.null(data$is_inside_country)) {
    stop('There is no is_inside_country variable in this dataset\n
         Please verify if it is the correct data\n')
  }

  # STEP 1
  # Initialising results objects
  results <- data

  # STEP 2
  # Fixing sign errors if sign_errors = TRUE
  if(sign_errors) {

    # 2.1 Are signs interchanged?
    sign_test_data <- coord_sign_test(data, maps_folder)

    # 2.2 Fix them if they are (multiply by -1)
    # latitude
    results$latitude[which(sign_test_data$lat_changed == TRUE)] <-
      results$latitude[which(sign_test_data$lat_changed == TRUE)] * (-1)
    # longitude
    results$longitude[which(sign_test_data$long_changed == TRUE)] <-
      results$longitude[which(sign_test_data$long_changed == TRUE)] * (-1)

    # 2.3 Console output indicating fixes and no-fixes
    message(sum(sign_test_data$lat_changed == TRUE, na.rm = TRUE),
            ' latitude sign errors fixed.\n',
            sum(sign_test_data$long_changed == TRUE, na.rm = TRUE),
            ' longitude sign errors fixed.\n',
            sum(is.na(sign_test_data$lat_changed)) +
              sum(is.na(sign_test_data$long_changed)),
            ' unable to fix due to country borders sharing positive and negative coordinates.\n')
  }

  # STEP 3 to STEP n
  # Here will appear other functions to fix other kind of coordinates errors

  # FINAL STEP
  # Returning the results

  return(results)

# END FUNCTION
}
