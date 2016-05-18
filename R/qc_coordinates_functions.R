################################################################################
#' Download maps for countries included in the database
#'
#' \code{qc_download_maps} fetch maps from \url{http://www.gadm.org/}.
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

qc_download_maps <- function(data, folder = getwd(), parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    #   if data is a data.frame
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data.frame. ',
           'Please check if it is the correct object')
    }
    #   if data contains a country variable
    if (is.null(data$si_country)) {
      stop('There is no country variable in this dataset')
    }
    #   if folder exists and is accesible
    if (!file_test("-d", folder)) {
      stop('Destination folder does not exist. ',
           'Please create destination folder before using this function')
    }

    # STEP 0.a
    # Initialise maps count and download count
    existent_maps <- length(list.files(folder, pattern = '.rds'))
    downloaded_maps <- 0

    # STEP 1
    # Begin for loop, and check if country code is NA, and if it is, don't do
    # anything with that value
    for (code in data$si_country) {
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
                    ' failed, check if ISO code are correct and/or if',
                    ' network connection is active. ',
                    'An empty file has been created with the bad iso code.')
          },
          warning = function(e) {
            message('Download for ', file_name,
                    ' failed, check if ISO code are correct and/or if ',
                    'network connection is active. ',
                    'An empty file has been created with the bad iso code.')
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
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_download_maps', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_download_maps', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_download_maps', sep = '.'))})

}

################################################################################
#' Site coordinates checking
#'
#' \code{qc_check_coordinates} verifies if provided coordinates are within
#' country declared in metadata form.
#'
#' This function internally download the maps with
#' \code{\link{qc_download_maps}} and it checks if the provided coordinates are
#' within the country limits. It only creates a data frame containing country,
#' site and a logical variable indicating if coordinates are correct.
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
#' @return The data frame used as input with a new variable, is_inside_country,
#'   a logical variable indicating if the site has wrong coordinates
#'
#' @import ggplot2
#'
#' @export


# START
# Function declaration

qc_check_coordinates <- function(data, maps_folder = getwd(),
                                 plot = FALSE, text_report = TRUE,
                                 parent_logger = 'test'){

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    #   if data is a data.frame
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data.frame.',
           ' Please verify if it is the correct object')
    }
    #   if data contains a longitude variable
    if (is.null(data$si_long)) {
      stop('There is no longitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a latitude variable
    if (is.null(data$si_lat)) {
      stop('There is no latitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a country variable
    if (is.null(data$si_country)) {
      stop('There is no country variable in this dataset.',
           ' Please verify if it is the correct data\n')
    }
    #   if data contains a site_name variable
    if (is.null(data$si_name)) {
      stop('There is no site_name variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if folder exists and is accesible
    if (!file_test("-d", maps_folder)) {
      stop('Maps folder does not exist, please verify the folder provided')
    }

    # STEP 1
    # Downlaod maps, if already not downloaded
    qc_download_maps(data = data, folder = maps_folder,
                     parent_logger = parent_logger)

    # STEP 2
    # Initialise results object
    results <- vector()

    # STEP 3
    # Begin the for loop and read the map file
    for (i in 1:length(data[,1])) {

      file_name <- paste(data$si_country[i], '_adm0.rds', sep = '')
      map_data <- readRDS(file.path(maps_folder, file_name))

      # 2.1 message to indicate status of loop, to avoid confussion if it takes
      #     a long time
      message('Checking ', data$si_country[i], '-', data$si_name[i])


      # STEP 3
      # Get coordinates and transform them in SpatialPoints object
      sp_points <- sp::SpatialPoints(
        data[i, c('si_long', 'si_lat')],
        proj4string = sp::CRS(sp::proj4string(map_data))
      )

      # STEP 4
      # Update results object, including the output of rgeos::gContains
      res_tmp <- rgeos::gContains(map_data, sp_points)

      results <- c(results, res_tmp)

      # STEP 5
      # Create and saving the plot if plot = TRUE and is_inside_country = FALSE

      if (plot && !res_tmp) {
        # 5.1 map data in adequate format to be able to plot
        plot_data <- broom::tidy(map_data)
        # 5.2 ggplot2 object
        plot_map <- ggplot(plot_data, aes(x = long, y = lat)) +
          geom_polygon(aes(group = group)) +
          geom_point(aes(x = si_long, y = si_lat),
                     data = data[i,], size = 2, color = 'red', alpha = 0.7) +
          coord_map() +
          labs(title = paste(data[i, c('si_country')],
                             data[i, c('si_name')], sep = ' - '))
        # 5.3 see plot
        print(plot_map)
        # 5.4 save plot in working directory
        ggsave(filename = paste(data[i, c('si_country')], '_',
                                data[i, c('si_name')], '.pdf', sep = ''),
               plot = plot_map, width = 6, height = 4, units = 'cm')
      }
    }

    # STEP 6
    # Create a console report with message if text_report is TRUE

    if (text_report) {

      # 6.1 Sum of wrong, correct and total coordinates checked
      wrong_coordinates <- sum(!results, na.rm = TRUE)
      correct_coordinates <- sum(results, na.rm = TRUE)
      total_coordinates <- wrong_coordinates + correct_coordinates

      # 6.2 messages
      message(wrong_coordinates, ' wrong coordinates in data')
      message(correct_coordinates, ' correct coordinates in data')
      message(total_coordinates, ' coordinates checked')
    }

    # STEP 7
    # Create a new variable in data with the results of the checks
    data$is_inside_country <- results

    # 7.1 Return data with the new variable
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_check_coordinates', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_check_coordinates', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_check_coordinates', sep = '.'))})

}

################################################################################
#' Coordinates sign test
#'
#' \code{qc_coord_sign_test} is an internal function to test if site coordinates
#' signs are interchanged. It's needed by \code{\link{qc_fix_latlong_errors}}
#' function.
#'
#' Country coordinates sign is established by this function and testing if
#' provided site coordinates are correct is made.
#'
#' @section Special countries:
#' There are special countries where border coordinates for longitude, latitude
#' or both have negative and positive values. In this case, the normal approach
#' of \code{qc_coord_sign_test} is not appropriate, and several tests involving
#' the internal use of \code{\link{qc_check_coordinates}} must be made. If
#' \code{special_countries = TRUE} is specified, then tests are made trying to
#' dilucidate if only a change in the sign of one, latitude or longitude, is
#' needed or, in the contrary, changing both of them is needed. There is one
#' case that can not be covered by this approach: when changing sign of one of
#' the coordinates \bold{AND} changing both coordinates seem to fix the issue,
#' as the correct option can not be assured.
#'
#' @family Quality Check Functions
#'
#' @param data Data frame with data coming from \code{\link{qc_check_coordinates}}
#'   (with latitude, longitude, country and is_inside_country variables).
#'
#' @param maps_folder Folder route where the maps are stored, by default the
#'   working directory. It must be a character object and it must end
#'   \bold{without} \code{/}.
#'
#' @param special_countries Logical indicating if the special approach to
#'   countries having positive and negative coordinates must be used. See
#'   \emph{Special countries} section for details.
#'
#' @return Same data frame provided, with a two new columns, \code{lat_changed}
#'   and \code{long_changed}, two logicals indicating if the coordinates are
#'   sign exchanged
#'
#' @export

# START
# Function declaration

qc_coord_sign_test <- function(data, maps_folder = getwd(),
                            special_countries = FALSE,
                            parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments check
    #   if data is a data.frame
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data.frame. ',
           'Please verify if it is the correct object')
    }
    #   if data contains a longitude variable
    if (is.null(data$si_long)) {
      stop('There is no longitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a latitude variable
    if (is.null(data$si_lat)) {
      stop('There is no latitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a country variable
    if (is.null(data$si_country)) {
      stop('There is no country variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a is_inside_country variable
    if (is.null(data$is_inside_country)) {
      stop('There is no is_inside_country variable in this dataset. ',
           ' Please verify if it is the correct data')
    }

    # STEP 1
    # Initialise result vectors, and start for loop
    lat_changed <- logical()
    long_changed <- logical()

    for (i in 1:length(data[,1])) {

      # STEP 2
      # Check if is_inside_country is FALSE, and if it is, read the map data
      if (!data$is_inside_country[i]) {
        file_name <- paste(data$si_country[i], '_adm0.rds', sep = '')

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
        if ( data$si_lat[i] < 0) {
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
        if ( data$si_long[i] < 0) {
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
    # Special countries approach
    if (special_countries) {

      # 7.1 Start for loop and check if lat_changed and/or long_changed is NA
      for (j in 1:length(res_data[,1])) {

        lat_na <- is.na(res_data$lat_changed[j])
        long_na <- is.na(res_data$long_changed[j])

        # 7.2 CASE 1 only one changed
        # 7.2.1 data frame
        if (lat_na && !long_na) {

          if (res_data$long_changed[j]) {
            check_data_lat <- data.frame(
              si_long = res_data$si_long[j] * -1,
              si_lat = res_data$si_lat[j] * -1,
              si_country = res_data$si_country[j],
              si_name = res_data$si_name[j]
            )
          } else {
            check_data_lat <- data.frame(
              si_long = res_data$si_long[j],
              si_lat = res_data$si_lat[j] * -1,
              si_country = res_data$si_country[j],
              si_name = res_data$si_name[j]
            )
          }

          # 7.2.2 check
          res_data$lat_changed[j] <- qc_check_coordinates(
            check_data_lat, maps_folder,
            plot = FALSE,
            text_report = FALSE)$is_inside_country[1]
        }

        if (!lat_na && long_na) {

          # 7.2.3 data frame
          if (res_data$lat_changed[j]) {
            check_data_long <- data.frame(
              si_long = res_data$si_long[j] * -1,
              si_lat = res_data$si_lat[j] * -1,
              si_country = res_data$si_country[j],
              si_name = res_data$si_name[j]
            )
          } else {
            check_data_long <- data.frame(
              si_long = res_data$si_long[j] * -1,
              si_lat = res_data$si_lat[j],
              si_country = res_data$si_country[j],
              si_name = res_data$si_name[j]
            )
          }

          # 7.2.4 check
          res_data$long_changed[j] <- qc_check_coordinates(
            check_data_long, maps_folder,
            plot = FALSE,
            text_report = FALSE)$is_inside_country[1]
        }

        # 7.3 CASE 2 Both border coordinates with positive and negative values

        if (lat_na && long_na) {

          # 7.3.1 data frames
          check_data_lat <- data.frame(
            si_long = res_data$si_long[j],
            si_lat = res_data$si_lat[j] * -1,
            si_country = res_data$si_country[j],
            si_name = res_data$si_name[j]
          )

          check_data_long <- data.frame(
            si_long = res_data$si_long[j] * -1,
            si_lat = res_data$si_lat[j],
            si_country = res_data$si_country[j],
            si_name = res_data$si_name[j]
          )

          check_data_both <- data.frame(
            si_long = res_data$si_long[j] * -1,
            si_lat = res_data$si_lat[j] * -1,
            si_country = res_data$si_country[j],
            si_name = res_data$si_name[j]
          )

          # 7.3.2 checks
          lat_check <- qc_check_coordinates(
            check_data_lat, maps_folder,
            plot = FALSE,
            text_report = FALSE)$is_inside_country[1]

          long_check <- qc_check_coordinates(
            check_data_long, maps_folder,
            plot = FALSE,
            text_report = FALSE)$is_inside_country[1]

          both_check <- qc_check_coordinates(
            check_data_both, maps_folder,
            plot = FALSE,
            text_report = FALSE)$is_inside_country[1]

          # 7.3.3 Changing both fix the problem
          if (both_check && (!lat_check && !long_check)) {
            res_data$lat_changed[j] <- TRUE
            res_data$long_changed[j] <- TRUE
          }

          # 7.3.4 Changing only latitude fix the problem
          if (lat_check && (!both_check && !long_check)) {
            res_data$lat_changed[j] <- TRUE
            res_data$long_changed[j] <- FALSE
          }

          # 7.3.5 Changing only longitude fix the problem
          if (long_check && (!both_check && !lat_check)) {
            res_data$lat_changed[j] <- FALSE
            res_data$long_changed[j] <- TRUE
          }

          # 7.3.6 Special case, when changing both coordinates fix, but also
          #       changing only latitude and/or longitude separately fix the
          #       issue. In this case, there is no solution as there is no
          #       certainty about the correct solution
          if ((both_check && (lat_check || long_check)) &&
              (both_check && (lat_check && long_check))) {
            message('No certainty about correct solution in ',
                    res_data$si_country[j], '-', res_data$si_name[j])
          }
        }
      }
    }

    # STEP 8
    # Return the results
    return(res_data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_coord_sign_test', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_coord_sign_test', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_coord_sign_test', sep = '.'))})

}

################################################################################
#' Fixing sign errors in coordinates
#'
#' \code{qc_fix_latlong_errors} makes possible to fix known errors in latitude and
#' longitude coordinates, as exchanged signs.
#'
#' This function calls to other internal functions in order to fix different
#' kinds of coordinates errors. At the moment, only exchanged signs in
#' coordinates errors are considered to be fixed.
#'
#' After the fixes are applied, \code{\link{qc_check_coordinates}} is called to
#' update \code{is_inside_country} variable.
#'
#' @section Sign errors:
#' If \code{sign_errors = TRUE} is specified, \code{\link{qc_coord_sign_test}} is
#' called to establish possible sign error and, if any, they are fixed. This fix
#' can be done with or without special countries (see next section)
#'
#' @section Special countries:
#' There are special countries where border coordinates for longitude, latitude
#' or both have negative and positive values. In this case, the normal approach
#' of \code{qc_coord_sign_test} is not appropriate, and several tests involving
#' the internal use of \code{\link{qc_check_coordinates}} must be made. If
#' \code{special_countries = TRUE} is specified, then tests are made trying to
#' dilucidate if only a change in the sign of one, latitude or longitude, is
#' needed or, in the contrary, changing both of them is needed. There is one
#' case that can not be covered by this approach: when changing sign of one of
#' the coordinates \bold{AND} changing both coordinates seem to fix the issue,
#' as the correct option can not be assured.
#'
#' @family Quality Check Functions
#'
#' @param data Data frame with data coming from \code{\link{qc_check_coordinates}}
#'   (with latitude, longitude, country and is_inside_country variables).
#'
#' @param maps_folder Folder route where the maps are stored, by default the
#'   working directory. It must be a character object and it must end
#'   \bold{without} \code{/}.
#'
#' @param sign_errors Logical indicating if sign errors must be checked and
#'   fixed. If TRUE (default), \code{\link{qc_coord_sign_test}} is internally
#'   called.
#'
#' @param special_countries Logical indicating if the special approach to
#'   countries having positive and negative coordinates must be used. See
#'   \emph{Special countries} section for details.
#'
#' @return Same data frame provided, with coordinates tested and fixed
#'
#' @export

# START
# Function declaration
qc_fix_latlong_errors <- function(data, maps_folder = getwd(),
                                  sign_errors = TRUE,
                                  special_countries = FALSE,
                                  parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    #   if data is a data.frame
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data.frame. ',
           'Please verify if it is the correct object')
    }
    #   if folder exists and is accesible
    if (!file_test("-d", maps_folder)) {
      stop('maps_folder location does not exist or is not accessible. ',
           'Please check provided folder name')
    }
    #   if data contains a longitude variable
    if (is.null(data$si_long)) {
      stop('There is no longitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a latitude variable
    if (is.null(data$si_lat)) {
      stop('There is no latitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a country variable
    if (is.null(data$si_country)) {
      stop('There is no country variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    #   if data contains a is_inside_country variable
    if (is.null(data$is_inside_country)) {
      stop('There is no is_inside_country variable in this dataset. ',
           'Please verify if it is the correct data')
    }

    # STEP 1
    # Initialising results objects
    results <- data

    # STEP 2
    # Fixing sign errors if sign_errors = TRUE
    if(sign_errors) {

      # 2.1 Are signs interchanged?
      sign_test_data <- qc_coord_sign_test(data, maps_folder,
                                           special_countries = special_countries)

      # 2.2 Fix them if they are (multiply by -1)
      # latitude
      results$si_lat[which(sign_test_data$lat_changed == TRUE)] <-
        results$si_lat[which(sign_test_data$lat_changed == TRUE)] * (-1)
      # longitude
      results$si_long[which(sign_test_data$long_changed == TRUE)] <-
        results$si_long[which(sign_test_data$long_changed == TRUE)] * (-1)

      # 2.3 Console output indicating fixes and no-fixes
      message(sum(sign_test_data$lat_changed == TRUE, na.rm = TRUE),
              ' latitude sign errors fixed. ',
              sum(sign_test_data$long_changed == TRUE, na.rm = TRUE),
              ' longitude sign errors fixed. ',
              sum(is.na(sign_test_data$lat_changed)) +
                sum(is.na(sign_test_data$long_changed)),
              ' unable to fix due to country borders sharing positive and negative coordinates.')
    }

    # STEP 3 to STEP n
    # Here will appear other functions to fix other kind of coordinates errors

    # n+1 STEP
    # Returning the results
    return(suppressMessages(
      qc_check_coordinates(results, maps_folder,
                           plot = FALSE, text_report = FALSE))
    )

    # END FUNCTION
  },

  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_fix_latlong_errors', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_fix_latlong_errors', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_fix_latlong_errors', sep = '.'))})

}

################################################################################
#' Commodity function to check site coordinates
#'
#' Wrap around \code{\link{qc_check_coordinates}} and \code{\link{qc_fix_latlong_errors}}
#' to process coordinates data in one step
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame with data coming from \code{\link{qc_check_coordinates}}
#'   (with latitude, longitude, country and is_inside_country variables).
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
#' @param sign_errors Logical indicating if sign errors must be checked and
#'   fixed. If TRUE (default), \code{\link{qc_coord_sign_test}} is internally
#'   called.
#'
#' @param special_countries Logical indicating if the special approach to
#'   countries having positive and negative coordinates must be used. See
#'   \code{\link{qc_fix_latlong_errors}} for details.
#'
#' @export

# START
# Function declaration
qc_coordinates <- function(data, maps_folder = getwd(), plot = FALSE,
                           text_report = TRUE, sign_errors = TRUE,
                           special_countries = TRUE,
                           parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checks
    # None, all are done in the internal functions

    # STEP 1
    # Check coordinates
    check_coord_data <- qc_check_coordinates(as.data.frame(data), maps_folder,
                                             parent_logger = parent_logger)

    # 1.1, check if it is correct
    if (check_coord_data$is_inside_country) {

      # if it is, return data with the is_inside_variable added
      message('Coordinates are correct')
      return(check_coord_data)

      # 1.2 if not, try to fixit
    } else {
      fixed_coord_data <- qc_fix_latlong_errors(check_coord_data, maps_folder,
                                                sign_errors, special_countries,
                                                parent_logger)

      # and return the fixed data with the is_inside_country variable
      return(fixed_coord_data)
    }

    # END FUNCTION
  },

  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_coordinates', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_coordinates', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_coordinates', sep = '.'))})


}
