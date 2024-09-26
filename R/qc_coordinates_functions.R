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
    existent_maps <- length(list.files(paste0(folder, "/gadm"), pattern = '.rds'))
    downloaded_maps <- 0
    errored_maps <- 0

    # STEP 1
    # Begin for loop, and check if country code is NA, and if it is, don't do
    # anything with that value
    for (code in data$si_country) {
      if (!is.na(code)) {

        # STEP 2
        # Create file name
        file_name <- paste("gadm41_", code, '_0_pk.rds', sep = '')

        # STEP 3
        # Check if file exists, and if it exists, don't download the map
        if (!file_name %in% list.files(path = paste0(folder, "/gadm"), pattern = '.rds')) {

          # STEP 4
          # Dowload file (In case of download error, indicate it and
          # try to skip to the next country)
          possibleError <- tryCatch({
            # STEP 5
            # Download the file
            geodata::gadm(code, level = 0, path = folder, resolution = 1)
          },
          error = function(e) {
            message('Download for ', file_name,
                    ' failed, check if ISO codes are correct and/or if',
                    ' network connection is active. ')
          },
          warning = function(e) {
            message('Download for ', file_name,
                    ' failed, check if ISO codes are correct and/or if ',
                    'network connection is active. ')
          }
            )

          if (is.null(possibleError)) {
            errored_maps <- errored_maps + 1
            next
          } else {
            downloaded_maps <- downloaded_maps + 1
          }
        }
    }
    }

    # STEP 6
    # Return a summary of downloaded maps and existent maps
    message(existent_maps, ' maps already downloaded and saved in ', folder)
    message(downloaded_maps, ' new maps downloaded')
    message(errored_maps, ' maps errors (not downloaded)')
    # message(length(list.files(paste0(folder, "/gadm"), pattern = '.rds')) - (existent_maps + downloaded_maps),
    #         ' empty maps created due to download error')
    message(length(list.files(paste0(folder, "/gadm"), pattern = '.rds')),
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
#'   a logical variable indicating if the site has wrong coordinates, and another
#'   new variable is_on_land, a logical variable with TRUE if the coordinates are
#'   on land and false if they are over water bodies.
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

    # Load high-resolution land polygons
    land <- ne_download(scale = "large", type = "land",
                        category = "physical", returnclass = "sf")

    # STEP 2
    # Initialise results object
    results <- vector()

    # STEP 3
    # Begin the for loop and read the map file
    for (i in 1:length(data[,1])) {
      file_name <- paste("gadm41_", data$si_country[i], '_0_pk.rds', sep = '')
      map_data <- readRDS(file.path(paste0(maps_folder, "/gadm"), file_name)) |>
        terra::unwrap()

      # 2.1 message to indicate status of loop, to avoid confussion if it takes
      #     a long time
      message('Checking ', data$si_country[i], '-', data$si_name[i])


      # STEP 3
      # Get coordinates and transform them in SpatialPoints object
      sp_points <- terra::vect(
        data[i, c('si_long', 'si_lat')], geom = c("si_long", "si_lat"),
        crs = "epsg:4326"
      )
      # sp_points <- sp::SpatialPoints(
      #   data[i, c('si_long', 'si_lat')],
      #   proj4string = sp::CRS(sp::proj4string(map_data))
      # )

      # STEP 4
      # Update results object, including the output of rgeos::gContains
      res_tmp <- terra::is.related(map_data, sp_points, "contains")
      # res_tmp <- rgeos::gContains(map_data, sp_points)

      results <- c(results, res_tmp)

      # Is over water?
      # Convert to sf object
      points_sf <- sf::st_as_sf(data[i, c('si_long', 'si_lat')],
                            coords = c("si_long", "si_lat"), crs = 4326)
      # Perform spatial join
      on_land <- st_join(points_sf, land, left = TRUE, join = st_within)

      # Add a new column indicating if the point is on land
      on_land$on_land <- !is.na(on_land$scalerank)  # scalerank is a field in land polygons



      # STEP 5
      # Create and saving the plot if plot = TRUE and is_inside_country = FALSE

      if (plot && !res_tmp) {
        # 5.1 ggplot2 object
        plot_map <- ggplot() +
          tidyterra::geom_spatvector(data = map_data) +
          tidyterra::geom_spatvector(data = sp_points, size = 2, color = "red", alpha = 0.7) +
          labs(title = paste(data[i, c('si_country')],
                             data[i, c('si_name')], sep = ' - '))
        # 5.2 see plot
        print(plot_map)
        # 5.3 save plot in working directory
        # ggsave(filename = paste(data[i, c('si_country')], '_',
        #                         data[i, c('si_name')], '.pdf', sep = ''),
        #        plot = plot_map, width = 6, height = 4, units = 'cm')
      }

      if (plot && !on_land$on_land) {
        # 6.1 Extract longitude and latitude
        lon <- data$si_long[i]
        lat <- data$si_lat[i]

        # 6.2 Define buffer size for zoom (in degrees). Adjust as needed.
        buffer_deg <- 1  # degrees

        # 6.3 Define the zoomed-in extent with a buffer around the point
        extent_zoom <- terra::ext(lon - buffer_deg, lon + buffer_deg,
                                  lat - buffer_deg, lat + buffer_deg)

        # 6.4 Transform to SpatVector
        map_data <- terra::vect(land)

        # 6.5 Crop land data to the zoomed-in extent
        land_zoom <- terra::crop(map_data, extent_zoom)

        # 6.6 ggplot2 object
        plot_map <- ggplot() +
          tidyterra::geom_spatvector(data = land_zoom, fill = "forestgreen",
                                     color = "black", alpha = 0.5) +
          tidyterra::geom_spatvector(data = sp_points, size = 2, color = "red",
                                     alpha = 0.7) +
          coord_sf(xlim = c(extent_zoom$xmin, extent_zoom$xmax),
                   ylim = c(extent_zoom$ymin, extent_zoom$ymax),
                   expand = FALSE) +
          labs(
            title = paste(paste(data$si_country[i], data$si_name[i], sep = " - "),
                          "is potentially over a water body"),
            x = "Longitude",
            y = "Latitude"
          ) +
          theme_minimal()

        # 6.7 Display the plot
        print(plot_map)

        # 6.8 Save the plot in the working directory
        # Sanitize file names to avoid illegal characters
        # sanitized_country <- gsub("[^A-Za-z0-9_]", "_", data$si_country[i])
        # sanitized_name <- gsub("[^A-Za-z0-9_]", "_", data$si_name[i])
        # plot_filename <- paste0(sanitized_country, "_", sanitized_name,
        #                         "_over_water.pdf")
        # plot_filepath <- file.path(getwd(),, plot_filename)
        #
        # ggsave(
        #   filename = plot_filepath,
        #   plot = plot_map,
        #   width = 20,
        #   height = 18,
        #   units = "cm"
        # )

    }

    # STEP 7
    # Create a console report with message if text_report is TRUE

    if (text_report) {

      # 7.1 Sum of wrong, correct and total coordinates checked
      wrong_coordinates <- sum(!results, na.rm = TRUE)
      correct_coordinates <- sum(results, na.rm = TRUE)
      total_coordinates <- wrong_coordinates + correct_coordinates

      over_water_coordinates <- sum(!on_land$on_land, na.rm = TRUE)
      on_land_coordinates <- sum(on_land$on_land, na.rm = TRUE)

      # 6.2 messages
      message(wrong_coordinates, ' wrong coordinates in data')
      message(correct_coordinates, ' correct coordinates in data')
      message(over_water_coordinates, ' coordinates over water in data')
      message(on_land_coordinates, ' coordinates on land in data')
      message(total_coordinates, ' coordinates checked')
    }

    # STEP 7
    # Create a new variables in data with the results of the checks
    data$is_inside_country <- results

    data$is_on_land <- on_land$on_land

    # 7.1 Return data with the new variables
    return(data)

    }
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
                            plot = FALSE,
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
        file_name <- paste("gadm41_", data$si_country[i], '_0_pk.rds', sep = '')

        # 2.1 map data is read and transformed to tidy format, to easy check signs
        country_map <- readRDS(file.path(maps_folder, "gadm", file_name)) |>
          terra::unwrap() |>
          terra::geom() |>
          as.data.frame() |>
          dplyr::rename(long = x, lat = y)

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
        if (data$si_lat[i] < 0) {
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
              si_name = res_data$si_name[j],
              stringsAsFactors = FALSE
            )
          } else {
            check_data_lat <- data.frame(
              si_long = res_data$si_long[j],
              si_lat = res_data$si_lat[j] * -1,
              si_country = res_data$si_country[j],
              si_name = res_data$si_name[j],
              stringsAsFactors = FALSE
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
              si_name = res_data$si_name[j],
              stringsAsFactors = FALSE
            )
          } else {
            check_data_long <- data.frame(
              si_long = res_data$si_long[j] * -1,
              si_lat = res_data$si_lat[j],
              si_country = res_data$si_country[j],
              si_name = res_data$si_name[j],
              stringsAsFactors = FALSE
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
            si_name = res_data$si_name[j],
            stringsAsFactors = FALSE
          )

          check_data_long <- data.frame(
            si_long = res_data$si_long[j] * -1,
            si_lat = res_data$si_lat[j],
            si_country = res_data$si_country[j],
            si_name = res_data$si_name[j],
            stringsAsFactors = FALSE
          )

          check_data_both <- data.frame(
            si_long = res_data$si_long[j] * -1,
            si_lat = res_data$si_lat[j] * -1,
            si_country = res_data$si_country[j],
            si_name = res_data$si_name[j],
            stringsAsFactors = FALSE
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
                                  plot= FALSE,
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
      sign_test_data <- qc_coord_sign_test(data, maps_folder, plot = FALSE,
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
    check_coord_data <- qc_check_coordinates(as.data.frame(data),
                                             maps_folder, plot = plot,
                                             parent_logger = parent_logger)

    # 1.1, check if it is correct
    if (check_coord_data$is_inside_country) {

      # if it is, return data with the is_inside_variable added
      message('Coordinates are correct')
      return(check_coord_data)

      # 1.2 if not, try to fixit
    } else {
      fixed_coord_data <- qc_fix_latlong_errors(check_coord_data, maps_folder,
                                                plot = FALSE,
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

################################################################################
#' Make a SpatialPolygonsDataFrame object of the biomes
#'
#' Creates a SpatialPolygonsDataFrame object of the Whittaker's biomes
#' modified by Ricklefs (2008) in function of mean annual temperature (MAT)
#' and mean annual precipitation (MAP) (MAT in degree Celsius and MAP in mm).
#'
#' @family Quality Checks Functions
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#'    in a single biome. By default, deserts are not merged.
#'
#' @return An object of class SpatialPolygonsDataFrame
#'
#' @export

# START
# Function declaration
qc_get_biomes_spdf <- function(merge_deserts = FALSE, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is merge_deserts logical?
    if (!(is.logical(merge_deserts))) {
      stop('merge_deserts must be logical')
    }
    # Is merge_deserts NA?
    if (is.na(merge_deserts)) {
      stop('merge_deserts must be either TRUE or FALSE')
    }

    # STEP 1
    # Create the data frame
    biomes_df <- data.frame(
      mat = c(29.339, 13.971, 15.371, 17.510, 24.131, 27.074, 28.915, 29.201, 29.339, 13.971, -9.706, -7.572,  4.491, 17.510,
              15.371, 13.971, 17.510,  4.491, -7.572, -9.706, -6.687, -0.949,  3.098,  7.147, 10.165, 13.918, 18.626, 18.176,
              17.510, 18.626, 13.918, 10.165,  7.147,  3.098, -0.949,  1.039,  1.998,  2.444,  3.118,  4.446,  7.758, 12.614,
              18.720, 18.637, 18.626, -0.949, -6.687, -4.395, -4.098, -1.592,  0.914,  4.155,  3.118,  2.444,  1.998,  1.039,
              -0.949, 18.720, 12.614,  7.758,  4.446,  3.118,  4.155, 15.716, 20.136, 19.392, 18.720, 18.720, 19.392, 20.136,
              22.278, 23.756, 24.199, 24.714, 25.667, 26.105, 27.414, 27.772, 25.709, 21.736, 18.720, 17.510, 18.176, 18.626,
              18.637, 18.720, 21.736, 25.709, 27.772, 28.418, 28.915, 27.074, 24.131, 17.510, -6.687, -8.896, -9.706, -13.382,
              -15.366, -15.217, -8.373, -4.098, -1.592, -4.098, -4.395, -6.687),
      map = c(21.3,  23.0, 174.6, 535.1, 702.9, 847.9, 992.4, 532.1,  21.3,  23.0,  7.3,  87.2, 314.6, 535.1, 174.6,  23.0,
              535.1, 314.6,  87.2,   7.3, 202.6, 391.7, 529.9, 783.1, 956.9,1116.5,1269.3, 794.3, 535.1,1269.3,1116.5, 956.9,
              783.1, 529.9, 391.7, 514.8, 673.4, 968.5,1630.6,1839.7,2028.0,2224.0,2355.7,1837.6,1269.3, 391.7, 202.6, 922.9,
              1074.1,1405.9,1744.9,2012.3,1630.6, 968.5,673.4, 514.8, 391.7,2355.7,2224.0,2028.0,1839.7,1630.6,2012.3,2930.1,
              3377.7,2917.0,2355.7,2355.7,2917.0,3377.7,3896.5,4343.1,4415.2,4429.8,4279.0,4113.7,3344.4,2790.6,2574.0,2414.3,
              2355.7, 535.1, 794.3,1269.3,1837.6,2355.7,2414.3,2574.0,2790.6,1920.3, 992.4, 847.9, 702.9, 535.1, 202.6,  50.8,
              7.3,  34.8,  98.8, 170.8, 533.0,1074.1,1405.9,1074.1, 922.9, 202.6),
      biome = c(rep('Subtropical desert', 9), rep('Temperate grassland desert', 7), rep('Mediterranean', 13),
                rep('Temperate forest', 16), rep('Boreal forest', 12), rep('Temperate rain forest', 10),
                rep('Tropical rain forest', 14), rep('Tropical forest savanna', 13), rep('Tundra', 12))
    )

    # STEP 2
    # Merge deserts if specified
    if (merge_deserts){

      biome <- as.character(biomes_df$biome)

      biome[grepl('desert', biome, fixed = TRUE)] <- 'Desert'

      biomes_df$biome <- as.factor(biome)

    }

    # STEP 3
    # Create SpatialPolygonsDataFrame object
    list_pol <- sapply(as.character(unique(biomes_df$biome)),
                       function(id_biome,df)
                         {terra::vect(cbind(df$map[df$biome == id_biome],
                                           df$mat[df$biome == id_biome]),
                                      type = "polygons")},
                          # sp::Polygon(cbind(df$map[df$biome == id_biome],
                          #                  df$mat[df$biome == id_biome]))
                       df = biomes_df, USE.NAMES = TRUE)


    spdf_biomes <- terra::vect(list_pol)
    spdf_biomes$biome <- names(list_pol)

    # spdf_biomes <- sp::SpatialPolygonsDataFrame(sp_biomes, data.frame(biome = names(list_pol)),
    #                                             match.ID = 'biome')

    # STEP 4
    # Return SpatialPolygonsDataFrame object
    return(spdf_biomes)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_get_biomes_spdf', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_get_biomes_spdf', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_get_biomes_spdf', sep = '.'))})

}

################################################################################
#' Get the biome, temperature and precipitation of a site
#'
#' This function takes a data frame of site metadata, including latitude (si_lat)
#' and longitude (si_long) columns, gets climatic data from WorldClim 1.4
#' using the GSODR package and
#' returns the same data frame with extra columns of mean annual temperature
#' (si_mat), mean annual precipitation (si_map) and biome (si_biome) according to
#' \code{\link{qc_get_biomes_spdf}}.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame of site metadata, including latitude (si_lat)
#'    and longitude (si_long) columns that are used to obtain climatic data.
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#'    in a single biome. By default, deserts are not merged.
#'
#' @return The same data frame with extra columns of mean annual temperature
#'    (si_mat), mean annual precipitation (si_map) and biome (si_biome)
#'
#' @export

# START
# Function declaration
qc_get_biome <- function(data, merge_deserts = FALSE, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data.frame?
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data frame.',
           ' Please verify if it is the correct object')
    }
    # Does data contains a longitude variable?
    if (is.null(data$si_long)) {
      stop('There is no longitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    # Does data contains a latitude variable?
    if (is.null(data$si_lat)) {
      stop('There is no latitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    # Is merge_deserts logical?
    if (!(is.logical(merge_deserts))) {
      stop('merge_deserts must be logical')
    }
    # Is merge_deserts NA?
    if (is.na(merge_deserts)) {
      stop('merge_deserts must be either TRUE or FALSE')
    }

    # STEP 1
    # Get WorldClim data (MAT and MAP) for sites
    temp_dir <- tempdir()
    t_site <- purrr::map2(
      data$si_long, data$si_lat,
      \(lon, lat) {
        bio_raster <- geodata::worldclim_tile(
          "bio", lon = lon, lat = lat, path = temp_dir
        )

        terra::extract(
          bio_raster,
          terra::vect(data.frame(lon = lon, lat = lat), crs = "epsg:4326")
        )[2] |> as.numeric()

      }
    ) |>
      purrr::list_c()
    p_site <- purrr::map2(
      data$si_long, data$si_lat,
      \(lon, lat) {
        bio_raster <- geodata::worldclim_tile(
          "bio", lon = lon, lat = lat, path = temp_dir
        )

        terra::extract(
          bio_raster,
          terra::vect(data.frame(lon = lon, lat = lat), crs = "epsg:4326")
        )[5] |> as.numeric()

      }
    ) |>
      purrr::list_c()

    # STEP 2
    # Obtain biome
    site_points <- terra::vect(
      cbind(p_site, t_site)
    )
    biome <- terra::extract(
      qc_get_biomes_spdf(merge_deserts = merge_deserts),
      site_points
    )$biome

    # STEP 4
    # Append new variables and return the data frame
    # 3.1 Append MAT, MAP and biome to data
    data$si_mat <- t_site
    data$si_map <- p_site
    data$si_biome <- biome

    # 3.2 Return data with the new variable
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_get_biome', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_get_biome', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_get_biome', sep = '.'))})

}
