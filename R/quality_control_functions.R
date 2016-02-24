#' Download maps for countries included in the database
#'
#' \code{download_maps} fetch maps from \url{http://www.gadm.org/}.
#'
#' This function fetch maps from \url{http://www.gadm.org/} if the map it's not
#' already present in the maps folder (by default, the working directory).
#'
#' @family Quality Check Functions
#'
#' @param data Dataframe where the countries ISO codes are. Must contain a
#'   variable called \code{country} where the ISO code resides.
#'
#' @param folder Folder where the maps are stored or where they will be stored,
#'   by default the working directory. It must be a character.
#'
#' @return Maps are downloaded (if needed) and a summary is returned indicating
#'   number of maps downlaoded and number of maps present in the map folder.

# START
# function declaration

download_maps <- function(data, folder) {

  # STEP 0
  # Data checks
  #   if data is a data.frame
  if (!is.data.frame(data)) {
    stop('Provided data object is not a data.frame.\n
         Please check if it is the correct object\n')
  }
  #   if data contains a country variable
  if (is.null(data$country)) {
    stop('There is no country variable in this dataset')
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
        # Dowload file
        download.file(url_name,
                      paste(folder, file_name, sep = ''),
                      cacheOK = FALSE)

        # STEP 5.a
        # Update downloaded maps count
        downloaded_maps <- downloaded_maps + 1
      }
    }
  }

  # STEP 6
  # Return a summary of downloaded maps and existent maps
  print(paste(existent_maps, 'already downloaded and saved in', folder),
        sep = ' ')
  print(paste(downloaded_maps, 'new maps downloaded'), sep = ' ')
  print(paste(length(list.files(folder, pattern = '.rds')),
              'maps now in', folder), sep = ' ')

# END function
}
