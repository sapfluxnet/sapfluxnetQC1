################################################################################
# DATA FLOW FUNCTIONS                                                          #
#                                                                              #
# Functions to perform data flows and to create data folder architecture       #
################################################################################

################################################################################
#' Initial project folder structure
#'
#' Create the folder tree skeleton for SAPFLUXNET project
#'
#' This functions only need to be called once, as it creates the needed folder
#' structure to recreate SAPFLUXNET project in any environment.
#'
#' @family Data Flow
#'
#' @param parent_dir Character vector indicating the parent directory where
#'   the structure must be built. Default to current directory.
#'
#' @return Nothing, dirs are created if they do not exist, or a warning is raised
#'   if they already exist.
#'
#' @export

# START
# Function declaration
df_folder_structure <- function(parent_dir = '.') {

  # STEP 0
  # Argument checkings
  # Is parent directory a character vector?
  if (!is.character(parent_dir)) {
    stop('Provided parent directory is not a character vector')
  }
  # Is parent directory a valid and existent directory?
  if (!dir.exists(parent_dir)) {
    stop('Provided parent directory does not exist')
  }

  # STEP 1
  # Check if dirs already exists and if not, create it (all with dir.create)
  dir.create(file.path(parent_dir, 'Data'), showWarnings = TRUE)
  dir.create(file.path(parent_dir, 'Logs'), showWarnings = TRUE)
  dir.create(file.path(parent_dir, 'Reports'), showWarnings = TRUE)
  dir.create(file.path(parent_dir, 'Templates'), showWarnings = TRUE)

  # END FUNCTION
}

################################################################################
#' Move from received to raw data folders
#'
#' Check if data file/s already exists and create the folder structure needed to
#' store them.
#'
#' The first step after receiving a data set is to move it from received folder
#' to raw folder (\code{Data/site_code/site_code_Raw/}).
#'
#' @family Data Flow
#'
#' @param remove Logical indicating if files in received folder are dropped after
#'   correct after the file transfer. Default to FALSE.
#'
#' @return
#'
#' @export

# START
# Function declaration
df_received_to_raw <- function(remove = FALSE) {

  # STEP 1
  # Obtaining the file names in the received folder.
  files <- list.files('.', pattern = "(environmental|sapflow)\\.csv$|_metadata\\.xls(x)?$")

  # STEP 2
  # Extract site code from file names
  codes <- unique(stringr::str_replace(
    files, "(environmental|sapflow)\\.csv$|_metadata\\.xls(x)?$", ""
  ))

  # STEP 2
  # Check if folder with site code already exists
  for (code in codes) {

    # 2.1 path and file names
    path <- file.path('..', 'Data', code)
    path_raw <- file.path(path, 'Raw')
    file_names <- c(file.path(path_raw, paste(code, 'metadata.xlsx')),
                    file.path(path_raw, paste(code, 'environmental.csv')),
                    file.path(path_raw, paste(code, 'sapflow.csv')))

    # 2.2 check presence
    if (dir.exists(path)) {
      warning('Folder ', path, ' already exists! ',
              'This means that site is already in the system')
      if (any(file.exists(file_names))) {
        warning('One or more files already exist. ',
                'Not copying any file, manual intervention needed.')
        break
      } else {
        # aquí iria la copia de los archivos cuando el directorio existe
        break
      }
    } else {

      # STEP 3
      # Creating the data folder structure for site
      dir.create(path)
      dir.create(path_raw)
      dir.create(file.path(path, 'Lvl_1'))
      dir.create(file.path(path, 'Lvl_2'))

      # STEP 4
      # Copy data to raw folder
      file.copy()
    }
  }
}
