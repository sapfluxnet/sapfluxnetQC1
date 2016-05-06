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
#' Move from received to accepted data folders
#'
#' Check if data file/s already exists and create the folder structure needed to
#' store them.
#'
#' The first step after receiving a data set is to move it from received folder
#' to accepted folder (\code{Data/site_code/site_code_accepted/}).
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
df_received_to_accepted <- function(remove = FALSE) {

  # STEP 0
  # Argument checking
  # is remove logical?
  if (!is.logical(remove)) {
    stop('remove parameter provided but not logical (TRUE or FALSE)')
  }

  # STEP 1
  # Obtaining the file names in the received folder.
  files <- list.files('Received_data', pattern = "(environmental|sapflow)\\.csv$|_metadata\\.xls(x)?$")

  # STEP 2
  # Extract site code from file names
  codes <- unique(stringr::str_replace(
    files, "(_environmental|_sapflow)\\.csv$|_metadata\\.xls(x)?$", ""
  ))

  # STEP 2
  # Check if folder with site code already exists
  for (code in codes) {

    # 2.1 path and file names
    path <- file.path('Data', code)
    path_accepted <- file.path(path, 'Accepted')
    file_names <- c(file.path(path_accepted, paste(code, '_metadata.xlsx', sep = '')),
                    file.path(path_accepted, paste(code, '_metadata.xls', sep = '')),
                    file.path(path_accepted, paste(code, '_environmental.csv', sep = '')),
                    file.path(path_accepted, paste(code, '_sapflow.csv', sep = '')))
    from_files <- c(file.path('Received_data', paste(code, '_metadata.xlsx', sep = '')),
                    file.path('Received_data', paste(code, '_metadata.xls', sep = '')),
                    file.path('Received_data', paste(code, '_environmental.csv', sep = '')),
                    file.path('Received_data', paste(code, '_sapflow.csv', sep = '')))

    # 2.2 check presence
    if (dir.exists(path)) {
      warning('Folder ', path, ' already exists! ',
              'This means that site is already in the system')
      if (any(file.exists(file_names))) {
        warning('One or more files already exist. ',
                'Not copying any file, manual intervention needed.')
        next
      } else {

        # 2.3 copy the files if directory exists but files don't
        file.copy(from = from_files, to = path_accepted, overwrite = FALSE)
      }
    } else {

      # STEP 3
      # Creating the data folder structure for site
      # dir.create(path)
      dir.create(path_accepted, recursive = TRUE)
      dir.create(file.path(path, 'Lvl_1'))
      dir.create(file.path(path, 'Lvl_2'))

      # STEP 4
      # Copy data to accepted folder
      file.copy(from = from_files, to = path_accepted, overwrite = FALSE)
    }

    # STEP 5
    # Check if copy has been done correctly by comparing file md5sums
    md5_ok <- tools::md5sum(from_files) == tools::md5sum(file_names)

    if (!all(md5_ok, na.rm = TRUE)) {
      warning('md5sums are not the same for original and copied files. ',
              'Please revise manually the files.',
              ' Skipping to the next site code (if any)')
      next
    }

    # STEP 6
    # Remove from files
    if (remove & all(md5_ok, na.rm = TRUE)) {
      message('Removing the received files...')
      file.remove(from_files)
      message('DONE!')
    } else {
      warning('remove argument set to FALSE, data will be left ',
              'in Received Data folder')
    }
  }

  # END FUNCTION
}
