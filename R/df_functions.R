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
df_folder_structure <- function(parent_dir = '.', parent_logger = 'test') {

  # Use calling handlers to logging to files
  withCallingHandlers({

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
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'df_folder_structure', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'df_folder_structure', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'df_folder_structure', sep = '.'))})
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
df_received_to_accepted <- function(remove = FALSE, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # is remove logical?
    if (!is.logical(remove)) {
      stop('remove parameter provided but not logical (TRUE or FALSE)')
    }

    # STEP 1
    # Obtaining the file names in the received folder.
    files <- list.files('Received_data', pattern = "(_env_data|_sapflow_data)\\.csv$|_metadata\\.xls(x)?$")

    # STEP 2
    # Extract site code from file names
    codes <- unique(stringr::str_replace(
      files, "(_env_data|_sapflow_data)\\.csv$|_metadata\\.xls(x)?$", ""
    ))

    # STEP 3
    # Check if folder with site code already exists
    for (code in codes) {

      # message indicating which code is moving
      message('Copying files corresponding to ', code, ' site.')

      # 3.1 path and file names
      path <- file.path('Data', code)
      path_accepted <- file.path(path, 'Accepted')
      from_files <- list.files('Received_data',
                               pattern = paste('(', code,
                                               '_env_data|', code, '_sapflow_data)\\.csv$|',
                                               code, '_metadata\\.xls(x)?$', sep = ''),
                               full.names = TRUE)
      file_names <- stringr::str_replace(from_files, 'Received_data', path_accepted)

      # 3.2 check presence
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

        # STEP 4
        # Creating the data folder structure for site
        # dir.create(path)
        dir.create(path_accepted, recursive = TRUE)
        dir.create(file.path(path, 'Lvl_1'))
        dir.create(file.path(path, 'Lvl_2'))

        # STEP 5
        # Copy data to accepted folder
        file.copy(from = from_files, to = path_accepted, overwrite = FALSE)
      }

      # STEP 6
      # Check if copy has been done correctly by comparing file md5sums
      md5_ok <- tools::md5sum(from_files) == tools::md5sum(file_names)

      if (!all(md5_ok, na.rm = TRUE)) {
        warning('md5sums are not the same for original and copied files. ',
                'Please revise the files manually.',
                ' Skipping to the next site code (if any)')
        next
      }

      # STEP 7
      # Remove from files
      if (remove & all(md5_ok, na.rm = TRUE)) {
        message('Removing the received files for site ', code, '.')
        file.remove(from_files)
        message('Remove DONE!')
      } else {
        warning('remove argument set to FALSE, data will be left ',
                'in Received Data folder')
      }

      # STEP 8
      # Indicating which files were copied
      files_copied <- vapply(dir(path_accepted),
                             function(x){paste(x, '\n', sep = '')},
                             character(1))

      message("List of files copied to ", path_accepted, ':\n', files_copied)

    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'df_received_to_accepted', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'df_received_to_accepted', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'df_received_to_accepted', sep = '.'))})
}

################################################################################
#' Initialise an empty status file
#'
#' Initialise an empty status file in yaml format, using the yaml package
#'
#' Before creating an empty file, \code{df_start_status} checks if an status
#' file already exists, in order to avoid accidental rewriting of the file.
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code
#'
#' @return Invisible TRUE if no errors were encountered, invisible FALSE if
#'   there was errors. Also, status file is created in the corresponding folder.
#'
#' @export

# START
# Function declaration
df_start_status <- function(si_code, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is si_code a character?
    if (!is.character(si_code)) {
      stop('si_code is not a character')
    }

    # STEP 1
    # Check if status file already exists
    filename <- file.path('Data', si_code, paste(si_code, '_status.yaml', sep = ''))

    if (file.exists(filename)) {

      # 1.1 if exists, raise a warning and return false
      warning('Status file for ', si_code, 'already exists, skipping creation of empty file')
      return(invisible(FALSE))

      # 1.2 if not, continue to step 2
    } else {

      # STEP 2
      # Create the file

      # 2.1 create the content
      content <- list(
        QC = list(DONE = FALSE, DATE = NULL),
        LVL1 = list(STORED = FALSE, DATE = NULL),
        LVL2 = list(STORED = FALSE, DATE = NULL)
      )

      # 2.2 create the yaml object
      yaml_content <- yaml::as.yaml(content)

      # 2.3 create the file with the yaml object
      cat(yaml_content, file = filename, append = FALSE)

      # 2.4 return true (invisible)
      return(invisible(TRUE))
    }

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'df_status_start', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'df_status_start', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'df_status_start', sep = '.'))})

}

################################################################################
#' Get the status file info
#'
#' Retrieve the status file info as a list, with the yaml package
#'
#' \code{yaml} file is parsed by yaml.load_file and a list object is returned.
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code
#'
#' @return a list object with the info contained in the status file
#'
#' @export

# START
# Function declaration

df_get_status <- function(si_code, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is si_code a character?
    if (!is.character(si_code)) {
      stop('si_code is not a character')
    }

    # STEP 1
    # Check if status file exists
    filename <- file.path('Data', si_code, paste(si_code, '_status.yaml', sep = ''))
    if (!file.exists(filename)) {

      # 1.1 if don't exist, raise a warning and return false
      warning('Status file for ', si_code, 'does not exist, unable to retrieve info')
      return(invisible(FALSE))
    } else {

      # STEP 2
      # Get the yaml object
      res <- yaml::yaml.load_file(filename)

      # 2.1 and return it
      return(res)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'df_get_status', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'df_get_status', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'df_get_status', sep = '.'))})
}

################################################################################
#' Set the status file info
#'
#' Change and update status file info using yaml package
#'
#' \code{QC}, \code{LVL1} and \code{LVL2} must be lists. In the case of
#' \code{QC}, a list with two elements, \code{DONE} and \code{DATE}. In the case
#' of \code{LVL1} and \code{LVL2} a list with two elements, \code{STORED} and
#' \code{DATE}. \code{DONE} and \code{STORED} are logicals, whereas \code{DATE}
#' is always a character or NULL.
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code
#'
#' @param QC List with the QC info to be updated
#'
#' @param LVL1 List with the LVL1 info to be updated
#'
#' @param LVL2 List with the LVL2 info to be updated
#'
#' @return Invisible TRUE if changes to status file were correctly made,
#'   invisble FALSE if changes were not made. Also, the status file for the site
#'   will be replaced with the new one.
#'
#' @export

# START
# Function declaration
df_set_status <- function(si_code,
                          QC = NULL,
                          LVL1 = NULL,
                          LVL2 = NULL,
                          parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is si_code a character?
    if (!is.character(si_code)) {
      stop('si_code is not a character')
    }

    # STEP 1
    # Check if the file already exists
    filename <- file.path('Data', si_code, paste(si_code, '_status.yaml', sep = ''))
    if (!file.exists(filename)) {

      # 1.1 if don't exist, raise a warning and return false
      warning('Status file for ', si_code, 'does not exist, unable to retrieve info')
      return(invisible(FALSE))
    } else {

      # STEP 2
      # Modify the status file

      # 2.1 get the original
      original_yaml <- df_get_status(si_code, parent_logger = parent_logger)

      # 2.2 modify original with new no NULL elements
      if (!is.null(QC)) {
        original_yaml$QC <- QC
      }

      if (!is.null(LVL1)) {
        original_yaml$LVL1 <- LVL1
      }

      if (!is.null(LVL2)) {
        original_yaml$LVL2 <- LVL2
      }

      # STEP 3
      # Convert to yaml and rewrite the status file
      res <- yaml::as.yaml(original_yaml)

      # 3.1 rewrite
      cat(res, file = filename, append = FALSE)

      # return invisible TRUE
      return(invisible(TRUE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_set_status', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_set_status', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_set_status', sep = '.'))})
}

################################################################################
#' Reports folders creation
#'
#' Function to create the report subfolder for the site to analyze
#'
#' @family Data Flow
#'
#' @param si_code Character indicating the site code to create the reports folder
#'
#' @return Invisible TRUE if the folder is created correctly, invisible FALSE if
#'   folder is not created.
#'
#' @export

# START
# Function declaration
df_report_folder_creation <- function(si_code, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # is si_code a character?
    if(!is.character(si_code)) {
      stop('si_code provided is not a character')
    }

    # STEP 1
    # Check if folder already exists
    folder <- file.path('Reports', si_code)

    if (dir.exists(folder)) {
      message(folder, ' folder already exists. skipping creation')
      return(invisible(FALSE))
    } else {

      # 1.1 If folder does not exist, create it
      dir.create(folder, showWarnings = TRUE)
      return(invisible(TRUE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_report_folder_creation',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_report_folder_creation',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_report_folder_creation',
                                                        sep = '.'))})
}
