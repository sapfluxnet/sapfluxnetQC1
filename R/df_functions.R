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
    files <- list.files('received_data', pattern = "(_env_data|_sapflow_data)\\.csv$|_metadata\\.xls(x)?$")

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
      from_files <- list.files('received_data',
                               pattern = paste('(', code,
                                               '_env_data|', code, '_sapflow_data)\\.csv$|',
                                               code, '_metadata\\.xls(x)?$', sep = ''),
                               full.names = TRUE)
      file_names <- stringr::str_replace(from_files, 'received_data', path_accepted)

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
        LVL1 = list(STORED = FALSE, DATE = NULL, TO_LVL2 = 'FREEZE'),
        LVL2 = list(STORED = FALSE, DATE = NULL, STEP = NULL,
                    TO_REM = 'FREEZE', TO_UNITS = 'FREEZE')
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
        purrr::walk(names(QC), function(x) {
          original_yaml[["QC"]][[x]] <<- QC[[x]]
        })
      }

      if (!is.null(LVL1)) {
        purrr::walk(names(LVL1), function(x) {
          original_yaml[["LVL1"]][[x]] <<- LVL1[[x]]
        })
      }

      if (!is.null(LVL2)) {
        purrr::walk(names(LVL2), function(x) {
          original_yaml[["LVL2"]][[x]] <<- LVL2[[x]]
        })
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

################################################################################
#' Get the folders to feed \code{\link{qc_start_process}}
#'
#' Get the folders to be able to start QC process
#'
#' No parameters (except for parent_logger) are needed, as the functions simply
#' collects the folders present in \code{Data}.
#'
#' @family Data Flow
#'
#' @return a character vector with the folders route
#'
#' @export

# START
# Function declaration
df_get_data_folders <- function(parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Collect the data folders names
    folder_names <- list.dirs('Data', recursive = FALSE)

    # STEP 2
    # Return the folder names
    return(folder_names)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_get_data_folders',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_get_data_folders',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_get_data_folders',
                                                        sep = '.'))})
}

################################################################################
#' Save the fixed datasets (metadata and data) in the LVL1 folder
#'
#' This function performs three actions: 1) write csv files with the fixed data
#' in the LVL1 folder. 2) Update the status files to indicate that data is in
#' LVL1 and the date of the move. 3) Save the objects generated in the QC to
#' make easy the creation of dashboards without the need of run all QC tests
#' again.
#'
#' @family Data Flow
#'
#' @param si_code Character with the site code
#'
#' @param sapf_data Data frame with the fixed sapflow data
#'
#' @param env_data Data frame with the fixed environmental data
#'
#' @param site_md Data frame with the fixed site metadata
#'
#' @param stand_md Data frame with the fixed stand metadata
#'
#' @param plant_md Data frame with the fixed plant metadata
#'
#' @param species_md Data frame with the fixed species metadata
#'
#' @param env_md Data frame with the fixed environmental metadata
#'
#' @return Nothing
#'
#' @export

# START
# Function declaration
df_accepted_to_lvl1 <- function(si_code, sapf_data = NULL, env_data = NULL,
                                site_md = NULL, stand_md = NULL,
                                plant_md = NULL, species_md = NULL,
                                env_md = NULL, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # if any of the data is NULL (does not exists), stop and report, except
    # for the different sapflow unit conversions, as they can be missing
    if(any(is.null(sapf_data), is.null(env_data), is.null(site_md),
           is.null(stand_md), is.null(plant_md), is.null(species_md),
           is.null(env_md))) {
      stop('One or more datasets were not provided')
    }
    # are datasets dataframes?
    if(any(!is.data.frame(sapf_data), !is.data.frame(env_data),
           !is.data.frame(site_md), !is.data.frame(stand_md),
           !is.data.frame(plant_md), !is.data.frame(species_md),
           !is.data.frame(env_md))) {
      stop('One or more datasets provided are not data frames')
    }
    # is si_code a character string?
    if(!is.character(si_code)) {
      stop('site code provided is not a character string')
    }
    # if files exist before the function execution, stop and inform
    if (all(
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'sapf_data.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'env_data.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'site_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'stand_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'plant_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'species_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'env_md.csv', sep = '_')))
    )) {
      stop('csv files already exist in Lvl_1 folder. Please revise manually')
    }

    # STEP 1
    # Writing csv files
    write.csv(sapf_data,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'sapf_data.csv', sep = '_')),
              row.names = FALSE)
    write.csv(env_data,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'env_data.csv', sep = '_')),
              row.names = FALSE)
    write.csv(site_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'site_md.csv', sep = '_')),
              row.names = FALSE)
    write.csv(stand_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'stand_md.csv', sep = '_')),
              row.names = FALSE)
    write.csv(plant_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'plant_md.csv', sep = '_')),
              row.names = FALSE)
    write.csv(species_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'species_md.csv', sep = '_')),
              row.names = FALSE)
    write.csv(env_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'env_md.csv', sep = '_')),
              row.names = FALSE)

    # STEP 2
    # Updating status file
    # only if the files have been created
    if (all(
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'sapf_data.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'env_data.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'site_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'stand_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'plant_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'species_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'env_md.csv', sep = '_')))
    )) {
      df_set_status(si_code,
                    LVL1 = list(STORED = TRUE, DATE = as.character(Sys.Date())))
    } else {
      stop('One or more files has been not created in Lvl_1 folder, please revise manually')
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_accepted_to_lvl1',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_accepted_to_lvl1',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_accepted_to_lvl1',
                                                        sep = '.'))})
}

################################################################################
#' Save the Rmd templates and the running scripts to their corresponding folders
#'
#' Copy the Rmd templates for file transfer and quality check to Template folder and
#' the template for shiny web app to parent directory, and running scripts to parent
#' directory.
#'
#' If it is the first time to set the SAPFLUXNET project, execute this function
#' after the function \code{\link{df_folder_structure}} with the argument
#' \code{first} set to \code{TRUE}
#'
#' @family Data Flow
#'
#' @param first Logical indicating if it is the first time the files are copied
#'
#' @return Logical indicating if all files have been copied or overwritten
#'
#' @export

# START
# Function declaration
df_copy_templates <- function(first = FALSE, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Does 'Templates' directory exists?
    if (!dir.exists('Templates')) {
      stop('Templates directory does not exist')
    }

    # STEP 2
    # If it is the first time, just copy the files
    if(first){

      # Copy templates for file transfer and quality check to Template folder
      file.copy(
        system.file('Rmd_templates', 'received_to_accepted.Rmd',
                    package = 'sapfluxnetQC1'),
        file.path('Templates'), overwrite = TRUE
      )
      file.copy(
        system.file('Rmd_templates', 'QC_report.Rmd',
                    package = 'sapfluxnetQC1'),
        file.path('Templates'), overwrite = TRUE
      )

      # Copy template for shiny web app to parent directory
      file.copy(
        system.file('Rmd_templates', 'sfn_monitor.Rmd',
                    package = 'sapfluxnetQC1'),
        file.path('.'), overwrite = TRUE
      )

      # Copy scripts to parent directory
      file.copy(
        system.file('run_scripts', 'main_script.R',
                    package = 'sapfluxnetQC1'),
        file.path('.'), overwrite = TRUE
      )
      file.copy(
        system.file('run_scripts', 'debug_script.R',
                    package = 'sapfluxnetQC1'),
        file.path('.'), overwrite = TRUE
      )

      return(invisible(TRUE))
    }

    # STEP 3
    # Checks when first is set to FALSE

    # Get the time of last modification for all the files previous to overwritting
    pre_time <- file.mtime(c(file.path('Templates','received_to_accepted.Rmd'),
                 file.path('Templates','QC_report.Rmd'),'sfn_monitor.Rmd',
                 'main_script.R','debug_script.R'))

    # Give an error if modification time of the files can not be obtained
    if(any(is.na(pre_time))){
      stop('Check whether templates and running scripts already exist. ',
           'If some of them do not exist, delete those that exist (if any) ',
           'and run again the function with parameter first set to TRUE')
    }

    # STEP 4
    # Copy templates for file transfer and quality check to Template folder
    file.copy(
      system.file('Rmd_templates', 'received_to_accepted.Rmd',
                  package = 'sapfluxnetQC1'),
      file.path('Templates'), overwrite = TRUE
    )
    file.copy(
      system.file('Rmd_templates', 'QC_report.Rmd',
                  package = 'sapfluxnetQC1'),
      file.path('Templates'), overwrite = TRUE
    )

    # Copy template for shiny web app to parent directory
    file.copy(
      system.file('Rmd_templates', 'sfn_monitor.Rmd',
                  package = 'sapfluxnetQC1'),
      file.path('.'), overwrite = TRUE
    )

    # Copy scripts to parent directory
    file.copy(
      system.file('run_scripts', 'main_script.R',
                  package = 'sapfluxnetQC1'),
      file.path('.'), overwrite = TRUE
    )
    file.copy(
      system.file('run_scripts', 'debug_script.R',
                  package = 'sapfluxnetQC1'),
      file.path('.'), overwrite = TRUE
    )

    # STEP 5
    # Check that all times of last modification have changed
    post_time <- file.mtime(c(file.path('Templates','received_to_accepted.Rmd'),
                             file.path('Templates','QC_report.Rmd'),'sfn_monitor.Rmd',
                             'main_script.R','debug_script.R'))

    if(all(post_time != pre_time)){
      return(invisible(TRUE))
    } else {
      warning("Some Rmd templates or running scripts have not been copied")
      return(invisible(FALSE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_copy_templates',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_copy_templates',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_copy_templates',
                                                        sep = '.'))})
}

################################################################################
#' Reset the data folder and status file for the si_code given.
#'
#' After manual changes, update the satus file to indicate that QC is needed.
#' Also rename Accepted and Lvl1 data to avoid conflicts with manually
#' changed data.
#'
#' A fast way of reset any site data folder when needed, usually after manual
#' changes of the files
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code to reset
#'
#' @return Nothing
#'
#' @export

# START
# Function declaration
df_reset_data_status <- function(si_code, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.character(si_code)) {
      stop('si_code provided is not a character')
    }

    # STEP 1
    # Setting the status

    # 1.1 status lists
    QC = list(DONE = FALSE, DATE = NULL)
    LVL1 = list(STORED = FALSE, DATE = NULL, TO_LVL2 = 'FREEZE')
    LVL2 = list(STORED = FALSE, DATE = NULL, STEP = NULL)

    # 1.2 set status
    df_set_status(si_code, QC = QC, LVL1 = LVL1, parent_logger = parent_logger)

    # STEP 2
    # Renaming data

    # 2.1 file names, old and new
    old_files <- c(
      list.files(file.path('Data', si_code, 'Accepted'), full.names = TRUE),
      list.files(file.path('Data', si_code, 'Lvl_1'), full.names = TRUE)
    )

    # 2.1.1 new names, substituting extension for _time.bak
    new_files <- stringr::str_replace_all(
      old_files,
      pattern = "(.csv|.RData|.xlsx)",
      replace = paste0('_', format(Sys.time(), '%Y%m%d%H%M'), '.bak')
    )

    # 2.2 Rename step
    file.rename(
      from = old_files,
      to = new_files
    )

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_reset_data_status',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_reset_data_status',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_reset_data_status',
                                                        sep = '.'))})
}

################################################################################
#' Create a SfnData object from results of Quality Checks
#'
#' This function gather all the data and metadata generated in the QC process
#' and build an SfnData class object
#'
#' \code{sfn_data_constructor} function generates a SfnData object containing
#' all relevant data and metadata for a site. Sapflow and environmental data
#' are converted to the same timestamp span and added rows are flagged as
#' \code{NA_ADDED}. Original NAs are also flagged as \code{NA_PRESENT}. For
#' info about the available slots see \code{\link{SfnData}},
#' \code{\link{sfn_get_methods}} and \code{\link{sfn_replacement}}
#'
#' @family Data Flow
#'
#' @param sapf_data Data frame with sapflow data after QC process
#'
#' @param env_data Data frame with environmental data after QC process
#'
#' @param site_md Data frame with the site metadata after QC process
#'
#' @param stand_md Data frame with the stand metadata after QC process
#'
#' @param species_md Data frame with the species metadata after QC process
#'
#' @param plant_md Data frame with the plant metadata after QC process
#'
#' @param env_md Data frame with the environmental after QC process
#'
#' @return A SfnData object with all the data and metadata of the site
#'
#' @export

# START
# Function declaration
sfn_data_constructor <- function(sapf_data = NULL, env_data = NULL,
                                 site_md = NULL, stand_md = NULL,
                                 species_md = NULL, plant_md = NULL,
                                 env_md = NULL, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (any(
      !is.data.frame(sapf_data), !is.data.frame(env_data),
      !is.data.frame(site_md), !is.data.frame(stand_md),
      !is.data.frame(species_md), !is.data.frame(plant_md),
      !is.data.frame(env_md)
    )) {
      stop('Data and/or metadata objects provided are not data.frames')
    }

    # STEP 1
    # match nrow between sapf and env data, and if new rows are
    # needed, flag them!!

    # 1.1 New timestamp with the full join of sapf and env
    sapf_timestamp <- sapf_data %>% dplyr::select(TIMESTAMP)
    env_timestamp <- env_data %>% dplyr::select(TIMESTAMP)
    timestamp_join <- dplyr::full_join(sapf_timestamp, env_timestamp, "TIMESTAMP")

    # 1.2 Data with the new timestamp and NAs where the rows are added
    .sapf_data <- dplyr::full_join(timestamp_join, sapf_data, "TIMESTAMP") %>%
      dplyr::arrange(TIMESTAMP)
    .env_data <- dplyr::full_join(timestamp_join, env_data, "TIMESTAMP") %>%
      dplyr::arrange(TIMESTAMP)

    # 1.3 flags indicating the pre-existent NAs and the new added NAs
    .sapf_flags <- sapf_data[,-1] %>%
      is.na() %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(dplyr::funs(replace(., which(. == TRUE), "NA_PRESENT"))) %>%
      dplyr::mutate_all(dplyr::funs(replace(., which(. == FALSE), ""))) %>%
      dplyr::mutate(TIMESTAMP = sapf_data$TIMESTAMP) %>%
      dplyr::full_join(timestamp_join, "TIMESTAMP") %>%
      dplyr::arrange(TIMESTAMP) %>%
      dplyr::select(-TIMESTAMP) %>%
      dplyr::mutate_all(dplyr::funs(replace(., which(is.na(.)), "NA_ADDED")))

    .env_flags <- env_data[,-1] %>%
      is.na() %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(dplyr::funs(replace(., which(. == TRUE), "NA_PRESENT"))) %>%
      dplyr::mutate_all(dplyr::funs(replace(., which(. == FALSE), ""))) %>%
      dplyr::mutate(TIMESTAMP = env_data$TIMESTAMP) %>%
      dplyr::full_join(timestamp_join, "TIMESTAMP") %>%
      dplyr::arrange(TIMESTAMP) %>%
      dplyr::select(-TIMESTAMP) %>%
      dplyr::mutate_all(dplyr::funs(replace(., which(is.na(.)), "NA_ADDED")))

    timestamp_join <- timestamp_join %>% dplyr::arrange(TIMESTAMP)

    # STEP 2
    # Build the SfnData object and return it
    res <- SfnData(
      sapf_data = .sapf_data[, -1],
      env_data = .env_data[, -1],
      sapf_flags = .sapf_flags,
      env_flags = .env_flags,
      timestamp = timestamp_join[[1]],
      si_code = rep(site_md$si_code, length(timestamp_join[[1]])),
      site_md = site_md,
      stand_md = stand_md,
      species_md = species_md,
      plant_md = plant_md,
      env_md = env_md
    )

    # 2.1 Return it!!
    return(res)
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'sfn_data_constructor',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'sfn_data_constructor',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'sfn_data_constructor',
                                                        sep = '.'))})
}

################################################################################
#' Who is ready for level 2?
#'
#' Check the site status files to list who is ready to move to level 2
#'
#' \code{filter} parameter indicates if the results must be filtered by the
#'   corresponding status:
#'
#'   \itemize{
#'     \item{\code{all} retrieves all the statuses}
#'     \item{\code{ready} retrieves only those sites marked to pass to level 2}
#'     \item{\code{freeze} retrieves only those sites freezed in level 1 yet}
#'     \item{\code{done} retrieves only those sites already passed to level 2}
#'   }
#'
#' @family Data Flow
#'
#' @param filter character vector indicating by which TO_LVL2 status results must
#'   been filtered. Accepted values are "all" (default), "ready", "freeze" or "done"
#'   (see details)
#'
#' @return A list with length equal to the number of sites containing the
#'   TO_LVL2 flag of the status files.
#'
#' @export

# START
# Function declaration
df_who_ready_to_lvl2 <- function(filter = c('all', 'ready', 'freeze', 'done'),
                                 parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Checking arguments (match.arg throws an error if not matching)
    filter <- match.arg(filter)
    filter <- switch(filter,
                     all = 'all',
                     ready = 'READY',
                     freeze = 'FREEZE',
                     done = 'DONE')

    # STEP 1
    # Getting the site codes to pass to df_get_status
    site_folders <- df_get_data_folders(parent_logger = parent_logger) %>%
      stringr::str_sub(6, -1)

    # STEP 2
    # Get the statuses
    whos_ready <- site_folders %>%
      purrr::map(df_get_status, parent_logger = parent_logger) %>%
      # STEP 3
      # Get the TO_LVL2 flag
      purrr::modify_depth(1, c('LVL1', 'TO_LVL2'), .null = NA)

    # STEP 3
    # Prepare the results
    # 3.1 Name the list elements
    names(whos_ready) <- site_folders
    # 3.2 filter the results
    if (filter != 'all') {
      whos_ready <- whos_ready[whos_ready == filter]
    }

    # STEP 4
    # Return the list
    return(whos_ready)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_who_ready_to_lvl2',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_who_ready_to_lvl2',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_who_ready_to_lvl2',
                                                        sep = '.'))})
}

################################################################################
#' Build the folder structure of level 2
#'
#' Take a site code and build the level 2 tree folder
#'
#' @family Data Flow
#'
#' @param si_code Site code as character
#'
#' @return Nothing, if dir is created ok silent exit, if not it throws an error
#'
#' @export

# START
# Function declaration
df_lvl2_folder_structure <- function(si_code, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Parameters check
    # is si_code a character
    if (!is.character(si_code)) {
      stop('si_code provided is not a character')
    }

    # STEP 1
    # Dir creation
    # 1.1 folder name
    lvl_root_name <- file.path('Data', si_code, 'Lvl_2')

    # 1.3 heck if dirs already exists and if not, create it (all with dir.create)
    dir.create(file.path(lvl_root_name, 'lvl_2_out_warn'), showWarnings = TRUE)
    dir.create(file.path(lvl_root_name, 'lvl_2_out_rem'), showWarnings = TRUE)
    dir.create(file.path(lvl_root_name, 'lvl_2_unit_trans'), showWarnings = TRUE)

    # STEP 2
    # Check if creation went well
    if (all(dir.exists(file.path(lvl_root_name, c('lvl_2_out_warn',
                                                  'lvl_2_out_rem',
                                                  'lvl_2_unit_trans'))))) {
      return()
    } else {
      stop("One or more folders can not be created in level 2. ",
           "Please revise manually")
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_lvl2_folder_structure',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_lvl2_folder_structure',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_lvl2_folder_structure',
                                                        sep = '.'))})
}

################################################################################
#' Load SfnData
#'
#' Accesory function to load an specified SfnData object
#'
#' Given a site code and a level description, \code{df_read_SfnData} will return
#' the selected SfnData object from the selected location
#'
#' @family Data Flow
#'
#' @param si_code Site code as a character string
#'
#' @param level Level to read from as a character string
#'
#' @return A SfnData object.
#'
#' @export

# START
# Function declaration
df_read_SfnData <- function(si_code, level = c("Lvl_1", "Lvl_2", "out_warn",
                                               "out_rem", "unit_trans"),
                            parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checking (done by match.arg)
    level <- match.arg(level)
    level <- switch(level,
                    Lvl_1 = 'Lvl_1',
                    Lvl_2 = 'Lvl_2',
                    out_warn = file.path('Lvl_2', 'lvl_2_out_warn'),
                    out_rem = file.path('Lvl_2', 'lvl_2_out_rem'),
                    unit_trans = file.path('Lvl_2', 'lvl_2_unit_trans'))

    # STEP 1
    # load the file
    file_name <- file.path('Data', si_code, level,
                           paste0(si_code, '.RData'))

    if (!file.exists(file_name)) {
      stop('SfnData for ', si_code, ' and ', level, ' does not exist.')
    } else {
      load(file = file_name)

      # 1.1 Return the SfnData object
      return(eval(as.name(si_code)))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_load_SfnData',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_load_SfnData',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_load_SfnData',
                                                        sep = '.'))})
}

################################################################################
#' Write SfnData
#'
#' Accesory function to write an SfnData object to a fixed location
#'
#' Given a site code and a level description, \code{df_write_SfnData} will save
#' the selected SfnData object in the selected location
#'
#' @family Data Flow
#'
#' @param SfnData SfnData object
#'
#' @param level Level to write to as a character string
#'
#' @return Nothing, the desired site data is saved.
#'
#' @export

# START
# Function declaration
df_write_SfnData <- function(SfnData, level = c("Lvl_1", "Lvl_2", "out_warn",
                                                "out_rem", "unit_trans"),
                             parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checking (done by match.arg)
    level <- match.arg(level)
    level <- switch(level,
                    Lvl_1 = 'Lvl_1',
                    Lvl_2 = 'Lvl_2',
                    out_warn = file.path('Lvl_2', 'lvl_2_out_warn'),
                    out_rem = file.path('Lvl_2', 'lvl_2_out_rem'),
                    unit_trans = file.path('Lvl_2', 'lvl_2_unit_trans'))
    # SfnData
    if (class(SfnData) != 'SfnData') {
      stop('object provided is not an SfnData object')
    }

    # STEP 1
    # file name
    # code <- deparse(substitute(SfnData))
    code <- unique(get_si_code(SfnData))
    file_name <- file.path('Data', code, level, paste0(code, '.RData'))

    # 1.1 Check if file exists
    if (file.exists(file_name)) {
      stop(code, ' object already exists in ', level)
    }

    # STEP 2
    # Write the object
    # 2.1 assign the code name to the object name
    assign(code, SfnData)

    # 2.2 write
    save(list = code, file = file_name)

    # STEP 3
    # Check for file and set status file
    # 3.1 Check if file exists
    if (!file.exists(file_name)) {
      stop('File has not been written, please revise manually')
    }

    # 3.2 Check if status must be changed
    if (level == file.path('Lvl_2', 'lvl_2_out_warn')) {
      # Update status
      df_set_status(code,
                    LVL1 = list(TO_LVL2 = 'DONE'),
                    LVL2 = list(STORED = TRUE, DATE = as.character(Sys.Date())),
                    parent_logger = parent_logger)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_write_SfnData',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_write_SfnData',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_write_SfnData',
                                                        sep = '.'))})
}

################################################################################
#' Interactively select the data sets ready to move to level 2
#'
#' Shiny app to flag data sets as ready to move to level 2
#'
#' @family apps
#'
#' @return An interactive app to flag datasets as ready to move to level 2
#'
#' @export
#'
#' @import shiny

# START
# Function declaration
df_flag_to_lvl2_app <- function(parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Defining the app
    shinyApp(

      # 1.1 UI
      ui = fluidPage(
        # app title
        titlePanel('Flag to level 2 app'),

        # main panel
        mainPanel(
          width  = 10,

          fluidRow(
            # input column (DT with the freeze)
            column(
              width = 4,
              DT::dataTableOutput('freeze')
            ),

            # action column (selected and action button)
            column(
              width = 3,
              br(),
              br(),
              br(),
              verbatimTextOutput('selected'),
              actionButton('flaggit', 'Flag it!',
                           icon = icon('flag')),
              br(),
              br(),
              'Select the desired rows in the left table to pass from "FREEZE"',
              ' to "READY" when "Flag it!" button is clicked.',
              br(),
              'Select the desired rows in the right table to undo the "READY"',
              ' status to "FREEZE" again when "Flag it!" button is clicked'
            ),

            # viewer column (DT with ready and done)
            column(
              width = 4,
              DT::dataTableOutput('ready')
            )
          )
        )
      ),

      # 1.2 server
      server = function(input, output) {

        # reactive expressions
        ## populate the tables
        pop_tables <- eventReactive(
          ignoreNULL = FALSE,
          eventExpr = input$flaggit,
          valueExpr = {
            clicked <- input$flaggit

            if (clicked == 0) {
              freeze_list <- df_who_ready_to_lvl2(filter = 'freeze',
                                                  parent_logger = parent_logger)
              ready_list <- df_who_ready_to_lvl2(filter = 'ready',
                                                 parent_logger = parent_logger)
              done_list <- df_who_ready_to_lvl2(filter = 'done',
                                                parent_logger = parent_logger)

              res <- list(freeze = freeze_list,
                          ready = ready_list,
                          done = done_list)

              res
            }

            selected <- input$freeze_rows_selected
            freeze_list <- df_who_ready_to_lvl2(filter = 'freeze',
                                                parent_logger = parent_logger)
            freeze_names <- names(freeze_list)[selected]
            purrr::walk(freeze_names, ~ df_set_status(
              .x, LVL1 = list(TO_LVL2 = 'READY')
            ))

            undo_sel <- input$ready_rows_selected
            ready_list <- df_who_ready_to_lvl2(filter = 'ready',
                                               parent_logger = parent_logger)
            ready_names <- names(ready_list)[undo_sel]
            purrr::walk(ready_names, ~ df_set_status(
              .x, LVL1 = list(TO_LVL2 = 'FREEZE')
            ))

            freeze_list <- df_who_ready_to_lvl2(filter = 'freeze',
                                                parent_logger = parent_logger)
            ready_list <- df_who_ready_to_lvl2(filter = 'ready',
                                               parent_logger = parent_logger)
            done_list <- df_who_ready_to_lvl2(filter = 'done',
                                              parent_logger = parent_logger)

            res <- list(freeze = freeze_list,
                        ready = ready_list,
                        done = done_list)

            res
          }
        )

        ## get the names
        get_names <- reactive({
          selected <- input$freeze_rows_selected
          freeze_list <- pop_tables()[['freeze']]
          freeze_names <- names(freeze_list)[selected]
          freeze_names
        })

        get_names_undo <- reactive({
          undo_sel <- input$ready_rows_selected
          ready_list <- pop_tables()[['ready']]
          ready_names <- names(ready_list)[undo_sel]
          ready_names
        })

        # freeze input table
        output$freeze <- DT::renderDataTable({
          freeze_list <- pop_tables()[['freeze']]
          freeze_table <- data.frame(
            Site = names(freeze_list),
            Status = purrr::flatten_chr(freeze_list),
            stringsAsFactors = FALSE
          )

          DT::datatable(
            freeze_table,
            colnames = c('Site', 'Status "TO_LVL2"'),
            extensions = 'Scroller',
            options = list(
              dom = 'ti',
              scrollY = 400,
              scrollX = '100%',
              scroller = TRUE
            ),
            selection = list(target = 'row')
          )
        })

        # sites selected text box
        output$selected <- renderPrint({
          cat("Selected sites ready for level 2:\n\n")
          cat(get_names(), sep = "\n")
          cat("\nSelected sites for undo:\n\n")
          cat(get_names_undo(), sep = "\n")
        })

        # ready input table
        output$ready <- DT::renderDataTable({
          ready_list <- pop_tables()[['ready']]
          ready_table <- data.frame(
            Site = names(ready_list),
            Status = purrr::flatten_chr(ready_list),
            stringsAsFactors = FALSE
          )

          DT::datatable(
            ready_table,
            colnames = c('Site', 'Status "TO_LVL2"'),
            extensions = 'Scroller',
            options = list(
              dom = 'ti',
              scrollY = 400,
              scrollX = '100%',
              scroller = TRUE
            ),
            selection = list(target = 'row')
          )
        })
      }
    )
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_flag_to_lvl2_app',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_flag_to_lvl2_app',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_flag_to_lvl2_app',
                                                        sep = '.'))})
}

################################################################################
#' Function to pass from level 1 to level 2
#'
#' LVL1 to LVL2 transfer
#'
#' This function is in charge of check for sites ready to pass to Level 2, create
#' the needed folder structure, flag for outliers warnings and saving the final
#' SfnData objects in the corresponding folder.
#'
#' @family Data Flow
#'
#' @return Nothing, all the process is internal
#'
#' @export

# START
# Function declaration
df_lvl1_to_lvl2 <- function(parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Get the ready sites list
    ready <- df_who_ready_to_lvl2(filter = "ready", parent_logger = parent_logger)

    # STEP 2
    # Move the data
    names(ready) %>%
      # 2.1 For each site, create the level 2 folder struture
      purrr::walk(~ df_lvl2_folder_structure(.x, parent_logger = parent_logger)) %>%
      # 2.2 read the sfnData objects
      purrr::map(~ df_read_SfnData(.x, level = 'Lvl_1', parent_logger = parent_logger)) %>%
      # 2.3 check for outliers
      purrr::map(~ qc_out_remove(.x, parent_logger = parent_logger)) %>%
      # 2.4 check for out of ranges values and flag them
      purrr::map(~ qc_out_of_range(.x, parent_logger = parent_logger)) %>%
      # 2.5 write the results
      purrr::walk(~ df_write_SfnData(.x, level = 'out_warn', parent_logger = parent_logger))
    # 2.6 update the status
    purrr::walk(names(ready), ~ df_set_status(.x, LVL2 = list(STEP = "WARN")))
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_lvl1_to_lvl2',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_lvl1_to_lvl2',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_lvl1_to_lvl2',
                                                        sep = '.'))})
}
