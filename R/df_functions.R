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
#'   the structure must be built. Default to current working directory.
#'
#' @return Nothing, dirs are created if they do not exist, or a warning is raised
#'   if they already exist.
#'
#' @export

# START
# Function declaration
df_folder_structure <- function(parent_dir = getwd()) {

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
