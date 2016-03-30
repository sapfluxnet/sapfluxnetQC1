#' Loading metadata from xls/xlsx
#'
#' \code{dl_metadata} function loads the metadata sheets from xls/xlsx file.
#'
#' This function make use of dplyr, tidyr and readxl packages in order to
#' retrieve and format the metadata. It's intended to be used as first step
#' to load the metadata and start the metadata quality check.
#'
#' @family Data Loading Functions
#'
#' @param file_name Character vector indicating the name of the xls/xlsx file
#'   containing the metadata.
#'
#' @param sheet_name Character vector indicating the name of the sheet to be
#'   loaded. It must be one of \code{site_md}, \code{stand_md}, \code{species_md},
#'   \code{plant_md} or \code{environmental_md}.
#'
#' @return The function returns a data_frame with the corresponding metadata
#'   (site, stand, species, plant or environmental) in "wide" format, with
#'   metadata variables as columns, ready to be feeded to quality check
#'   functions.
#'
#' @import dplyr
#' @import tidyr
#' @import readxl
#'
#' @export

# START
# Function declaration

dl_metadata <- function(file_name, sheet_name){

  # STEP 0
  # Argument checking

  # check if file name is a character and it exist
  if (!is.character(file_name)) {
    stop('File name is not provided as character')
  }

  if (!file_test("-f", file_name)) {
    stop('File does not exist, please check if file name has been correctly provided')
  }

  # check if sheet name is one of the five kinds of metadata allowed
  accepted_sheets <- c('site_md', 'stand_md', 'species_md',
                       'plant_md', 'environmental_md')

  if (!is.character(sheet_name) || !(sheet_name %in% accepted_sheets)) {
    stop('Provided sheet name is not a character or is not a metadata sheet
         Please see function help')
  }

  # STEP 1
  # Load metadata and tidy it

  # 1.1 If sheet is site_md we have to skip first line
  if (sheet_name == 'site_md') {
    # read the sheet
    res <- readxl::read_excel(file_name, sheet_name, skip = 1) %>%
      # select only the name and the value of the variables
      dplyr::select(Variable, Value) %>%
      # in case of the read_excel function gets more rows filled with NA's, remove them
      dplyr::filter(!is.na(Variable)) %>%
      # spread the variables to their own columns, getting back their class
      tidyr::spread(Variable, Value, convert = TRUE)

    # 1.1.1 return the site metadata
    return(res)
  }

  # 1.2 If sheet is stand or environmental, load as it is
  if (any(sheet_name == 'stand_md', sheet_name == 'environmental_md')) {
    # read the sheet
    res <- readxl::read_excel(file_name, sheet_name) %>%
      # select only the name and the value of the variables
      dplyr::select(Variable, Value) %>%
      # in case of the read_excel function gets more rows filled with NA's, remove them
      dplyr::filter(!is.na(Variable)) %>%
      # spread the variables to their own columns, getting back their class
      tidyr::spread(Variable, Value, convert = TRUE)

    # 1.2.1 return the stand or environmental metadata
    return(res)
  }

  # 1.3 If sheet is plant_md we need to take extra steps to tidy the data
  if (sheet_name == 'plant_md') {
    # read the sheet
    res <- readxl::read_excel(file_name, sheet_name) %>%
      # select only the name and the value of the variables
      dplyr::select(-Description, -Units) %>%
      # in case of the read_excel function gets more rows filled with NA's, remove them
      dplyr::filter(!is.na(Variable)) %>%
      # gather the species columns in one to be able to spread it afterwards
      tidyr::gather('Indexes', 'Values', -Variable) %>%
      # spread the variables to their own columns, getting back their class
      tidyr::spread(Variable, Values, convert = TRUE) %>%
      # clean the resulting data
      dplyr::select(-Indexes) %>%
      dplyr::filter(!is.na(pl_age) | !is.na(pl_azimut_int) |
                      !is.na(pl_bark_thick) | !is.na(pl_code) |
                      !is.na(pl_dbh))

    # 1.3.1 return the plant metadata
    return(res)
  }

  # 1.4 If sheet is species_md, we need to take the same extra steps as before,
  #     but with different final filter
  if (sheet_name == 'species_md') {
    # read the sheet
    res <- readxl::read_excel(file_name, sheet_name) %>%
      # select only the name and the value of the variables
      dplyr::select(-Description, -Units) %>%
      # in case of the read_excel function gets more rows filled with NA's, remove them
      dplyr::filter(!is.na(Variable)) %>%
      # gather the species columns in one to be able to spread it afterwards
      tidyr::gather('Indexes', 'Values', -Variable) %>%
      # spread the variables to their own columns, getting back their class
      tidyr::spread(Variable, Values, convert = TRUE) %>%
      # clean the resulting data
      dplyr::select(-Indexes) %>%
      dplyr::filter(!is.na(sp_basal_area_perc) | !is.na(sp_leaf_habit) |
                      !is.na(sp_name) | !is.na(sp_ntrees))

    # 1.4.1 return the species metadata
    return(res)
  }

  # END FUNCTION
}
