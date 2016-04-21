################################################################################
#' Remove columns with duplicate names
#'
#' \code{remove_dupcols} is an internal function to use inside of the \code{dl_*}
#' functions. It checks for duplicate column names and drop them
#'
#' When exporting from excel files, sometimes cell fomatting makes empty columns
#' and rows to be read and loaded in R. If that is the case, any try to transform
#' and to shape the data faces a "duplicate column names error". This can be
#' solved by the correct formatting of the excel files, but this can not be
#' always achieved, hence this function.
#'
#' @family Data Loading Functions
#'
#' @param data Data frame in which check for duplicate column names
#'
#' @return \code{remove_dupcols} returns the loaded data with duplicate columns
#'   removed, if any.
#'

# START
# Function declaration

remove_dupcols <- function(data) {

  # STEP 0
  # Argument checking
  # data is a data_frame
  if (!is.data.frame(data)) {
    stop('Data is not a data frame')
  }

  # STEP 1
  # Check for duplicate columns and drop them if any
  if (any(duplicated(names(data)))) {
    res <- data[!duplicated(names(data))]
    return(res)
  } else { return(data) }

  # END FUNCTION
}

################################################################################
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
#' @param si_code_loc Name of the object containing the site metadata, in order
#'   to obtain si_code variable to include it in other metadata objects. Default
#'   to \code{NULL}, as the first metadata to load must be the site metadata.
#'
#' @return The function returns a data_frame with the corresponding metadata
#'   (site, stand, species, plant or environmental) in "wide" format, with
#'   metadata variables as columns, ready to be feeded to quality check
#'   functions.
#'
#' @importFrom magrittr %>%
#'
#' @export

# START
# Function declaration

dl_metadata <- function(file_name, sheet_name, si_code_loc = NULL){

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

  # check for si_code_loc and if NULL set si_code to NA
  if (is.null(si_code_loc)) {
    si_code_txt <- NA
    message('si_code_loc set to NULL. If loading other metadata than site_md,
            please indicate the object containing the site metadata.')
  } else {
    si_code_txt <- si_code_loc$si_code[1]
  }

  # STEP 1
  # Load metadata and tidy it

  # 1.1 If sheet is site_md we have to skip first line
  if (sheet_name == 'site_md') {
    # read the sheet
    res <- readxl::read_excel(file_name, sheet_name, skip = 1) %>%
      # check for duplicate columns
      remove_dupcols() %>%
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
      # check for duplicate columns
      remove_dupcols() %>%
      # select only the name and the value of the variables
      dplyr::select(Variable, Value) %>%
      # in case of the read_excel function gets more rows filled with NA's, remove them
      dplyr::filter(!is.na(Variable)) %>%
      # spread the variables to their own columns, getting back their class
      tidyr::spread(Variable, Value, convert = TRUE) %>%
      # adding the si_code
      dplyr::mutate(si_code = si_code_txt)

    # 1.2.1 return the stand or environmental metadata
    return(res)
  }

  # 1.3 If sheet is plant_md we need to take extra steps to tidy the data
  if (sheet_name == 'plant_md') {
    # read the sheet
    res <- readxl::read_excel(file_name, sheet_name) %>%
      # check for duplicate columns
      remove_dupcols() %>%
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
                      !is.na(pl_dbh)) %>%
      # adding the si_code
      dplyr::mutate(si_code = si_code_txt)

    # 1.3.1 return the plant metadata
    return(res)
  }

  # 1.4 If sheet is species_md, we need to take the same extra steps as before,
  #     but with different final filter
  if (sheet_name == 'species_md') {
    # read the sheet
    res <- readxl::read_excel(file_name, sheet_name) %>%
      # check for duplicate columns
      remove_dupcols() %>%
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
                      !is.na(sp_name) | !is.na(sp_ntrees)) %>%
      # adding the si_code
      dplyr::mutate(si_code = si_code_txt)

    # 1.4.1 return the species metadata
    return(res)
  }

  # END FUNCTION
}

################################################################################
#' Loading data from xls/xlsx
#'
#' This function make use of dplyr, tidyr and readxl packages in order to
#' retrieve and format the data.
#'
#' In case of data to be in the xls/xlsx file, instead of separate csv files,
#' this function is used to retrieve the raw data (sapflow and environmental).
#'
#' @family Data Loading Functions
#'
#' @param file_name Character vector indicating the name of the xls/xlsx file
#'   containing the data.
#'
#' @param sheet_name Character vector indicating the name of the sheet to be
#'   loaded. It must be one of \code{sapflow_hd} or \code{environmental_hd}.
#'
#' @param long Logical indicating if returned data must be in \code{long} format
#'
#' @return \code{dl_data} returns sapflow or environmental data in wide format,
#'   ready to pipe it in the quality checks for raw data. If \code{long = TRUE}
#'   data is returned in long format, ready for plotting.
#'
#' @importFrom magrittr %>%
#'
#' @export

# START
# Function declaration

dl_data <- function(file_name, sheet_name, long = FALSE) {

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
  accepted_sheets <- c('sapflow_hd', 'environmental_hd')

  if (!is.character(sheet_name) || !(sheet_name %in% accepted_sheets)) {
    stop('Provided sheet name is not a character or is not a metadata sheet
         Please see function help')
  }

  # STEP 1
  # Loading and shaping the data

  # 1.1 sapflow data
  if (sheet_name == 'sapflow_hd') {
    res <- readxl::read_excel(file_name, sheet_name, skip = 4) %>%
      # 1.1.2 Check and remove duplicate columns
      remove_dupcols() %>%
      # 1.1.3 Remove any extra column that could be created in the read_excel step.
      #       This is achieved with a regular expression indicating that we select
      #       those variables that have TIME or end with a number
      dplyr::select(matches("(TIME|^.+?\\d$)"))

    # 1.1.4 If long format is needed, gather time!!
    if (long) {
      res <- res %>%
        tidyr::gather(Plant, Sapflow_value, -TIMESTAMP)

      # 1.1.5 return long format
      return(res)

    } else {
      # or return wide format
      return(res)
    }

  } else {
    # 1.2 environmental data
    res <- readxl::read_excel(file_name, sheet_name, skip = 3) %>%
      # 1.2.2 check and remove duplicate columns
      remove_dupcols() %>%
      # 1.2.3 Remove any extra column that could be created in the read_excel step.
      #       This is achieved with a regular expression indicating that we select
      #       the environmental variables and the TIMESTAMP
      dplyr::select(matches(
        "(TIME|ta|rh|vpd|sw_in|ppfd_in|netrad|ws|precip|swc_deep|swc_shallow)"
      ))

    # 1.2.4 If long format is needed, gather time!!
    if (long) {
      res <- res %>%
        tidyr::gather(Variable, Value, -TIMESTAMP)

      # 1.2.5 return long format
      return(res)

    } else {
      # or return wide format
      return(res)
    }
  }

  # END FUNCTION
}
