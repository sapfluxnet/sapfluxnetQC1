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
#' @export

# START
# Function declaration

remove_dupcols <- function(data, parent_logger = 'test') {

  # Using callin handlers to logging
  withCallingHandlers({

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
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'remove_dupcols', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'remove_dupcols', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'remove_dupcols', sep = '.'))})
}

################################################################################
#' Create a characters vector for NA values
#'
#' Generation of a character vector with the possible na characters found in
#' csv files in order to load data with data.table::fread
#'
#' Sometimes, csv files exported from excel files have some characters in the
#' cells, causing data columns to be interpreted as characters or factors
#' instead of numeric. This function compile an exhaustive list of those
#' characters in order to transform them in NAs at the moment of reading the
#' data.
#'
#' @family Data Loading Functions
#'
#' @return A character vector with the text strings to be converted to NAs, in
#'   order to use it in data.table::fread function.
#'
#' @export

dl_na_char_generator <- function(parent_logger = 'test') {

  # Use calling handlers to logging
  withCallingHandlers({

    # STEP 1
    # Return a character vector with values to be converted to NA
    return(c(
      # Usual
      '', 'NA',
      # Numeric NAs
      '-9999',
      # Excel errors (castilian)
      "#\xadVALOR!", "#\xadDIV/0!", "VERDADERO", 'FALSO', '#\xadREF!',
      # Excel errors (english)
      '#VALUE!', '#DIV/0!', 'TRUE', 'FALSE', '#REF!'
    )
    )

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'dl_na_char_generator', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'dl_na_char_generator', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'dl_na_char_generator', sep = '.'))})
}

################################################################################
#' Decimal character detection
#'
#' Decimal character detection to csv files loading with data.table::fread
#'
#' With data coming from any place in the world, data loading functions have to
#' cope with different decimal characters, separator characters...
#' \code{\link[data.table]{fread}} can autodetect separator character at loading
#' data, but decimal character must be specified in order to load correct data.
#' This function aims to provide an easy way to autodetect decimal character to
#' automatize data loading proccess.
#'
#' @section Algorithm:
#' The process to determine the decimal character is as follows:
#' \enumerate{
#'   \item 1000 rows and 4 variables are sampled from the data.
#'   \item For each variable, number of dots (\code{.}) and commas (\code{,})
#'   are calculated.
#'   \item If the number of dots is larger than the commas, dot is selected as
#'   decimal character. If the number of commas is larger than the dots, comma
#'   is selected as decimal character.
#' }
#'
#' @family Data Loading Functions
#'
#' @param file Character vector indicating the name (and route) of the file in
#'   which the decimal character must be guessed.
#'
#' @return A character vector containing the decimal character, to use in the
#'   fread order to load the data.
#'
#' @export

dl_dec_char_detect <- function(file, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({
    # STEP 0
    # Argument checking

    # STEP 1
    # Load file sample
    sample_data <- data.table::fread(
      file, skip = 'TIMESTAMP', data.table = FALSE, na.strings = dl_na_char_generator(),
      select = c(2,3,4,5), nrows = 1000, header = FALSE
    )

    # STEP 2
    # Sum of commas and dots
    commas <- sum(vapply(sample_data[,1], stringr::str_detect, logical(1),
                         pattern='(^.*\\d+\\,\\d+.*$)'), na.rm = TRUE) +
      sum(vapply(sample_data[,2], stringr::str_detect, logical(1),
                 pattern='(^.*\\d+\\,\\d+.*$)'), na.rm = TRUE) +
      sum(vapply(sample_data[,3], stringr::str_detect, logical(1),
                 pattern='(^.*\\d+\\,\\d+.*$)'), na.rm = TRUE) +
      sum(vapply(sample_data[,4], stringr::str_detect, logical(1),
                 pattern='(^.*\\d+\\,\\d+.*$)'), na.rm = TRUE)

    dots <- sum(vapply(sample_data[,1], stringr::str_detect, logical(1),
                       pattern='(^.*\\d+\\.\\d+.*$)'), na.rm = TRUE) +
      sum(vapply(sample_data[,2], stringr::str_detect, logical(1),
                 pattern='(^.*\\d+\\.\\d+.*$)'), na.rm = TRUE) +
      sum(vapply(sample_data[,3], stringr::str_detect, logical(1),
                 pattern='(^.*\\d+\\.\\d+.*$)'), na.rm = TRUE) +
      sum(vapply(sample_data[,4], stringr::str_detect, logical(1),
                 pattern='(^.*\\d+\\.\\d+.*$)'), na.rm = TRUE)

    # STEP 3
    # Deciding which is the decimal character adn returning it
    if (commas > dots) {
      return(',')
    }
    if (dots > commas) {
      return('.')
    }
    if (dots == commas) {
      stop('Dots & commas have the same appearance.',
           ' Unable to detect decimal character')
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'dl_dec_char_detect', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'dl_dec_char_detect', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'dl_dec_char_detect', sep = '.'))})
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

dl_metadata <- function(file_name, sheet_name,
                        si_code_loc = NULL, parent_logger = 'test'){

  # Using calling handlers to logging
  withCallingHandlers({

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
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'dl_metadata', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'dl_metadata', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'dl_metadata', sep = '.'))})

}

################################################################################
#' Loading data from xls/xlsx ann csv files
#'
#' Detecting file type and load data from files.
#'
#' This function make use of dplyr, tidyr and readxl packages in order to
#' retrieve and format the data. Also, in the case of csv files it uses
#' data.table package.
#'
#' @family Data Loading Functions
#'
#' @param file_name Character vector indicating the name of the file
#'   containing the data.
#'
#' @param sheet_name Character vector indicating the name of the sheet to be
#'   loaded. It must be one of \code{sapflow_hd} or \code{environmental_hd}. It
#'   is only used if origin file is an xlsx file
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

dl_data <- function(file_name, sheet_name, long = FALSE,
                    parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

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
    # Is xlsx?
    if (stringr::str_detect(file_name, "^.+\\.xls(x)?$")) {

      # STEP 2
      # Loading and shaping the data
      # 2.1 sapflow data
      if (sheet_name == 'sapflow_hd') {
        res <- readxl::read_excel(file_name, sheet_name, skip = 4) %>%
          # 2.1.2 Check and remove duplicate columns
          remove_dupcols() %>%
          # 2.1.3 Remove any extra column that could be created in the read_excel step.
          #       This is achieved with a regular expression indicating that we select
          #       those variables that have TIME or end with a number
          dplyr::select(matches("(TIME|^.+?\\d$)"))

        # 2.1.4 If long format is needed, gather time!!
        if (long) {
          res <- res %>%
            tidyr::gather(Plant, Sapflow_value, -TIMESTAMP)

          # 2.1.5 return long format
          return(res)

        } else {
          # or return wide format
          return(res)
        }

      } else {
        # 2.2 environmental data
        res <- readxl::read_excel(file_name, sheet_name, skip = 3) %>%
          # 2.2.2 check and remove duplicate columns
          remove_dupcols() %>%
          # 2.2.3 Remove any extra column that could be created in the read_excel step.
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
    }

    # STEP 3
    # Is csv file?
    if (stringr::str_detect(file_name, "^.+\\.csv$")) {

      # STEP 4
      # Loading and shaping the data
      # 4.1 sapflow data
      if (sheet_name == 'sapflow_hd') {
        res <- data.table::fread(
          file_name, skip = 'TIMESTAMP', data.table = FALSE,
          na.strings = dl_na_char_generator(),
          dec = dl_dec_char_detect(file_name)
        ) %>%
          remove_dupcols() %>%
          dplyr::select(matches('(TIME|^.+?\\d$)'))

        # 4.1.1 If long format is needed, gather time!!
        if (long) {
          res <- res %>%
            tidyr::gather(Plant, Sapflow_value, -TIMESTAMP)

          # 4.1.2 return long format
          return(res)

        } else {
          # or return wide format
          return(res)
        }
      } else {

        # 4.2 environmental data
        res <- data.table::fread(
          file_name, skip = 'TIMESTAMP', data.table = FALSE,
          na.strings = dl_na_char_generator(),
          dec = dl_dec_char_detect(file_name)
        ) %>%
          remove_dupcols() %>%
          dplyr::select(matches(
            "(TIME|ta|rh|vpd|sw_in|ppfd_in|netrad|ws|precip|swc_deep|swc_shallow)"
          ))

        # 4.2.1 If long format is needed, gather time!!
        if (long) {
          res <- res %>%
            tidyr::gather(Variable, Value, -TIMESTAMP)

          # 4.1.2 return long format
          return(res)

        } else {
          # or return wide format
          return(res)
        }
      }
    }
    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'dl_data', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'dl_data', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'dl_data', sep = '.'))})

}
