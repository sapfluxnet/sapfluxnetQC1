################################################################################
#' Verify provided species names (spelling and correctness)
#'
#' \code{qc_species_names} uses \code{tpl} package
#' (\code{\link{https://github.com/gustavobio/tpl}}) to verify the species
#' introduced by the contributors and fix spelling errors.
#'
#' This function takes a vector of species names and check if they are right
#' spelled. Also, if a synonym is used, the function changes it automatically in
#' order to have the same name for the same species.
#'
#' @section GitHub package:
#' \code{tpl} and \code{tpldata} are GitHub packages, and they are not available
#' for install in the ususal way. In order to achieve that
#' \code{qc_species_verification} works as expected, manual installation of
#' those two packages must be done previously:
#' \code{devtools::install_github("gustavobio/tpldata")}
#' \code{devtools::install_github("gustavobio/tpl")}
#'
#' @param data Data frame as the obtained from \code{\link{qc_species_names_info}}
#'
#' @return A character vector with species fixed in spelling and correctness.
#'
#' @import tpl
#'
#' @export

# START
# Function declaration

qc_species_names_fix <- function(data, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data frame
    if (!is.data.frame(data)) {
      stop('data provided is not a data frame')
    }
    # data has the needed variables
    if (is.null(data$data_names) | is.null(data$tpl_names) | is.null(data$Concordance) | is.null(data$IsNA)) {
      stop('data do not have needed variables: ',
           'data_names, tpl_names, Concordance and isNA')
    }

    # STEP 1
    # If tpl generated NAs, return the original species with a warning
    if (any(data$IsNA)) {
      warning('NAs have been generated, please try again with a lower value of conservatism')
      return(data$data_names)

      # STEP 2
      # If not, return the tpl names
    } else {
      return(data$tpl_names)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_names_fix', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_names_fix', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_names_fix', sep = '.'))})

}

################################################################################
#' Info of species names spelling
#'
#' Summary of species names spelling info
#'
#' @family Quality Checks Functions
#'
#' @param species Character vector containing the species names to verify
#'
#' @param conservatism Numerical value between 0 and 1 indicating the
#'   conservatism level of the tpl spelling check algorithm. Default to
#'   0.9
#'
#' @return A data frame summarizing the species names declared, the species
#'   names obtained after tpl and the concordance and NAs info
#'
#' @export

# START
# Function declaration
qc_species_names_info <- function(species, conservatism = 0.9,
                                  parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checking
    # Is species a character vector?
    if (!is.vector(species, 'character')) {
      stop('species object is not a character vector, please verify data object')
    }
    # Warning if conservatism is under 0.75
    if (conservatism < 0.75) {
      message('Conservatism value for spelling algorithm is under 0.75',
              ' and this can be cause of species name changes.',
              ' Maybe manual fix of some species should be done')
    }

    # STEP 1
    # Obtaining tpl info
    tpl_df <- tpl::tpl.get(species, replace.synonyms = FALSE,
                           suggestion.distance = conservatism)
    species_tpl <- tpl_df$name

    # STEP 2
    # Create the results data frame
    res <- data.frame(
      data_names = species,
      tpl_names = species_tpl,
      IsNA = is.na(species_tpl),
      Concordance = species == species_tpl,
      stringsAsFactors = FALSE
    )

    # STEP 3
    # Return the results
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_names_info', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_names_info', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_names_info', sep = '.'))})


}

################################################################################
#' Wrapper for species names check
#'
#' Wrapper for \code{\link{qc_species_names_info}} and \code{\link{qc_species_names_fix}}
#'
#' @family Quality Checks Functions
#'
#' @param species Character vector containing the species names to verify
#'
#' @param conservatism Numerical value between 0 and 1 indicating the
#'   conservatism level of the tpl spelling check algorithm. Default to
#'   0.9
#'
#' @return a vector with the fixed names of the species if fix is possible or
#'   needed, or a vector with the original names of the species if the fix is not
#'   possible or they are correct
#'
#' @export

# START
# Function declaration
qc_species_names <- function(species, conservatism = 0.9,
                             parent_logger= 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checking
    # No needed as the checks are already made in the internal functions

    # STEP 1
    # Get names and tpl info
    info <- qc_species_names_info(species, conservatism, parent_logger)

    # STEP 2
    # Return the names
    res <- qc_species_names_fix(info, parent_logger)
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_names', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_names', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_names', sep = '.'))})


}

################################################################################
#' Check if sp_name and sp_ntrees coincides with plants specified in plant_md
#'
#' \code{qc_species_verification} checks for coincidence in the number and species
#' names between species_md and plant_md
#'
#' In order to check if provided species metadata coincides with provided plant
#' metadata, species names and number of plants in each species are checked
#' against plant metadata.
#'
#' @section Coincidence:
#'   Possible values of \code{coincidence} column in results data frame are:
#'   \code{TRUE}, indicating both metadata have the same species and the number
#'   of trees are the same; \code{FALSE} indicates that both metadata have the
#'   same species, but the number of trees are no the same; finally,
#'   \code{NA} indicates that the species are not the same in both metadata.
#'
#' @family Quality Checks Functions
#'
#' @param species_md Data frame containing species metadata.
#'
#' @param plant_md Data frame containing plant metadata.
#'
#' @return A data frame with the species provided in plant and species metadata,
#'   as well as the number of trees of each species in both metadata.
#'   \code{coincidence} column indicates result of checking both numbers and
#'   presence of the same species.
#'
#' @export

# START
# Function declaration

qc_species_verification <- function(species_md, plant_md,
                                    parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(species_md) | !is.data.frame(plant_md)) {
      message('One or both metadata objects is/are not data frame/s')
    }

    # STEP 1
    # Extract number and species names information from species_md
    sp_md <- species_md %>%
      dplyr::select(sp_name, sp_ntrees) %>%
      dplyr::rename(sp_names = sp_name, sp_n_trees = sp_ntrees) %>%
      dplyr::arrange(sp_names)

    # STEP 2
    # Extract number and species names information from plant_md
    pl_md <- plant_md %>%
      dplyr::select(pl_species) %>%
      dplyr::group_by(pl_species) %>%
      dplyr::summarize(pl_n_trees = n()) %>%
      dplyr::rename(sp_names = pl_species) %>%
      dplyr::arrange(sp_names)

    # STEP 3
    # Compare both metadata to look for errors and generate result object
    res <- dplyr::full_join(sp_md, pl_md, by = 'sp_names') %>%
      dplyr::mutate(Concordance = (sp_n_trees == pl_n_trees))

    # STEP 4
    # Return the results object
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_verification', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_verification', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_verification', sep = '.'))})

}
