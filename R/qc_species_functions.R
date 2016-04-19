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
#' @param species Character vector containing the species names to verify
#'
#' @param conservatism Numerical value between 0 and 1 indicating the
#'   conservatism level of the tpl spelling check algorithm. Default to
#'   0.75
#'
#' @return A character vector with species fixed in spelling and correctness.
#'
#' @import tpl
#'
#' @export

# START
# Function declaration

qc_species_names <- function(species, conservatism = 0.9) {

  # STEP 0
  # Argument checks
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
  # Check species in The Plant List with tpl package
  # suggestion.distance can be set to 0.75 to be a little less conservative
  # than in the default options, but be careful, a lower value can be source of
  # specie change.
  res_df <- tpl::tpl.get(species, suggestion.distance = conservatism)

  # STEP 2
  # Be sure that no NAs have been introduced
  if (any(is.na(res_df$name))) {
    stop('NAs have been generated, please try again with a lower value of conservatism')
  } else {

    # STEP 3
    # Return the character vector with the names corrected
    return(res_df$name)
  }

# END FUNCTION
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

qc_species_verification <- function(species_md, plant_md) {

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
    dplyr::mutate(coincidence = (sp_n_trees == pl_n_trees))

  # STEP 4
  # Return the results object
  return(res)

  # END FUNCTION
}
