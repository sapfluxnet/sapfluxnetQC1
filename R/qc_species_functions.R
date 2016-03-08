#' Verify provided species names (spelling and correctness)
#'
#' \code{qc_species_verification} uses \code{tpl} package
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

qc_species_verification <- function(species, conservatism = 0.9) {

  # STEP 0
  # Argument checks
  # Is species a character vector?
  if (!is.vector(species, 'character')) {
    stop('species object is not a character vector, please verify data object')
  }
  # Warning if conservatism is under 0.75
  if (conservatism < 0.75) {
    message('Conservatism value for spelling algorithm is under 0.75',
            ' and this can be cause of specie name changes.',
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
