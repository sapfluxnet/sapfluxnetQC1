################################################################################
#' Check plant treatments for misspelling and concordance errors
#'
#' \code{qc_pl_treatments} function looks for spelling errors in treatments
#' provided in the plant metadata.
#'
#' This function recopiles all declared treatments and look for mispelling errors
#' in the treatments (i.e. \code{Drought} and \code{drought}) that can be cause
#' of later analysis errors.
#'
#' @family Quality Checks Functions
#'
#' @param plant_md Data frame containing the plant metadata
#'
#' @return A data frame with the information about the different treeatments if
#'   any
#'
#' @import dplyr
#'
#' @export

# START
# Function declaration

qc_pl_treatments <- function(plant_md) {

  # STEP 0
  # Argument checks

  # STEP 1
  # Check if pl_treatment is a NA vector (there are no treatments), so this check
  # is unnecessary
  if (all(is.na(plant_md$pl_treatment))) {
    stop('No treatments found')
  }

  # STEP 2
  # Extract the unique treatments and summarise the results
  res <- plant_md %>%
    dplyr::select(pl_treatment) %>%
    dplyr::group_by(pl_treatment) %>%
    dplyr::arrange(pl_treatment) %>%
    dplyr::summarize(n = n())

}
