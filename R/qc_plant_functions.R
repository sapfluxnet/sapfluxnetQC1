################################################################################
#' Check plant treatments for misspelling and concordance errors
#'
#' \code{qc_pl_treatments} function looks for spelling errors in treatments
#' provided in the plant metadata.
#'
#' This function summarizes the treatments declared in the plant metadata to
#' look for mispelling and concordance errors. At the moment, function only
#' returns an informative data frame, but the search for spelling errors and
#' fixes must be done at hand, as there is no automatized way of doing it.
#'
#' @family Quality Checks Functions
#'
#' @param plant_md Data frame containing the plant metadata
#'
#' @return A data frame with the information about the different treatments if
#'   any.
#'
#' @import dplyr
#'
#' @export

# START
# Function declaration

qc_pl_treatments <- function(plant_md) {

  # STEP 0
  # Argument checks
  # Is plant_md a data frame and have a pl_treatment variable?
  if (!is.data.frame(plant_md) | is.null(plant_md$pl_treatment)) {
    stop('Data object is not a data frame or it not contains any variable called pl_treatment')
  }

  # STEP 1
  # Check if pl_treatment is a NA vector (there are no treatments), so the
  # treatment comprobation is not necessary
  if (all(is.na(plant_md$pl_treatment))) {
    stop("No treatments found, all values for pl_treatment are NA's")
  }

  # STEP 2
  # Extract the unique treatments and summarise the results
  res <- plant_md %>%
    dplyr::select(pl_treatment) %>%
    dplyr::group_by(pl_treatment) %>%
    dplyr::arrange(pl_treatment) %>%
    dplyr::summarize(n = n())

  # STEP 3
  # Return the results data frame
  return(res)

  # END FUNCTION
}
