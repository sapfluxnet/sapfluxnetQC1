################################################################################
#' Check if provided email directions are correct
#'
#' \code{qc_email_check} function checks for correctness in the email fields
#'
#' Provided email direction/s (contact and additional contributor, if any) are
#' checked to ensure that are valid email directions. This function uses
#' stringr package utilities for character string tinkering and regex to match
#' the characteristics of email directions
#'
#' @section Regex:
#'   Email directions can be divided in three components, \code{username},
#'   \code{@} and \code{domain}. \code{username} component can contain
#'   any letter, number, underscores, dots and hyphens. \code{domain} component
#'   can be also any letter, number, underscores dots and hyphens, followed by
#'   a dot and from two to six letters and/or dots. So, the regex expression
#'   to match a email direction is as follows:
#'   \code{"^([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})$"}
#'
#' @family Quality Checks functions
#'
#' @param data Data frame containing the site metadata where email variables are
#'   located.
#'
#' @return A data frame with the directions and the result of the check
#'
#' @export

# START
# Function declaration
qc_email_check <- function(data) {

  # STEP 0
  # Argument checks
  # Has data valid email variables
  if (is.null(data$si_contact_email) & is.null(data$si_addcontr_email)) {
    stop('Data provided has not valid email variables')
  }

  # STEP 1
  # Initialize pattern
  emilio_pattern <- "^([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})$"

  # Initialize email directions object
  emilio_vec <- c(data$si_contact_email, data$si_addcontr_email)

  # STEP 2
  # Check the email
  emilio_res <- stringr::str_detect(string = emilio_vec, pattern = emilio_pattern)

  # STEP 3
  # Results object
  emilio_df <- data.frame(email = emilio_vec, Is_correct = emilio_res)

  # 3.1 return de results
  return(emilio_df)

  # END FUNCTION
}
