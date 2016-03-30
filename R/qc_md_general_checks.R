#' Site metadata column check
#'
#' \code{qc_site_col} checks if all the site metadata columns were
#' created and their classes setted correctly
#'
#' First qaulity check before continue to more complex checks. Here we are
#' simply testing if all the columns are there with the correct classes
#' (numerical for numerical values, character for text fields, etc.).
#'
#' @family Quality Checks Functions
#'
#' @param data Site metadata as obtained from \code{\link{dl_metadata}}
#'
#' @return A data frame with variables in one column and results of checks in
#'   the following columns
#'
#' @export

# START
# Function declaration

qc_site_col <- function(data) {

  # STEP 0
  # Arguments checking


  # STEP 1
  # Data dictionary to allow checks
  dic <- data.frame(
    variables = c(''),
    class = c('')
  )

  # STEP 2
  # Checks

  # 2.1 Are there all the variables?
  if (!identical(sort(names(data), sort(dic$variables)))) {
    message('Oh oh! Something is wrong here!')
  }
}
