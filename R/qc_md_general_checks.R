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
  # Data dictionary to allow checks and vector with names of data
  dic <- data.frame(
    variables = c('si_name', 'si_country', 'si_contact_firstname',
                  'si_contact_lastname', 'si_contact_email', 'si_contact_institution',
                  'si_addcontr_firstname', 'si_addcontr_lastname',
                  'si_addcontr_email', 'si_addcontr_institution', 'si_lat',
                  'si_long', 'si_elev', 'si_igbp', 'si_paper', 'si_dist_mgmt',
                  'si_flux_network', 'si_dendro_network', 'si_remarks', 'si_code'),
    class = c('character', 'character', 'character',
              'character', 'character', 'character',
              'character', 'character',
              'character', 'character', 'numeric',
              'numeric', 'numeric', 'character', 'character', 'character',
              'logical', 'logical', 'character', 'character')
  )

  data_variables <- names(data)

  # 1.1 Initiate result objects
  presence_res <- vector()
  classes_res <- vector()

  # STEP 2
  # Checks (presence and class)
  for (name in data_variables) {
    p_res <- name %in% dic$variables
    presence_res <- c(presence_res, p_res)
    c_res <- identical(class(data[[name]]),
                       as.character(dic$class[dic$variable == name]))
    classes_res <- c(classes_res, c_res)
  }

  # STEP 3
  # Create and return the result object
  result <- data.frame(Variable = data_variables,
                       PresenceOK = presence_res,
                       ClassOK = classes_res)

  return(result)

  # END FUNCTION
}
