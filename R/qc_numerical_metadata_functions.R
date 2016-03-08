#' Find if provided values of a numerical variable are in a suitable range
#'
#' \code{qc_suitable_range} is used to determine if a variable has strange
#' values out of natural or biological expected range.
#'
#' Checks are made in function of the variable and acceptable ranges provided.
#' \code{variables} and \code{ranges} must be of the same length, as the first
#' variable is compared with the first range provided, and so on. If both
#' objects are not of the same length, an informative error appears and function
#' stops.
#'
#' @family Quality Check Functions
#'
#' @param data Data frame containing the variables to check
#'
#' @param variables Character vector with variables names
#'
#' @param ranges List in the form of
#'   \code{list("variable1" = c(min, max), "variable2" = c(min, max)...)}
#'   indicating the suitable ranges for every variable contained in
#'   \code{variable} (See details).
#'
#' @return A logical vector indicating if variables are in range
#'
#' @export


# START
# function declaration

qc_suitable_range <- function(data, variables, ranges) {

  # STEP 0
  # Argument checks
  # Is ranges a list?
  if (!is.list(ranges)) {
    stop('ranges argument provided is not a list, please provide a list in
         the form of list("variable1" = c(min, max), "variable2" = c(min, max)...)')
  }
  # Is data a data.frame?
  if (!is.data.frame(data)) {
    stop('provided data is not a data.frame, please review data object provided')
  }
  # Is variables a character vector?
  if (!is.character(variables)) {
    stop('variables are not provided as character vector, please review
         variables object')
  }
  # Are variables and ranges of the same length?
  if (length(variables) != length(ranges)) {
    stop('variables vector and ranges list are not of the same length, please
         review provided objects')
  }
  # Are variables present in data?
  if (!all(variables %in% data)) {
    stop('one or more variables are nor present in data, please make sure that
         provided data and variables are correct')
  }

  # STEP 1
  # Extract the variable values
  var_values <- lapply(variables, function(x) {
    data[[x]]
  })

  # STEP 2
  # Check if values are in range

  # 2.1 Empty results vector
  res <- vector()
  # 2.2 For loop to see if each variable value is in range
  for (i in 1:length(var_values)) {
    tmp_res <- var_values[i] >= ranges[[i]][1] && var_values[i] <= ranges[[i]][2]
    res <- c(res, tmp_res)
  }

  # STEP 3
  # Return the results
  return(res)

# END FUNCTION
}
