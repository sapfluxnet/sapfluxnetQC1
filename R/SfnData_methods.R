#' SfnData get methods
#'
#' Methods to get the info from the SfnData class slots
#'
#' @param object Object of class SfnData from which data is retrieved
#'
#' @param ... Further arguments to pass on to methods
#'
#' @name sfn_get_methods
#' @include SfnData_class.R SfnData_generics.R
NULL

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf", "SfnData",
  function(object) {
    slot(object, "sapf")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env", "SfnData",
  function(object) {
    slot(object, "env")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf_flags", "SfnData",
  function(object) {
    slot(object, "sapf_flags")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_flags", "SfnData",
  function(object) {
    slot(object, "env_flags")
  }
)

# #' @rdname sfn_get_methods
# #' @export
# setMethod(
#   "get_sapf_timestamp", "SfnData",
#   function(object) {
#     slot(object, "sapf_timestamp")
#   }
# )

# #' @rdname sfn_get_methods
# #' @export
# setMethod(
#   "get_env_timestamp", "SfnData",
#   function(object) {
#     slot(object, "env_timestamp")
#   }
# )

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_si_code", "SfnData",
  function(object) {
    slot(object, "si_code")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_site_md", "SfnData",
  function(object) {
    slot(object, "site_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_stand_md", "SfnData",
  function(object) {
    slot(object, "stand_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_species_md", "SfnData",
  function(object) {
    slot(object, "species_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_plant_md", "SfnData",
  function(object) {
    slot(object, "plant_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_md", "SfnData",
  function(object) {
    slot(object, "env_md")
  }
)

#' Show method for SfnData
#'
#' @param object SfnData object to show
#' @export
setMethod(
  "show", "SfnData",
  definition = function(object) {
    # object class
    cat(class(object), " object\n", sep = "")
    # site code
    cat("Data from ", get_si_code(object), " site\n", sep = "")
    # number of trees
    cat("Sapflow data contains ", length(names(get_sapf(object)[-1])),
        " trees/plants\n", sep = "")
    # env_vars
    cat("Environmental data variables:\n",
        paste(names(get_env(object)[-1])),
        "\n", sep = " ")
  }
)

#' Sub-setting operation
#'
#' @param i sapflow data row index
#' @param j sapflow data column index
#' @param k env data row index
#' @param l env data column index
#' @param object SfnData object
#'
#' @export
setMethod(
  "[", "SfnData",
  function(x, i, j, k, l, drop = "missing") {

    # subsetting the slots for subset
    .sapf <- slot(x, "sapf")[i, j]
    .env <- slot(x, "env")[k, l]

    # if no flags, create an empty data.frame
    if (nrow(get_sapf_flags(x)) < 1) {
      .sapf_flags <- data.frame()
    } else {
      .sapf_flags <- slot(x, "sapf_flags")[i, j]
    }

    if (nrow(get_env_flags(x)) < 1) {
      .env_flags <- data.frame()
    } else {
      .env_flags <- slot(x, "env_flags")[k, l]
    }

    # create the SfnData object, the metadata slots remain without modifications
    # as well as si_code
    SfnData(
      sapf = .sapf,
      env = .env,
      sapf_flags = .sapf_flags,
      env_flags = .env_flags,
      si_code = slot(x, "si_code"),
      site_md = slot(x, "site_md"),
      stand_md = slot(x, "stand_md"),
      species_md = slot(x, "species_md"),
      plant_md = slot(x, "plant_md"),
      env_md = slot(x, "env_md")
    )
  }
)

#' Replacement methods
#'
#' Methods for replacing the slots with new data or metadata
#'
#' @name sfn_replacement
NULL

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_sapf", "SfnData",
  function(object, value) {
    slot(object, "sapf") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_env", "SfnData",
  function(object, value) {
    slot(object, "env") <- value
    return(object)
  }
)
