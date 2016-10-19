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
    # data and timestamp
    .sapf <- slot(object, "sapf_data")
    .timestamp <- slot(object, "timestamp")

    # combining both
    res <- cbind(.timestamp, .sapf)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env", "SfnData",
  function(object) {
    # data and timestamp
    .env <- slot(object, "env_data")
    .timestamp <- slot(object, "timestamp")

    # combining both
    res <- cbind(.timestamp, .env)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf_flags", "SfnData",
  function(object) {
    .sapf_flags <- slot(object, "sapf_flags")
    .timestamp <- slot(object, "timestamp")

    # combining both
    res <- cbind(.timestamp, .sapf_flags)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_flags", "SfnData",
  function(object) {
    .env_flags <- slot(object, "env_flags")
    .timestamp <- slot(object, "timestamp")

    # combining both
    res <- cbind(.timestamp, .env_flags)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_timestamp", "SfnData",
  function(object) {
    slot(object, "timestamp")
  }
)

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
    cat("Data from ", unique(get_si_code(object)), " site/s\n", sep = "")
    # number of trees
    cat("Sapflow data:", nrow(slot(object, "sapf_data")), "observations of",
        length(names(slot(object, "sapf_data"))), "trees/plants\n")
    # env_vars
    cat("Environmental data:", nrow(slot(object, "env_data")), "observations.\n",
        "Env vars:", paste(names(slot(object, "env_data"))), "\n")
    # timestamp span
    cat("TIMESTAMP span, from", as.character(head(get_timestamp(object), 1)),
        "to", as.character(tail(get_timestamp(object), 1)))
  }
)

#' Sub-setting operation
#'
#' @param i data row index
#' @param j sapflow data column index
#' @param k env data column index
#' @param object SfnData object
#'
#' @export
setMethod(
  "[", "SfnData",
  function(x, i, j, k, drop = "missing") {

    # subsetting the slots for subset
    .sapf <- slot(x, "sapf_data")[i, j]
    .env <- slot(x, "env_data")[i, k]

    # if no flags, create an empty data.frame
    if (nrow(get_sapf_flags(x)) < 1) {
      .sapf_flags <- data.frame()
    } else {
      .sapf_flags <- slot(x, "sapf_flags")[i, j]
    }

    if (nrow(get_env_flags(x)) < 1) {
      .env_flags <- data.frame()
    } else {
      .env_flags <- slot(x, "env_flags")[i, k]
    }

    .timestamp <- slot(x, "timestamp")[i]
    .si_code <- slot(x, "si_code")[i]

    # create the SfnData object, the metadata slots remain without modifications
    # as well as si_code
    SfnData(
      sapf = .sapf,
      env = .env,
      sapf_flags = .sapf_flags,
      env_flags = .env_flags,
      timestmap = .timestamp,
      si_code = .si_code,
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
    slot(object, "sapf_data") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_env", "SfnData",
  function(object, value) {
    slot(object, "env_data") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_sapf_flags", "SfnData",
  function(object, value) {
    slot(object, "sapf_flags") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_env_flags", "SfnData",
  function(object, value) {
    slot(object, "env_flags") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_timestamp", "SfnData",
  function(object, value) {
    slot(object, "timestamp") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_si_code", "SfnData",
  function(object, value) {
    slot(object, "si_code") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_site_md", "SfnData",
  function(object, value) {
    slot(object, "site_md") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_stand_md", "SfnData",
  function(object, value) {
    slot(object, "stand_md") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_species_md", "SfnData",
  function(object, value) {
    slot(object, "species_md") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_plant_md", "SfnData",
  function(object, value) {
    slot(object, "plant_md") <- value
    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_env_md", "SfnData",
  function(object, value) {
    slot(object, "env_md") <- value
    return(object)
  }
)

#' Validity method for SfnData class
#'
#' @name sfn_validity
setValidity(
  "SfnData",
  function(object) {
    # initial values
    info <- NULL
    valid <- TRUE

    # check timestamp variable
    # if (is.null(get_sapf(object)$TIMESTAMP) | is.null(get_env(object)$TIMESTAMP)) {
    #   valid <- FALSE
    #   info <- c(info, 'No TIMESTAMP variable in sapf or env slots')
    # }

    # check dimensions
    if (any(
      nrow(get_sapf(object)) != nrow(get_env(object)),
      nrow(get_sapf(object)) != length(get_timestamp(object)),
      nrow(get_env(object)) != length(get_timestamp(object)),
      nrow(get_sapf(object)) != length(get_si_code(object)),
      nrow(get_env(object)) != length(get_si_code(object)),
      length(get_timestamp(object)) != length(get_si_code(object))
    )) {
      valid <- FALSE
      info <- c(info, 'dimensions are incorrect, they must fulfill "nrow(sapf_data) == nrow(env_data) == length(timestamp) == length(si_code)"')
    }

    # check if si_code is empty
    if (any(get_si_code(object) == '')) {
      valid <- FALSE
      info <- c(info, 'si_code slot can not be an empty string')
    }

    # check for metadata presence
    if (any(nrow(get_site_md(object)) < 1, nrow(get_stand_md(object)) < 1,
            nrow(get_species_md(object)) < 1, nrow(get_plant_md(object)) < 1,
            nrow(get_env_md(object)) < 1)) {
      valid <- FALSE
      info <- c(info, 'metadata slots can not be empty data frames')
    }

    # check for timestamp presence
    if (length(get_timestamp(object)) < 1) {
      valid <- FALSE
      info <- c(info, 'TIMESTAMP must be of length >= 1')
    }

    # check for si_code presence
    if (length(get_si_code(object)) < 1) {
      valid <- FALSE
      info <- c(info, 'si_code must be of length >= 1')
    }

    # insert more checks here

    # return validity or info
    if (valid) {
      return(TRUE)
    } else { return(info) }
  }
)
