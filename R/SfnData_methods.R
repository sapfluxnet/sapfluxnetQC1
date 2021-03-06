#' SfnData get methods
#'
#' Methods to get the info from the SfnData class slots
#'
#' \code{get_sapf} and \code{get_env} methods retrieve sapflow or environmental
#' data and timestamp to create a functional dataset to work with.
#'
#' \code{get_sapf_flags} and \code{get_env_flags} methods retrieve sapflow or
#' environmental flags also with the timestamp.
#'
#' \code{get_timestamp} method retrieve only the timestamp as POSIXct vector.
#'
#' \code{get_si_code} method retrieve a character vector with length(timestamp)
#' containing the site code.
#'
#' \code{get_site_md}, \code{get_stand_md}, \code{get_species_md},
#' \code{get_plant_md} and \code{get_env_md} methods retrieve the corresponding
#' metadata.
#'
#' @param object Object of class SfnData from which data is retrieved
#'
#' @param solar Logical indicating if the timestamp to return in the get_sapf,
#'   get_env, get_sapf_flags and get_env_flags methods
#'
#' @name sfn_get_methods
#' @include SfnData_class.R SfnData_generics.R
NULL

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf", "SfnData",
  function(object, solar = FALSE) {
    # data
    .sapf <- slot(object, "sapf_data")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .sapf)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env", "SfnData",
  function(object, solar = FALSE) {
    # data
    .env <- slot(object, "env_data")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .env)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf_flags", "SfnData",
  function(object, solar = FALSE) {
    .sapf_flags <- slot(object, "sapf_flags")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .sapf_flags)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_flags", "SfnData",
  function(object, solar = FALSE) {
    .env_flags <- slot(object, "env_flags")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .env_flags)

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
  "get_solar_timestamp", "SfnData",
  function(object) {
    slot(object, "solar_timestamp")
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
    cat("Data from ", unique(get_si_code(object)), " site/s\n\n", sep = "")
    # number of trees
    cat("Sapflow data: ", nrow(slot(object, "sapf_data")), " observations of ",
        length(names(slot(object, "sapf_data"))), " trees/plants\n\n")
    # env_vars
    cat("Environmental data: ", nrow(slot(object, "env_data")), " observations.\n",
        "Env vars: ", paste(names(slot(object, "env_data"))), "\n\n")
    # timestamp span
    cat("TIMESTAMP span, from ", as.character(head(get_timestamp(object), 1)),
        "to ", as.character(tail(get_timestamp(object), 1)), "\n\n")

    # solar_timestamp
    cat("Solar TIMESTAMP available: ", !is.null(get_solar_timestamp(object)),
        "\n\n")

    # sapf_flags
    sapf_flags <- unique(unlist(stringr::str_split(unlist(lapply(slot(object, "sapf_flags"), unique)), '; ')))
    sapf_flags_table <- vapply(sapf_flags, function(flag){sum(stringr::str_count(as.matrix(slot(object, "sapf_flags")), flag))}, numeric(1))
    sapf_flags_table <- sapf_flags_table[names(sapf_flags_table) != '']
    cat("Sapflow data flags:\n")
    if (length(sapf_flags_table)) {
      print(sort(sapf_flags_table))
    } else {cat("No flags present")}
    cat("\n")

    # env_flags
    env_flags <- unique(unlist(stringr::str_split(unlist(lapply(slot(object, "env_flags"), unique)), '; ')))
    env_flags_table <- vapply(env_flags, function(flag){sum(stringr::str_count(as.matrix(slot(object, "env_flags")), flag))}, numeric(1))
    env_flags_table <- env_flags_table[names(env_flags_table) != '']
    cat("Environmental data flags:\n")
    if (length(env_flags_table)) {
      print(sort(env_flags_table))
    } else {cat("No flags present")}
    cat("\n")

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
  "[", signature(x = "SfnData", i = "numeric", j = "ANY", drop = "missing"),
  function(x, i, j, k) {

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

    TIMESTAMP <- slot(x, "timestamp")[i]
    .solar_timestamp <- slot(x, "solar_timestamp")[i]
    .si_code <- slot(x, "si_code")[i]

    # create the SfnData object, the metadata slots remain without modifications
    # as well as si_code
    SfnData(
      sapf_data = .sapf,
      env_data = .env,
      sapf_flags = .sapf_flags,
      env_flags = .env_flags,
      timestamp = TIMESTAMP,
      solar_timestamp = .solar_timestamp,
      si_code = .si_code,
      site_md = slot(x, "site_md"),
      stand_md = slot(x, "stand_md"),
      species_md = slot(x, "species_md"),
      plant_md = slot(x, "plant_md"),
      env_md = slot(x, "env_md")
    )
  }
)

#' plot SfnData method
#'
#' @param object SfnData object
#' @param type what to plot
#' @param solar use solarTIMESTAMP?
#'
#' @export
setMethod(
  'plot', c('SfnData', 'missing'),
  function(x,
           type = c('sapf', 'env',
                    'ta', 'rh', 'vpd', 'ppfd_in', 'netrad', 'sw_in', 'ext_rad',
                    'ws', 'precip', 'swc_shallow', 'swc_deep'),
           solar = FALSE) {
    # get the type with match argument
    type <- match.arg(type)

    # sapf
    if (type == 'sapf') {
      data <- get_sapf(x, solar)
      units_char <- get_plant_md(x)[['pl_sap_units']][1]

      # actual plot
      res_plot <- data %>%
        tidyr::gather(Tree, Sapflow, -TIMESTAMP) %>%
        ggplot(aes(x = TIMESTAMP, y = Sapflow, colour = Tree)) +
        geom_point(alpha = 0.4) +
        labs(y = paste0('Sapflow [', units_char, ']')) +
        scale_x_datetime() +
        facet_wrap('Tree', ncol = 3, scale = 'fixed')
    }

    # env
    if (type == 'env') {
      data <- get_env(x, solar)

      # actual plot
      res_plot <- data %>%
        tidyr::gather(Variable, Value, -TIMESTAMP) %>%
        ggplot(aes(x = TIMESTAMP, y = Value, colour = Variable)) +
        geom_point(alpha = 0.4) +
        scale_x_datetime() +
        facet_wrap('Variable', ncol = 3, scale = 'free_y')
    }

    # ta
    if (type == 'ta') {
      data <- get_env(x, solar)

      if (is.null(data[['ta']])) {
        stop('Site has not ta data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = ta)) +
        geom_point(alpha = 0.4, colour = '#C0392B') +
        labs(y = 'Air Temperature [C]') +
        scale_x_datetime()
    }

    # rh
    if (type == 'rh') {
      data <- get_env(x, solar)

      if (is.null(data[['rh']])) {
        stop('Site has not rh data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = rh)) +
        geom_point(alpha = 0.4, colour = '#6BB9F0') +
        labs(y = 'Relative Humidity [%]') +
        scale_x_datetime()
    }

    # vpd
    if (type == 'vpd') {
      data <- get_env(x, solar)

      if (is.null(data[['vpd']])) {
        stop('Site has not vpd data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = vpd)) +
        geom_point(alpha = 0.4, colour = '#6BB9F0') +
        labs(y = 'VPD [kPa]') +
        scale_x_datetime()
    }

    # ppfd_in
    if (type == 'ppfd_in') {
      data <- get_env(x, solar)

      if (is.null(data[['ppfd_in']])) {
        stop('Site has not ppfd_in data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = ppfd_in)) +
        geom_point(alpha = 0.4, colour = '#D35400') +
        labs(y = 'PPFD [?]') +
        scale_x_datetime()
    }

    # sw_in
    if (type == 'sw_in') {
      data <- get_env(x, solar)

      if (is.null(data[['sw_in']])) {
        stop('Site has not sw_in data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = sw_in)) +
        geom_point(alpha = 0.4, colour = '#E87E04') +
        labs(y = 'sw [?]') +
        scale_x_datetime()
    }

    # netrad
    if (type == 'netrad') {
      data <- get_env(x, solar)

      if (is.null(data[['netrad']])) {
        stop('Site has not netrad data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = netrad)) +
        geom_point(alpha = 0.4, colour = '#EB9532') +
        labs(y = 'Net Radiation [?]') +
        scale_x_datetime()
    }

    # ext_rad
    if (type == 'ext_rad') {
      data <- get_env(x, solar)

      if (is.null(data[['ext_rad']])) {
        stop('Site has not ext_rad data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = ext_rad)) +
        geom_point(alpha = 0.4, colour = '#F89406') +
        labs(y = 'Extraterrestrial Radiation [?]') +
        scale_x_datetime()
    }

    # ws
    if (type == 'ws') {
      data <- get_env(x, solar)

      if (is.null(data[['ws']])) {
        stop('Site has not ws data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = ws)) +
        geom_col(alpha = 0.4, fill = '#674172') +
        labs(y = 'Wind Speed [m/s]') +
        scale_x_datetime()
    }

    # precip
    if (type == 'precip') {
      data <- get_env(x, solar)

      if (is.null(data[['precip']])) {
        stop('Site has not precip data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = precip)) +
        geom_col(alpha = 0.4, fill = '#67809F') +
        labs(y = 'Precipitation [?]') +
        scale_x_datetime()
    }

    # swc_shallow
    if (type == 'swc_shallow') {
      data <- get_env(x, solar)

      if (is.null(data[['swc_shallow']])) {
        stop('Site has not swc_shallow data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = swc_shallow)) +
        geom_point(alpha = 0.4, colour = '#26A65B') +
        labs(y = 'SWC Shallow [cm3/cm3]') +
        scale_x_datetime()
    }

    # swc_deep
    if (type == 'swc_deep') {
      data <- get_env(x, solar)

      if (is.null(data[['swc_deep']])) {
        stop('Site has not swc_deep data')
      }

      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = TIMESTAMP, y = swc_deep)) +
        geom_point(alpha = 0.4, colour = '#019875') +
        labs(y = 'SWC Deep [cm3/cm3]') +
        scale_x_datetime()
    }

    return(res_plot)
  }
)

#' Replacement methods
#'
#' Methods for replacing the slots with new data or metadata
#'
#' The replacement object must be a valid object for that slot, i.e. for sapflow
#' data slot a data frame with the same dimensions and without TIMESTAMP variable
#' is needed. A validity check is done before returning the replaced SfnData
#' object and an error is returned if this check fails.
#'
#' @return The same SfnData object with the corresponding slot changed to the
#'   value provided. An error if the value provided generates an invalid
#'   SfnData object.
#'
#' @name sfn_replacement
NULL

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_sapf", "SfnData",
  function(object, value) {
    slot(object, "sapf_data") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_env", "SfnData",
  function(object, value) {
    slot(object, "env_data") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_sapf_flags", "SfnData",
  function(object, value) {
    slot(object, "sapf_flags") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_env_flags", "SfnData",
  function(object, value) {
    slot(object, "env_flags") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_timestamp", "SfnData",
  function(object, value) {
    slot(object, "timestamp") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_solar_timestamp", "SfnData",
  function(object, value) {
    slot(object, "solar_timestamp") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_si_code", "SfnData",
  function(object, value) {
    slot(object, "si_code") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_site_md", "SfnData",
  function(object, value) {
    slot(object, "site_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_stand_md", "SfnData",
  function(object, value) {
    slot(object, "stand_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_species_md", "SfnData",
  function(object, value) {
    slot(object, "species_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_plant_md", "SfnData",
  function(object, value) {
    slot(object, "plant_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement
setReplaceMethod(
  "get_env_md", "SfnData",
  function(object, value) {
    slot(object, "env_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

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
      nrow(slot(object, "sapf_data")) != nrow(slot(object, "env_data")),
      nrow(slot(object, "sapf_data")) != length(slot(object, "timestamp")),
      nrow(slot(object, "env_data")) != length(slot(object, "timestamp")),
      nrow(slot(object, "sapf_data")) != length(slot(object, "si_code")),
      nrow(slot(object, "env_data")) != length(slot(object, "si_code")),
      length(slot(object, "timestamp")) != length(slot(object, "si_code")),
      length(slot(object, "timestamp")) != length(slot(object, "solar_timestamp")),
      nrow(slot(object, "sapf_flags")) != nrow(slot(object, "sapf_data")),
      nrow(slot(object, "sapf_flags")) != nrow(slot(object, "env_data")),
      nrow(slot(object, "env_flags")) != nrow(slot(object, "sapf_data")),
      nrow(slot(object, "env_flags")) != nrow(slot(object, "env_data")),
      nrow(slot(object, "sapf_flags")) != nrow(slot(object, "env_flags")),
      nrow(slot(object, "sapf_flags")) != length(slot(object, "timestamp")),
      nrow(slot(object, "env_flags")) != length(slot(object, "timestamp")),
      nrow(slot(object, "sapf_flags")) != length(slot(object, "si_code")),
      nrow(slot(object, "env_flags")) != length(slot(object, "si_code"))
    )) {
      valid <- FALSE
      info <- c(info, 'dimensions are incorrect, they must fulfill "nrow(sapf_data) == nrow(env_data) == length(timestamp) == length(si_code)"')
    }

    # check if si_code is empty
    if (any(slot(object, "si_code") == '')) {
      valid <- FALSE
      info <- c(info, 'si_code slot can not be an empty string')
    }

    # check for metadata presence
    if (any(nrow(slot(object, "site_md")) < 1, nrow(slot(object, "stand_md")) < 1,
            nrow(slot(object, "species_md")) < 1, nrow(slot(object, "plant_md")) < 1,
            nrow(slot(object, "env_md")) < 1)) {
      valid <- FALSE
      info <- c(info, 'metadata slots can not be empty data frames')
    }

    # check for timestamp presence
    if (length(slot(object, "timestamp")) < 1) {
      valid <- FALSE
      info <- c(info, 'TIMESTAMP must be of length >= 1')
    }

    # check for si_code presence
    if (length(slot(object, "si_code")) < 1) {
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
