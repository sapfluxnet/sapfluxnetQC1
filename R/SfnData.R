#' An S4 class for sapfluxnet data
#'
#' @slot sapf A data frame with the sapf data
#'
#' @slot env A data frame with the env data
#'
#' @slot sapf_flags A data frame with the same dimensions of \code{sapf} with the
#'   flag info for each tree/TIMESTAMP combination
#'
#' @slot env_flags A data frame with the same dimensions of \code{env} with the
#'   flag info for each tree/TIMESTAMP combination
#'
#' @slot sapf_timestamp A posix vector with the TIMESTAMP for \code{sapf}
#'
#' @slot env_timestamp A posix vector with the TIMESTAMP for \code{env}
#'
#' @slot si_code A character indicating the site code
#'
#' @slot site_md A data frame containing the site metadata
#'
#' @slot stand_md A data frame containing the stand metadata
#'
#' @slot species_md A data frame containing the species metadata
#'
#' @slot plant_md A data frame containing the plant metadata
#'
#' @slot env_md A data frame containing the env metadata

SfnData <- setClass(
  'SfnData',
  slots = list(
    sapf = "data.frame",
    env = "data.frame",
    sapf_flags = "data.frame",
    env_flags = "data.frame",
    sapf_timestamp = "POSIXt",
    env_timestamp = "POSIXt",
    si_code = "character",
    site_md = "data.frame",
    stand_md = "data.frame",
    species_md = "data.frame",
    plant_md = "data.frame",
    env_md = "data.frame"
  )
)

#' SfnData custom get generics
#'
#' Generics for getting the info in the slots of SfnData
#'
#' @name sfn_get_generics
NULL

#' @rdname sfn_get_generics
setGeneric(
  "get_sapf",
  function(object, ...) {
    standardGeneric("get_sapf")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_env",
  function(object, ...) {
    standardGeneric("get_env")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_sapf_flags",
  function(object, ...) {
    standardGeneric("get_sapf_flags")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_env_flags",
  function(object, ...) {
    standardGeneric("get_env_flags")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_sapf_timestamp",
  function(object, ...) {
    standardGeneric("get_sapf_timestamp")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_env_timestamp",
  function(object, ...) {
    standardGeneric("get_env_timestamp")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_site_md",
  function(object, ...) {
    standardGeneric("get_site_md")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_stand_md",
  function(object, ...) {
    standardGeneric("get_stand_md")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_species_md",
  function(object, ...) {
    standardGeneric("get_species_md")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_plant_md",
  function(object, ...) {
    standardGeneric("get_plant_md")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_env_md",
  function(object, ...) {
    standardGeneric("get_env_md")
  }
)

#' @rdname sfn_get_generics
setGeneric(
  "get_si_code",
  function(object, ...) {
    standardGeneric("get_si_code")
  }
)

#' SfnData get methods
#'
#' Methods to get the info from the SfnData class slots
#'
#' @param object Object of class SfnData from which data is retrieved
#'
#' @param ... Further arguments to pass on to methods
#'
#' @name sfn_get_methods
NULL

#' @rdname sfn_get_methods
setMethod(
  "get_sapf", "SfnData",
  function(object) {
    slot(object, "sapf")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_env", "SfnData",
  function(object) {
    slot(object, "env")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_sapf_flags", "SfnData",
  function(object) {
    slot(object, "sapf_flags")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_env_flags", "SfnData",
  function(object) {
    slot(object, "env_flags")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_sapf_timestamp", "SfnData",
  function(object) {
    slot(object, "sapf_timestamp")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_env_timestamp", "SfnData",
  function(object) {
    slot(object, "env_timestamp")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_si_code", "SfnData",
  function(object) {
    slot(object, "si_code")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_site_md", "SfnData",
  function(object) {
    slot(object, "site_md")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_stand_md", "SfnData",
  function(object) {
    slot(object, "stand_md")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_species_md", "SfnData",
  function(object) {
    slot(object, "species_md")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_plant_md", "SfnData",
  function(object) {
    slot(object, "plant_md")
  }
)

#' @rdname sfn_get_methods
setMethod(
  "get_env_md", "SfnData",
  function(object) {
    slot(object, "env_md")
  }
)

#' Show method for SfnData
#'
#' @param object SfnData object to show
setMethod(
  "show", "SfnData",
  definition = function(object) {
    cat(class(object), " object.\n", sep = "")
  }
)
