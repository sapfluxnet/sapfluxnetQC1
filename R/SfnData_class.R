#' An S4 class for sapfluxnet data
#'
#' @slot sapf_data A data frame with the sapf data
#'
#' @slot env_data A data frame with the env data
#'
#' @slot sapf_flags A data frame with the same dimensions of \code{sapf_data}
#'   with the flag info for each tree/TIMESTAMP combination
#'
#' @slot env_flags A data frame with the same dimensions of \code{env_data} with
#'   the flag info for each env_var/TIMESTAMP combination
#'
#' @slot si_code A character vector of length \code{nrow(sapf_data)} indicating
#'   the site code
#'
#' @slot timestamp A POSIXct vector of length \code{nrow(sapf_data)} with the
#'   timestamp
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
#'
#' @import methods
#' @export SfnData
#' @exportClass SfnData

SfnData <- setClass(
  'SfnData',
  slots = list(
    sapf_data = "data.frame",
    env_data = "data.frame",
    sapf_flags = "data.frame",
    env_flags = "data.frame",
    si_code = "character",
    timestamp = "POSIXt",
    site_md = "data.frame",
    stand_md = "data.frame",
    species_md = "data.frame",
    plant_md = "data.frame",
    env_md = "data.frame"
  )
)
