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
#'
#' @import methods
#' @export SfnData
#' @exportClass SfnData

SfnData <- setClass(
  'SfnData',
  slots = list(
    sapf = "data.frame",
    env = "data.frame",
    sapf_flags = "data.frame",
    env_flags = "data.frame",
    # sapf_timestamp = "POSIXt",
    # env_timestamp = "POSIXt",
    si_code = "character",
    site_md = "data.frame",
    stand_md = "data.frame",
    species_md = "data.frame",
    plant_md = "data.frame",
    env_md = "data.frame"
  )
)
