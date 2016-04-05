################################################################################
#' Variable dictionaries for metadata
#'
#' \code{create_dic} creates a dictionary containing metadata variable names
#' and expected class. It is intended as an internal function.
#'
#' After loading metadata sheets, introduced variables and their classes must be
#' checked in order to ensure data correctness. For that, we need dictionaries
#' containing all the expected variables and their corresponding classes to
#' compare. This function works inside of \code{\link{qc_col_class}}, so there
#' is no need to call it directly
#'
#' @family Quality Checks Functions
#'
#' @param dic_name Name of the metadata sheet of which dictionary is needed.
#'   It must be one of \code{site_md}, \code{stand_md}, \code{species_md},
#'   \code{plant_md} or \code{environmental_md}.
#'
#' @return A named list, variable names being the index and class the value

# START
# Function declaration
create_dic <- function(dic_name) {

  # STEP 0
  # Argument checking
  # check if dictionary name is one of the five kinds of metadata allowed
  accepted_sheets <- c('site_md', 'stand_md', 'species_md',
                       'plant_md', 'environmental_md')

  if (!is.character(dic_name) || !(dic_name %in% accepted_sheets)) {
    stop('Provided dicitonary name is not a character or is not a valid name.
         Please see function help for information about valid dictionary names')
  }

  # STEP 1
  # Get the kind of metadata and populate the dictionary

  # 1.1 site metadata
  if (dic_name == 'site_md') {
    dic <- list(si_name = 'character', si_country = 'character',
                si_contact_firstname = 'character', si_contact_lastname = 'character',
                si_contact_email = 'character', si_contact_institution = 'character',
                si_addcontr_firstname = 'character', si_addcontr_lastname = 'character',
                si_addcontr_email = 'character', si_addcontr_institution = 'character',
                si_lat = c('numeric', 'integer'), si_long = c('numeric', 'integer'),
                si_elev = c('numeric', 'integer'), si_igbp = 'character',
                si_paper = 'character', si_dist_mgmt = 'character',
                si_flux_network = 'logical', si_dendro_network = 'logical',
                si_remarks = 'character', si_code = 'character')

    # 1.1.1 return dic
    return(dic)
  }

  # 1.2 stand metadata
  if (dic_name == 'stand_md'){
    dic <- list(st_name = 'character', st_growth_condition = 'character',
                st_treatment = 'character', st_age = c('numeric', 'integer'),
                st_height = c('numeric', 'integer'), st_density = c('numeric', 'integer'),
                st_basal_area = c('numeric', 'integer'), st_lai = c('numeric', 'integer'),
                st_aspect = 'character', st_terrain = 'character',
                st_soil_depth = c('numeric', 'integer'), st_soil_texture = 'character',
                st_sand_perc = c('numeric', 'integer'), st_silt_perc = c('numeric', 'integer'),
                st_clay_perc = c('numeric', 'integer'), st_remarks = 'character',
                si_code = 'character')

    # 1.2.1 return dic
    return(dic)
  }

  # 1.3 Species metadata
  if (dic_name == 'species_md') {
    dic <- list(sp_name = 'character', sp_ntrees = c('numeric', 'integer'),
                sp_leaf_habit = 'character', sp_basal_area_perc = c('numeric', 'integer'),
                si_code = 'character')

    # 1.3.1 return dic
    return(dic)
  }

  # 1.4 Plant metadata
  if (dic_name == 'plant_md') {
    dic <- list(pl_name = 'character', pl_species = 'character',
                pl_treatment = 'character', pl_dbh = c('numeric', 'integer'),
                pl_height = c('numeric', 'integer'), pl_age = c('numeric', 'integer'),
                pl_social = 'character', pl_sapw_area = c('numeric', 'integer'),
                pl_sapw_depth = c('numeric', 'integer'), pl_bark_thick = c('numeric', 'integer'),
                pl_leaf_area = c('numeric', 'integer'), pl_sens_meth = 'character',
                pl_sens_man = 'character', pl_sens_cor_grad = 'character',
                pl_sens_cor_zero = 'character', pl_sap_units = 'character',
                pl_sens_length = c('numeric', 'integer'), pl_sens_hgt = c('numeric', 'integer'),
                pl_sens_timestep = c('numeric', 'integer'), pl_radial_int = 'character',
                pl_azimut_int = 'character', pl_remarks = 'character',
                pl_code = 'character', si_code = 'character',
                pl_sens_calib = 'logical')

    # 1.4.1 return dic
    return(dic)
  }

  # 1.5 Environmental metadata
  if (dic_name == 'environmental_md') {
    dic <- list(env_time_zone = 'character', env_time_daylight = 'logical',
                env_timestep = c('numeric', 'integer'), env_ta = 'character',
                env_rh = 'character', env_vpd = 'character',
                env_sw_in = 'character', env_ppfd_in = 'character',
                env_netrad = 'character', env_ws = 'character',
                env_precip = 'character', env_swc_shallow_depth = c('numeric', 'integer'),
                env_swc_deep_depth = c('numeric', 'integer'), env_plant_watpot = 'character',
                env_leafarea_seasonal = 'character', env_remarks = 'character',
                si_code = 'character')

    # 1.5.1 return dic
    return(dic)
  }

  # END FUNCTION
}

################################################################################
#' Metadata columns check
#'
#' \code{qc_md_cols} checks if the columns of the provided metadata return the
#' correct class, as well as indicates any NAs present and the absence of any
#' mandatory variable.
#'
#' After loading the metadata, columns classes and presence/absence of mandatory
#' variables must be checked in order to continue with the quality check
#' workflow. This function returns a summary of the metadata columns and their
#' state.
#'
#' @family Quality Checks Functions
#'
#' @param metadata Data frame containing the metadata in which the test will be
#'   made.
#'
#' @param dic Name of the metadata dictionary to use as character. It must be
#'   one of the following: \code{'stand_md'}, \code{'site_md'},
#'   \code{'species_md'}, \code{'plant_md'} or \code{'environmental_md'}
#'
#' @return A data frame with variable names in one column and result of the
#'   checks in teh following columns
#'
#' @export

# START
# Function declaration

qc_md_cols <- function(metadata, dic) {

  # STEP 0
  # Argument checks
  # metadata is a data frame?
  if (!is.data.frame(metadata)) {
    stop('Metadata object is not a data frame')
  }

  # check if dictionary name is one of the five kinds of metadata allowed
  accepted_sheets <- c('site_md', 'stand_md', 'species_md',
                       'plant_md', 'environmental_md')

  if (!is.character(dic) || !(dic %in% accepted_sheets)) {
    stop('Provided dicitonary name is not a character or is not a valid name.
         Please see function help for information about valid dictionary names')
  }

  # STEP 1
  # Initialise result objects and dictionary
  dictionary <- create_dic(dic) # dictionary
  md_variables <- names(metadata) # variable names
  presence_res <- vector() # results of presence test
  classes_res <- vector() # results of class test
  det_class_res <- vector() # detected class
  na_res <- vector() # results of NA test

  # STEP 2
  # Checks
  for (name in names(dictionary)) {
    # 2.1 Presence test
    p_res <- name %in% md_variables
    presence_res <- c(presence_res, p_res)

    # 2.2 Class test
    d_res <- class(metadata[[name]])
    c_res <- any(dictionary[[name]] == d_res)
    # c_res <- identical(class(metadata[[name]]),
    #                    as.character(dictionary$Class[dictionary$Name == name]))
    classes_res <- c(classes_res, c_res)
    det_class_res <- c(det_class_res, d_res)

    # 2.3 NA test
    if (p_res) {
      n_res <- all(is.na(metadata[[name]]))
      na_res <- c(na_res, n_res)
    } else {
      n_res <- NA
      na_res <- c(na_res, n_res)
    }
  }

  # STEP 3
  # Create and return the result object
  result <- data.frame(Variable = names(dictionary),
                       PresenceOK = presence_res,
                       DetectedClass = det_class_res,
                       ClassOK = classes_res,
                       IsNA = na_res)

  return(result)

  # END FUNCTION
}
