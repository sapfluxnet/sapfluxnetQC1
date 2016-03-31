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
#' @param metadata Name of the metadata sheet of which dictionary is needed.
#'   It must be one of \code{site_md}, \code{stand_md}, \code{species_md},
#'   \code{plant_md} or \code{environmental_md}.
#'
#' @return A data frame containing two columns, \code{Name} and \code{Class} for
#'   the selected kind of metadata.

# START
# Function declaration
create_dic <- function(metadata) {

  # STEP 0
  # Argument checking


  # STEP 1
  # Get the kind of metadata and populate the dictionary

  # 1.1 site metadata
  if (metadata == 'site_md') {
    dic <- data.frame(
      Name = c('si_name', 'si_country', 'si_contact_firstname',
               'si_contact_lastname', 'si_contact_email', 'si_contact_institution',
               'si_addcontr_firstname', 'si_addcontr_lastname',
               'si_addcontr_email', 'si_addcontr_institution', 'si_lat',
               'si_long', 'si_elev', 'si_igbp', 'si_paper', 'si_dist_mgmt',
               'si_flux_network', 'si_dendro_network', 'si_remarks', 'si_code'),
      Class = c('character', 'character', 'character',
                'character', 'character', 'character',
                'character', 'character',
                'character', 'character', 'numeric',
                'numeric', 'numeric', 'character', 'character', 'character',
                'logical', 'logical', 'character', 'character')
    )

    # 1.1.1 return dic
    return(dic)
  }

  # 1.2 stand metadata
  if (metadata == 'stand_md'){
    dic <- data.frame(
      Name = c('st_age', 'st_aspect', 'st_basal_area', 'st_clay_perc',
               'st_code', 'st_density', 'st_growth_condition', 'st_height',
               'st_lai', 'st_remarks', 'st_sand_perc', 'st_silt_perc',
               'st_soil_depth', 'st_soil_texture', 'st_terrain', 'st_treatment'),
      Class = c('numeric', 'character', 'numeric', 'numeric',
                'character', 'character', 'character', 'numeric',
                'numeric', 'character', 'numeric', 'numeric',
                'numeric', 'character', 'character', 'character')
    )

    # 1.2.1 return dic
    return(dic)
  }

  # 1.3 Species metadata
  if (metadata == 'species_md') {
    dic <- data.frame(
      Name = c('sp_basal_area_perc', 'sp_leaf_habit', 'sp_name', 'sp_ntrees'),
      Class = c('numeric', 'character', 'character', 'numeric')
    )

    # 1.3.1 return dic
    return(dic)
  }

  # 1.4 Plant metadata
  if (metadata == 'species_md') {
    dic <- data.frame(
      Name = c('pl_age', 'pl_azimut_int', 'pl_bark_thick', 'pl_code',
               'pl_dbh', 'pl_height', 'pl_leaf_area', 'pl_radial_int',
               'pl_remarks', 'pl_sap_units', 'pl_sapw_area', 'pl_sapw_depth',
               'pl_sens_cor_grad', 'pl_sens_cor_zero', 'pl_sens_hgt',
               'pl_sens_length', 'pl_sens_man', 'pl_sens_meth',
               'pl_sens_timestep', 'pl_social', 'pl_species', 'pl_treatment'),
      Class = c('numeric', 'character', 'numeric', 'character',
                'numeric', 'numeric', 'numeric', 'character',
                'character', 'character', 'numeric', 'numeric',
                'character', 'character', 'numeric',
                'numeric', 'character', 'character',
                'numeric', 'character', 'character', 'character')
    )

    # 1.4.1 return dic
    return(dic)
  }

  # 1.5 Environmental metadata
  if (metadata == 'environmental_md') {
    dic <- data.frame(
      Name = c('env_leafarea_seasonal', 'env_netrad', 'env_plant_watpot',
               'env_remarks', 'env_ppdf_in', 'env_precip',
               'env_rh', 'env_swc_deep_depth', 'env_swc_shallow_depth',
               'env_sw_in', 'env_ta', 'env_time_daylight',
               'env_timestep', 'env_time_zone', 'env_vpd', 'env_ws'),
      Class = c('character', 'character', 'character',
                'character', 'character', 'character',
                'character', 'numeric', 'numeric',
                'character', 'character', 'logical',
                'numeric', 'character', 'character', 'character')
    )

    # 1.5.1 return dic
    return(dic)
  }

  # END FUNCTION
}

################################################################################
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
  na_res <- vector()

  # STEP 2
  # Checks (presence and class)
  for (name in dic$variables) {
    # presence
    p_res <- name %in% data_variables
    presence_res <- c(presence_res, p_res)
    # class
    c_res <- identical(class(data[[name]]),
                       as.character(dic$class[dic$variable == name]))
    classes_res <- c(classes_res, c_res)
    # NAs
    if (p_res) {
      n_res <- is.na(data[[name]])
      na_res <- c(na_res, n_res)
    } else {
      n_res <- NA
      na_res <- c(na_res, n_res)
    }

  }

  # STEP 3
  # Create and return the result object
  result <- data.frame(Variable = dic$variables,
                       PresenceOK = presence_res,
                       ClassOK = classes_res,
                       IsNA = na_res)

  return(result)

  # END FUNCTION
}
