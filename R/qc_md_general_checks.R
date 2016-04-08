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
#' Dictionary creation for site_md variables
#'
#' \code{qc_site_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in site_md are \code{si_country}, \code{si_dist_mgmt}
#' and \code{si_igbp}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'si_igbp'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable

# START
# Function declaration
qc_site_dics <- function(variable) {

  # STEP 0
  # Argument checks
  # valid variables for site_md
  accepted_vars <- c('si_country', 'si_dist_mgmt', 'si_igbp')
  if (!(variable %in% accepted_vars)) {
    stop('Variable provided is not adequate for creating a dictionary.
         Please see "Accepted variables" section of the function help.')
  }

  # STEP 1
  # Get the variable and populate the dictionary
  # 1.1 si_country
  if (variable == 'si_country') {
    res <- c('AFG', 'ALA', 'ALB', 'DZA', 'ASM', 'AND', 'AGO', 'AIA', 'ATA',
             'ATG', 'ARG', 'ARM', 'ABW', 'AUS', 'AUT', 'AZE', 'BHS', 'BHR',
             'BGD', 'BRB', 'BLR', 'BEL', 'BLZ', 'BEN', 'BMU', 'BTN', 'BOL',
             'BES', 'BIH', 'BWA', 'BVT', 'BRA', 'IOT', 'BRN', 'BGR', 'BFA',
             'BDI', 'CPV', 'KHM', 'CMR', 'CAN', 'CYM', 'CAF', 'TCD', 'CHL',
             'CHN', 'CXR', 'CCK', 'COL', 'COM', 'COG', 'COD', 'COK', 'CRI',
             'CIV', 'HRV', 'CUB', 'CUW', 'CYP', 'CZE', 'DNK', 'DJI', 'DMA',
             'DOM', 'ECU', 'EGY', 'SLV', 'GNQ', 'ERI', 'EST', 'ETH', 'FLK',
             'FRO', 'FJI', 'FIN', 'FRA', 'GUF', 'PYF', 'ATF', 'GAB', 'GMB',
             'GEO', 'DEU', 'GHA', 'GIB', 'GRC', 'GRL', 'GRD', 'GLP', 'GUM',
             'GTM', 'GGY', 'GIN', 'GNB', 'GUY', 'HTI', 'HMD', 'VAT', 'HND',
             'HKG', 'HUN', 'ISL', 'IND', 'IDN', 'IRN', 'IRQ', 'IRL', 'IMN',
             'ISR', 'ITA', 'JAM', 'JPN', 'JEY', 'JOR', 'KAZ', 'KEN', 'KIR',
             'PRK', 'KOR', 'KWT', 'KGZ', 'LAO', 'LVA', 'LBN', 'LSO', 'LBR',
             'LBY', 'LIE', 'LTU', 'LUX', 'MAC', 'MKD', 'MDG', 'MWI', 'MYS',
             'MDV', 'MLI', 'MLT', 'MHL', 'MTQ', 'MRT', 'MUS', 'MYT', 'MEX',
             'FSM', 'MDA', 'MCO', 'MNG', 'MNE', 'MSR', 'MAR', 'MOZ', 'MMR',
             'NAM', 'NRU', 'NPL', 'NLD', 'NCL', 'NZL', 'NIC', 'NER', 'NGA',
             'NIU', 'NFK', 'MNP', 'NOR', 'OMN', 'PAK', 'PLW', 'PSE', 'PAN',
             'PNG', 'PRY', 'PER', 'PHL', 'PCN', 'POL', 'PRT', 'PRI', 'QAT',
             'REU', 'ROU', 'RUS', 'RWA', 'BLM', 'SHN', 'KNA', 'LCA', 'MAF',
             'SPM', 'VCT', 'WSM', 'SMR', 'STP', 'SAU', 'SEN', 'SRB', 'SYC',
             'SLE', 'SGP', 'SXM', 'SVK', 'SVN', 'SLB', 'SOM', 'ZAF', 'SGS',
             'SSD', 'ESP', 'LKA', 'SDN', 'SUR', 'SJM', 'SWZ', 'SWE', 'CHE',
             'SYR', 'TWN', 'TJK', 'TZA', 'THA', 'TLS', 'TGO', 'TKL', 'TON',
             'TTO', 'TUN', 'TUR', 'TKM', 'TCA', 'TUV', 'UGA', 'UKR', 'ARE',
             'GBR', 'USA', 'UMI', 'URY', 'UZB', 'VUT', 'VEN', 'VNM', 'VGB',
             'VIR', 'WLF', 'ESH', 'YEM', 'ZMB', 'ZWE')

    # 1.1.1 return the dic
    return(res)
  }

  # 1.2 si_dist_mgmt
  if (variable == 'si_dist_mgmt') {
    res <- c('Agriculture', 'Drought', 'Fire', 'Forestry', 'Grazing',
             'Hydrologic event', 'Land cover change', 'Pests and disease',
             'NULL')

    # 1.2.1 return the dic
    return(res)
  }

  # 1.3 si_igbp
  if (variable == 'si_igbp') {
    res <- c('BSV', 'CRO', 'CSH', 'CVM', 'DBF', 'DNF', 'EBF',
             'ENF', 'MF', 'OSH', 'SAV', 'URB', 'WET', 'WSA')

    # 1.3.1 return the dic
    return(res)
  }

  # END FUNCTION
}

################################################################################
#' Dictionary creation for stand_md variables
#'
#' \code{qc_stand_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in stand_md are \code{st_growth_condition},
#' \code{st_aspect}, \code{st_terrain} and \code{st_soil_texture}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'st_aspect'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable

# START
# Function declaration
qc_stand_dics <- function(variable) {

  # STEP 0
  # Argument checks
  # valid variables for site_md
  accepted_vars <- c('st_growth_condition', 'st_aspect',
                     'st_terrain', 'st_soil_texture')
  if (!(variable %in% accepted_vars)) {
    stop('Variable provided is not adequate for creating a dictionary.
         Please see "Accepted variables" section of the function help.')
  }

  # STEP 1
  # Get the variable and populate the dictionary
  # 1.1 st_growth_condition
  if (variable == 'st_growth_condition') {
    res <- c('Naturally regenerated, unmanaged', 'Naturally regenerated, managed',
             'Plantation, managed', 'Plantation, unmanaged', 'Orchard',
             'Urban')

    # 1.1.1 return the dic
    return(res)
  }

  # 1.2 st_aspect
  if (variable == 'st_aspect') {
    res <- c('Flat', 'N', 'E', 'S', 'W')

    # 1.2.1 return the dic
    return(res)
  }

  # 1.3 st_terrain
  if (variable == 'st_terrain') {
    res <- c('Flat', 'Undulated/Variable', 'Valley', 'Gentle slope (<2 %)',
             'Medium Slope (>2 %, <5%)', 'Significant Slope (>5%, <10%)',
             'Strong Slope (>10%)', 'Hilltop')

    # 1.3.1 return the dic
    return(res)
  }

  # 1.4 st_soil_texture
  if (variable == 'st_soil_texture') {
    res <- c('SAND', 'LOAM', 'SILT', 'CLAY')

    # return the dic
    return(res)
  }

  # END FUNCTION
}

################################################################################
#' Dictionary creation for species_md variables
#'
#' \code{qc_species_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in species_md are \code{sp_leaf_habit}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'sp_leaf_habit'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable

# START
# Function declaration
qc_species_dics <- function(variable = 'sp_leaf_habit') {

  # STEP 0
  # Argument checks
  # valid variables for site_md
  accepted_vars <- c('sp_leaf_habit')
  if (!(variable %in% accepted_vars)) {
    stop('Variable provided is not adequate for creating a dictionary.
         Please see "Accepted variables" section of the function help.')
  }

  # STEP 1
  # Get the variable and populate the dictionary
  # 1.1 sp_leaf_habit
  res <- c('evergreen', 'cold deciduous', 'drought deciduous', 'marcescent')

  # 1.1.1 return dic
  return(res)

  # END FUNCTION
}

################################################################################
#' Dictionary creation for plant_md variables
#'
#' \code{qc_plant_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in plant_md are \code{pl_social}, \code{pl_sens_meth},
#' \code{pl_sens_man}, \code{pl_sens_cor_grad}, \code{pl_sens_cor_zero},
#' \code{pl_sap_units}, \code{pl_radial_int} and \code{pl_azimut_int}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'pl_sap_units'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable

# START
# Function declaration
qc_plant_dics <- function(variable) {

  # STEP 0
  # Argument checks
  # valid variables for site_md
  accepted_vars <- c('pl_social', 'pl_sens_meth', 'pl_sens_man',
                     'pl_sens_cor_grad', 'pl_sens_cor_zero',
                     'pl_sap_units', 'pl_radial_int', 'pl_azimut_int')
  if (!(variable %in% accepted_vars)) {
    stop('Variable provided is not adequate for creating a dictionary.
         Please see "Accepted variables" section of the function help.')
  }

  # STEP 1
  # Get the variable and populate the dictionary
  # 1.1 pl_social
  if (variable == 'pl_social') {
    res <- c('dominant', 'codominant', 'suppressed')

    # 1.1.1 return the dic
    return(res)
  }

  # 1.2 pl_sens_meth
  if (variable == 'pl_sens_meth') {
    res <- c('CAG', 'HD', 'CHP', 'CHD', 'HFD', 'HPTM',
             'HR', 'SFPLUS', 'SHB', 'TSHB', 'Other/unknown')

    # 1.2.1 return the dic
    return(res)
  }

  # 1.3 pl_sens_man
  if (variable == 'pl_sens_man') {
    res <- c('Lab made', 'Dynamax', 'UP GmbH', 'Ecomatik', 'PlantSensors',
             'ICT International', 'Ems Brno', 'East30', 'Tranzflo', 'Phytech',
             'Puech Asociados', 'Advanced Measurements and Controls',
             'HortResearch', 'Greenspan Technology', 'Other/unknown')

    # 1.3.1 return the dic
    return(res)
  }

  # 1.4 pl_sens_cor_grad
  if (variable == 'pl_sens_cor_grad') {
    res <- c('No correction', 'NTG separately measured',
             'NTG measured in cyclic heating deisgn','NTG modelled',
             'Other/unknown')

    # 1.4.1 return the dic
    return(res)
  }

  # 1.5 pl_sens_cor_zero
  if (variable == 'pl_sens_cor_zero') {
    res <- c('Previous night zero flow', 'Long time-window zero flow',
             'Moist nights zero flow', 'Manipulative zero flow', 'Not needed',
             'Other/unknown')

    # 1.5.1 return the dic
    return(res)
  }

  # 1.6 pl_sap_units
  if (variable == 'pl_sap_units') {
    res <- c('“cm3 cm-2 h-1”', '“cm3 m-2 s-1”', '“dm3 dm-2 h-1”',
             '“dm3 dm-2 s-1”', '“mm3 mm-2 s-1”', '“g m-2 s-1”',
              '“kg m-2 h-1”', '“kg m-2 s-1”', '“cm3 s-1”',
              '“cm3 h-1”', '“dm3 h-1”', '“g h-1”', '“kg h-1”')

    # 1.6.1 return the dic
    return(res)
  }

  # 1.7 pl_radial_int
  if (variable == 'pl_radial_int') {
    res <- c('No radial correction', 'Sensor-integrated', 'Measured',
             'Corrected, measured radial variation',
             'Corrected, species coefficients', 'Corrected, other coefficients')

    # 1.7.1 return the dic
    return(res)
  }

  # 1.8 pl_azimut_int
  if (variable == 'pl_azimut_int') {
    res <- c('No azimuthal correction', 'sensor_integrated', 'measured',
             'Corrected, measured azimuthal variation')

    # 1.8.1 return the dic
    return(res)
  }

  # END FUNCTION
}

################################################################################
#' Dictionary creation for environmental_md variables
#'
#' \code{qc_env_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in environmental_md are \code{env_time_zone},
#' \code{env_ta}, \code{env_rh}, \code{env_vpd}, \code{env_sw_in},
#' \code{env_ppfd_in}, \code{env_netrad}, \code{env_ws}, \code{env_precip},
#' \code{env_plant_watpot} and \code{env_leafarea_seasonal}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'env_vpd'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable

# START
# Function declaration
qc_env_dics <- function(variable) {

  # STEP 0
  # Argument checks
  # valid variables for site_md
  accepted_vars <- c('env_time_zone', 'env_ta', 'env_rh', 'env_vpd',
                     'env_sw_in', 'env_ppfd_in', 'env_netrad', 'env_ws',
                     'env_precip', 'env_plant_watpot', 'env_leafarea_seasonal')
  if (!(variable %in% accepted_vars)) {
    stop('Variable provided is not adequate for creating a dictionary.
         Please see "Accepted variables" section of the function help.')
  }

  # STEP 1
  # Get the variable and populate the dictionary
  # 1.1 env_time_zone
  if (variable == 'env_time_zone') {
    res <- c('1UTC−12:00, Y', '2UTC−11:00, X', '3UTC−10:00, W', '4UTC−09:30, V†',
             '5UTC−09:00, V', '6UTC−08:00, U', '7UTC−07:00, T', '8UTC−06:00, S',
             '9UTC−05:00, R', '10UTC−04:30, Q†', '11UTC−04:00, Q',
             '12UTC−03:30, P†', '13UTC−03:00, P', '14UTC−02:00, O',
             '15UTC−01:00, N','16UTC±00:00, Z', '17UTC+01:00, A',
             '18UTC+02:00, B', '19UTC+03:00, C', '20UTC+03:30, C†',
             '21UTC+04:00, D', '22UTC+04:30, D†', '23UTC+05:00, E',
             '24UTC+05:30, E†', '25UTC+05:45, E*', '26UTC+06:00, F',
             '27UTC+06:30, F†', '28UTC+07:00, G', '29UTC+08:00, H',
             '30UTC+08:30, H†', '31UTC+08:45, H*', '32UTC+09:00, I',
             '33UTC+09:30, I†', '34UTC+10:00, K', '35UTC+10:30, K†',
             '36UTC+11:00, L', '37UTC+12:00, M', '38UTC+12:45, M*',
             '39UTC+13:00, M†', '40UTC+14:00, M†')

    # 1.1.1 return the dic
    return(res)
  }

  # 1.2 env_ta, rh, vpd, sw_in, ppfd_in, netrad, ws and precip
  if (variable %in% c('env_ta', 'env_rh', 'env_vpd',
                      'env_sw_in', 'env_ppfd_in', 'env_netrad', 'env_ws',
                      'env_precip')) {
    res <- c('Above canopy', 'Within canopy', 'Clearing',
             'Off-site', 'Not provided')

    # 1.2.1 return the dic
    return(res)
  }

  # 1.3 env_plant_watpot
  if (variable == 'env_plant_watpot') {
    res <- c('leaf: predawn', 'leaf: midday', 'xylem: predawn', 'xylem: midday',
             'leaf: predawn and midday', 'xylem: predawn and midday',
             'xylem: continuous', 'leaf: continuous')

    # 1.3.1 return the dic
    return(res)
  }

  # 1.4 env_leafarea_seasonal
  if (variable == 'env_leafarea_seasonal') {
    res <- c('stand level', 'species level', 'tree level', 'NULL')

    # 1.4.1 return the dic
    return(res)
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

################################################################################
#' Metadata factor variables check
#'
#' \code{qc_factor_values} function checks in the provided metadata if the
#' factor variables value is a valid value.
#'
#' Values for factor variables in the metadata must be checked in order to
#' ensure that they are valid (i.e. they are one of the established factor
#' value).
#'
#' @family Quality Checks Functions
#'
#' @param site,stand,species,plant,environmental Data frames with the metadata
#'   to check.
#'
#' @return A data frame with variable names, check result and NA presence as
#'   columns
#'
#' @export

# START
# Function declaration
qc_factor_values <- function(site = NULL, stand = NULL, species = NULL,
                             plant = NULL, environmental = NULL) {

  # STEP 0
  # Argument checks
  #

  # STEP 1
  # Initialize result objects
  var_name <- vector()
  res_check <- vector()
  na_presence <- vector()

  # STEP 2
  # Walk through metadata files and check variables
  # 2.1 site
  if (!is.null(site)) {
    si_names <- c('si_country', 'si_dist_mgmt', 'si_igbp')
    si_checks <- sapply(si_names, function(x) { site[[x]] %in% qc_site_dics(x) })
    si_nas <- sapply(si_names, function(x) { any(is.na(site[[x]])) })
    var_name <- c(var_name, si_names)
    res_check <- c(res_check, si_checks)
    na_presence <- c(na_presence, si_nas)
  }

  # 2.2 stand
  if(!is.null(stand)) {
    st_names <- c('st_growth_condition', 'st_aspect',
                  'st_terrain', 'st_soil_texture')
    st_checks <- sapply(st_names, function(x) { stand[[x]] %in% qc_stand_dics(x) })
    st_nas <- sapply(st_names, function(x) { any(is.na(stand[[x]])) })
    var_name <- c(var_name, st_names)
    res_check <- c(res_check, st_checks)
    na_presence <- c(na_presence, st_nas)
  }

  # 2.3 species
  if(!is.null(species)) {
    sp_names <- c('sp_leaf_habit')
    sp_checks <- sapply(sp_names, function(x) {
      all(species[[x]] %in% qc_species_dics(x))
    })
    sp_nas <- sapply(sp_names, function(x) { any(is.na(species[[x]])) })
    var_name <- c(var_name, sp_names)
    res_check <- c(res_check, sp_checks)
    na_presence <- c(na_presence, sp_nas)
  }

  # 2.4 plant
  if(!is.null(plant)) {
    pl_names <- c('pl_social', 'pl_sens_meth', 'pl_sens_man',
                  'pl_sens_cor_grad', 'pl_sens_cor_zero',
                  'pl_sap_units', 'pl_radial_int', 'pl_azimut_int')
    pl_checks <- sapply(pl_names, function(x) {
      all(plant[[x]] %in% qc_plant_dics(x))
    })
    pl_nas <- sapply(pl_names, function(x) { any(is.na(plant[[x]])) })
    var_name <- c(var_name, pl_names)
    res_check <- c(res_check, pl_checks)
    na_presence <- c(na_presence, pl_nas)
  }

  # 2.5 environmental
  if(!is.null(environmental)) {
    env_names <- c('env_time_zone', 'env_ta', 'env_rh', 'env_vpd',
                   'env_sw_in', 'env_ppfd_in', 'env_netrad', 'env_ws',
                   'env_precip', 'env_plant_watpot', 'env_leafarea_seasonal')
    env_checks <- sapply(env_names, function(x) { environmental[[x]] %in% qc_env_dics(x) })
    env_nas <- sapply(env_names, function(x) { any(is.na(environmental[[x]])) })
    var_name <- c(var_name, env_names)
    res_check <- c(res_check, env_checks)
    na_presence <- c(na_presence, env_nas)
  }

  # 3. Generate the results data frame and return it
  res_data <- data.frame(Variable = var_name,
                         Check_result = res_check,
                         NA_presence = na_presence)

  return(res_data)

  # END FUNCTION
}
