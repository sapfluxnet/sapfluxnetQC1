################################################################################
# SERVER MANAGEMENT UTILITIES                                                  #
################################################################################

#' Status updater to server management
#'
#' Quick update for status files to update them to the new sapfluxnetQC1 library
#' versions
#'
#' This function get the old status, save it, destroy the file, generate a new
#' one and use the old to update the new one. Use it to generate updated
#' status files for the sites after updating the QC package
#'
#' @family Server Management
#'
#' @param si_code character with the site code
#'
#' @return Invisible TRUE if no problem was found. Invisible FALSE if the site
#'   was not updated.
#'
#' @export

# START FUNCTION
# Function declaration
sm_status_updater <- function(si_code, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.character(si_code)) {
      stop('Site code argument is not a character')
    }

    # STEP 1
    # 1.1 Get the old status
    old_status <- df_get_status(si_code, parent_logger = parent_logger)

    if (is.logical(old_status)) {
      message(si_code, ' status file does not exist, skipping')
      return(invisible(FALSE))
    }

    # 1.2 remove the status file
    file.rename(
      from = file.path('Data', si_code, paste0(si_code, '_status.yaml')),
      to = file.path('Data', si_code, paste0(si_code, '_status.bak'))
    )

    # 1.3 create an empty status file
    df_start_status(si_code, parent_logger = parent_logger)

    # 1.4 update the newly created status with the old one
    df_set_status(
      si_code,
      QC = old_status[['QC']],
      LVL1 = old_status[['LVL1']],
      LVL2 = old_status[['LVL2']],
      parent_logger = parent_logger
    )

    # STEP 2
    # Check file creation
    if (!file.exists(file.path('Data', si_code, paste0(si_code, '_status.yaml')))) {
      warning('file was not created, backup saved')
      return(invisible(FALSE))
    } else {
      unlink(file.path('Data', si_code, paste0(si_code, '_status.bak')))
      return(invisible(TRUE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_status_updater',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_status_updater',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_status_updater',
                                                        sep = '.'))})
}

################################################################################
#' solarTIMESTAMP adder
#'
#' Add solarTIMESTAMP slot to SfnData objects
#'
#' solarTIMESTAMP was a latter addition to the SfnData class. Sites ran with the
#' pre-solarTIMESTAMP version lacks this slot, which will cause problems in the
#' data flow. This function looks for level 1 SfnData object and update it.
#'
#' @family Server Management
#'
#' @param si_code Character indicating the site code
#'
#' @return Invisible TRUE if solarTIMESTAMP was succesfully added to all levels
#'   in si_code site
#'
#' @export

# START FUNCTION
# Function declaration
sm_solarTIMESTAMP_adder <- function(si_code, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.character(si_code)) {
      stop('site code provided is not a character')
    }

    # STEP 1
    # 1.1 get the SfnData
    sfndata <- try(
      df_read_SfnData(si_code, 'Lvl_1', parent_logger = parent_logger)
    )

    if (is(sfndata, 'try-error')) {
      message('SfnData for level 1 does not exists for ', si_code)
      return(invisible(FALSE))
    }

    # 1.2 add the solar timestamp
    get_solar_timestamp(sfndata) <- rep(as.POSIXct(NA),
                                        length(get_timestamp(sfndata)))

    # 1.3 rename old SfnData
    file.rename(
      from = file.path('Data', si_code, 'Lvl_1', paste0(si_code, '.RData')),
      to = file.path('Data', si_code, 'Lvl_1', paste0(si_code, '.bak'))
    )

    # 1.4 write the updated SfnData
    df_write_SfnData(sfndata, 'Lvl_1', parent_logger = parent_logger)

    # STEP 2
    # Check file creation
    if (!file.exists(file.path('Data', si_code, 'Lvl_1', paste0(si_code, '.RData')))) {
      warning('file was not created, backup saved')
      return(invisible(FALSE))
    } else {
      unlink(file.path('Data', si_code, 'Lvl_1', paste0(si_code, '.bak')))
      return(invisible(TRUE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'sm_solarTIMESTAMP_adder',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'sm_solarTIMESTAMP_adder',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'sm_solarTIMESTAMP_adder',
                                                        sep = '.'))})
}

################################################################################
#' SfnData to sfn_data conversion
#'
#' This function converts an SfnData to sfn_data object
#'
#' Things that this funcion do:
#'   List of thing to do when passing from SfnData to sfn_data:
#'   1. pl_name in plant metadata must be set to character in all sites.
#'   2. si_biome convert ot character (is factor)
#'   3. tz of solarTIMESTAMP to UTC fixed
#'   4. get rid of ascii characters in pl_sap_units and pl_sap_units_orig
#'   5. leaf data has not pl_sap_units_orig and pl_sap_units does not match
#'      due to bug in qc_sapw_conversion function
#'
#' @export

# START FUNCTION
# Function declaration
as_sfn_data <- function(SfnData, parent_logger = 'test') {

  withCallingHandlers({

    print(paste0('plant_md for ', get_si_code(SfnData)[1]))
    plant_md <- slot(SfnData, 'plant_md') %>%
      dplyr::mutate(
        # 1. pl_name in plant metadata must be set to character in all sites.
        pl_name = as.character(pl_name),

        # 4. get rid of ascii characters in pl_sap_units and pl_sap_units_orig
        # 5. leaf data has not pl_sap_units_orig and pl_sap_units does not match
        #    due to bug in qc_sapw_conversion function
        old_sap_units = pl_sap_units,
        pl_sap_units = if (
          is.null(.[['pl_sap_units_orig']])
        ) {
          'cm3 cm-2 h-1'
        } else {
          stringr::str_replace_all(pl_sap_units, "[“”]", '')
        },
        pl_sap_units_orig = if (
          is.null(.[['pl_sap_units_orig']])
        ) {
          stringr::str_replace_all(old_sap_units, "[“”]", '')
        } else { stringr::str_replace_all(pl_sap_units_orig, "[“”]", '') }
      ) %>%
      dplyr::select(-old_sap_units)

    print(paste0('site_md for ', get_si_code(SfnData)[1]))
    site_md <- slot(SfnData, 'site_md') %>%
      dplyr::mutate(
        # 2. si_biome convert ot character (is factor)
        si_biome = if (is.null(.[['si_biome']])) {NA} else {as.character(.[['si_biome']])}
      )

    # 3. tz of solarTIMESTAMP to UTC fixed
    print(paste0('solar TIMESTAMP for ', get_si_code(SfnData)[1]))
    solar_timestamp <- slot(SfnData, 'solar_timestamp') %>%
      lubridate::with_tz('UTC')


    res <- sapfluxnetr::sfn_data(
      sapf_data = slot(SfnData, 'sapf_data'),
      sapf_flags = slot(SfnData, 'sapf_flags'),
      env_data = slot(SfnData, 'env_data'),
      env_flags = slot(SfnData, 'env_flags'),
      si_code = slot(SfnData, 'si_code')[1],
      timestamp = slot(SfnData, 'timestamp'),
      solar_timestamp = solar_timestamp,
      site_md = site_md,
      stand_md = slot(SfnData, 'stand_md'),
      species_md = slot(SfnData, 'species_md'),
      plant_md = plant_md,
      env_md = slot(SfnData, 'env_md')
    )

    return(res)

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'as_sfn_data',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'as_sfn_data',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'as_sfn_data',
                                                        sep = '.'))})
}

################################################################################
#' Function to save the sfn_data created
#'
#' This function saves the RData files with the sfn_data objects
#'
#' @export

# START FUNCTION
# Funtion declaration
write_sfn_data <- function(sfn_data, folder, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    si_code <- sapfluxnetr::get_si_code(sfn_data)
    path <- file.path(folder, paste0(si_code, '.RData'))

    print(paste0('Writing ', si_code))

    assign(si_code, sfn_data)
    save(list = si_code, file = path)

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'write_sfn_data',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'write_sfn_data',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'write_sfn_data',
                                                        sep = '.'))})
}

################################################################################
#' sfn_data2csv function
#'
#' This function is used in lvl3_process to write the csv files for each sfn_data objet
#' slots in the corresponding folder of the database tree
#'
#' @export
sfn_data2csv <- function(sfn_data, csv_folder) {

  # get the slots and store them. In the case of data and flags, add the solar timestamp
  # also
  sapf_data <- sapfluxnetr::get_sapf_data(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  sapf_flags <- sapfluxnetr::get_sapf_flags(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  env_data <- sapfluxnetr::get_env_data(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  env_flags <- sapfluxnetr::get_env_flags(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  site_md <- sapfluxnetr::get_site_md(sfn_data)
  stand_md <- sapfluxnetr::get_stand_md(sfn_data)
  species_md <- sapfluxnetr::get_species_md(sfn_data)
  plant_md <- sapfluxnetr::get_plant_md(sfn_data)
  env_md <- sapfluxnetr::get_env_md(sfn_data)

  sapf_data_name <- file.path(csv_folder, paste0(si_code, '_sapf_data.csv'))
  env_data_name <- file.path(csv_folder, paste0(si_code, '_env_data.csv'))
  sapf_flags_name <- file.path(csv_folder, paste0(si_code, '_sapf_flags.csv'))
  env_flags_name <- file.path(csv_folder, paste0(si_code, '_env_flags.csv'))
  site_md_name <- file.path(csv_folder, paste0(si_code, '_site_md.csv'))
  stand_md_name <- file.path(csv_folder, paste0(si_code, '_stand_md.csv'))
  species_md_name <- file.path(csv_folder, paste0(si_code, '_species_md.csv'))
  plant_md_name <- file.path(csv_folder, paste0(si_code, '_plant_md.csv'))
  env_md_name <- file.path(csv_folder, paste0(si_code, '_env_md.csv'))

  readr::write_csv(sapf_data, sapf_data_name)
  readr::write_csv(env_data, env_data_name)
  readr::write_csv(sapf_flags, sapf_flags_name)
  readr::write_csv(env_flags, env_flags_name)
  readr::write_csv(site_md, site_md_name)
  readr::write_csv(stand_md, stand_md_name)
  readr::write_csv(species_md, species_md_name)
  readr::write_csv(plant_md, plant_md_name)
  readr::write_csv(env_md, env_md_name)
}

################################################################################
#' QC3 function, cleaning a little
#'
#' Function to final clean the data and generate the sapfluxnetr::sfn_data
#' objects
#'
#' This function looks for LVL2 completed data at the three levels and performs
#' the last cleaning and the sfn_data construction. See as_sfn_data for more
#' details
#'
#' @export

lvl3_process <- function(version = '0.0.1', parent_logger = 'test') {

  # get the sites ready to lvl3
  sites <- names(sapfluxnetQC1::df_whos_ready_to('lvl3', 'ready'))

  # folders
  folder_plant <- file.path('..', 'sapfluxnet_db', version, 'RData', 'plant')
  folder_sapwood <- file.path('..', 'sapfluxnet_db', version, 'RData', 'sapwood')
  folder_leaf <- file.path('..', 'sapfluxnet_db', version, 'RData', 'leaf')
  csv_folder_plant <- file.path('..', 'sapfluxnet_db', version, 'csv', 'plant')
  csv_folder_sapwood <- file.path('..', 'sapfluxnet_db', version, 'csv', 'sapwood')
  csv_folder_leaf <- file.path('..', 'sapfluxnet_db', version, 'csv', 'leaf')

  # big loop
  for (site in sites) {

    # plant level
    if ('plant' %in% df_get_status(site)[['LVL2']][['AVAIL']]) {

      df_read_SfnData(
        site, 'unit_trans', 'plant', parent_logger = parent_logger
      ) %>%
        as_sfn_data(parent_logger = parent_logger) -> plant_sfn_data

      write_sfn_data(plant_sfn_data, folder = folder_plant)
      sfn_data2csv(plant_sfn_data, folder = csv_folder_plant)

    }

    # sapwood level
    if ('sapwood' %in% df_get_status(site)[['LVL2']][['AVAIL']]) {

      df_read_SfnData(
        site, 'unit_trans', 'sapwood', parent_logger = parent_logger
      ) %>%
        as_sfn_data(parent_logger = parent_logger) -> sapwood_sfn_data

      write_sfn_data(sapwood_sfn_data, folder = folder_sapwood)
      sfn_data2csv(sapwood_sfn_data, folder = csv_folder_sapwood)

    }

    # leaf level
    if ('leaf' %in% df_get_status(site)[['LVL2']][['AVAIL']]) {

      df_read_SfnData(
        site, 'unit_trans', 'leaf', parent_logger = parent_logger
      ) %>%
        as_sfn_data(parent_logger = parent_logger) -> leaf_sfn_data

      write_sfn_data(leaf_sfn_data, folder = folder_leaf)
      sfn_data2csv(leaf_sfn_data, folder = csv_folder_leaf)

    }

    # set status
    df_set_status(site, LVL2 = list(TO_LVL3 = 'DONE'))

  }

}
