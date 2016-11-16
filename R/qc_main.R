################################################################################
#' Get the site code and the names of the data files (metadata, sapflow and env)
#'
#' Look at the data folder provided and get the code and the names of the files
#' with the metadata, sapflow data and environmental data, in order to use them
#' as parameters in the automated reports
#'
#' @family Data Loading Functions
#'
#' @param folder Route to folder in which are the code and the file names to
#'   retrieve
#'
#' @return A list. The first element is the site code, the second one the
#'   metadata file route, the third the sapflow data file route and finally, the
#'   fourth the environmental data file route.
#'
#' @export

# START
# Function declaration
dl_get_si_code <- function(folder = '.', parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # is folder a valid and existent folder?
    if (!file_test("-d", folder)) {
      stop('Folder does not exist, please check if folder name has been correctly provided')
    }

    # STEP 1
    # be fast, get the files, now!
    files <- list.files(folder,
                        pattern = "(_env_data|_sapflow_data)\\.csv$|_metadata\\.xls(x)?$")
    complete_files <- list.files(folder,
                                 pattern = "(_env_data|_sapflow_data)\\.csv$|_metadata\\.xls(x)?$",
                                 full.names = TRUE)

    # 1.1 Check if there is files, to avoid waste time
    if (length(files) < 1) {
      stop('There is no files matching data names pattern')
    }

    # STEP 2
    # don't forget to extract the si_code, is needed!
    code <- unique(stringr::str_replace(
      files, "(_env_data|_sapflow_data)\\.csv$|_metadata\\.xls(x)?$", ""
    ))

    # 2.1 check if there is more than one code, which is a problem!
    if (length(code) > 1) {
      stop('There is more than one code in the folder, please revise manually the folder')
    }

    # STEP 3
    # How many files? are they the correct ones?

    # 3.1 if more than three files ending in env_data.csv, sapflow_data.csv or
    #     metadata.xlsx, stop it now!!!
    if (length(files) > 3) {
      stop('There is more than three data files, please revise manually the folder')
    } else {

      # 3.2 Three files, that is the trifecta (xlsx, csv, csv)
      if (length(files) == 3) {

        # 3.2.1 get the names, quick!
        metadata <- complete_files[grep('_metadata\\.xls(x)?$', complete_files)]
        sapf <- complete_files[grep('_sapflow_data\\.csv$', complete_files)]
        env <- complete_files[grep('env_data\\.csv$', complete_files)]
      } else {

        # 3.3 One file to rule them all
        if (length(files) == 1) {

          # 3.3.1 only one name but three things
          metadata <- complete_files[grep('_metadata\\.xls(x)?$', complete_files)]
          sapf <- metadata
          env <- metadata
        }
      }
    }

    # STEP 4
    # now, lets make the results object, a list
    res <- list(
      si_code = code,
      md_file = metadata,
      sapf_file = sapf,
      env_file = env
    )

    # STEP 5
    # Return it!!!
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'dl_get_si_code', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'dl_get_si_code', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'dl_get_si_code', sep = '.'))})


}



################################################################################
#' Main function to resume Metadata QC in one data frame
#'
#' Metadata QC codified results in one data frame
#'
#' @family Quality Checks Functions
#'
#' @param md_cols
#'
#' @param factor_values
#'
#' @param email_check
#'
#' @param site_md_coordfix
#'
#' @param species_md
#'
#' @param plant_md
#'
#' @param species_md_spnames
#'
#' @param plant_md_spnames
#'
#' @param sp_verification
#'
#' @param env_var_presence
#'
#' @return A data frame with the highlights of the QC
#'
#' @export

# START
# Function declaration
qc_md_results_table <- function(md_cols, factor_values,
                                email_check, site_md_coordfix,
                                species_md, plant_md,
                                species_md_spnames, plant_md_spnames,
                                sp_verification, env_var_presence,
                                parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 1
    # Create the result vectors
    step <- vector()
    status <- vector()
    description <- vector()

    # STEP 2
    # Metadata columns
    # 2.1 Presence
    if (any(!md_cols$PresenceOK)) {
      step <- c(step, 'Metadata variables presence')
      status <- c(status, 'ERROR')
      description <- c(description, 'One or more variables are missing from metadata')
    } else {
      step <- c(step, 'Metadata variables presence')
      status <- c(status, 'PASS')
      description <- c(description, 'All metadata variables are present')
    }

    # 2.2 ClassOK
    if (any(is.na(md_cols$IsNA)) | any(is.na(md_cols$ClassOK))) {
      step <- c(step, 'Metadata variables expected class')
      status <- c(status, 'WARNING')
      description <- c(description, 'One or more variables are missing from metadata and class check is unfeasible')
    } else {
      if (any(!md_cols$ClassOK & !md_cols$IsNA)) {
        step <- c(step, 'Metadata variables expected class')
        status <- c(status, 'ERROR')
        description <- c(description, 'One or more variables have the wrong class')
      } else {
        step <- c(step, 'Metadata variables expected class')
        status <- c(status, 'PASS')
        description <- c(description, 'All metadata variables have the correct class')
      }
    }

    # 2.3 NAs
    if (any(is.na(md_cols$IsNA))) {
      step <- c(step, 'Metadata variables NA presence')
      status <- c(status, 'WARNING')
      description <- c(description, 'One or more variables are missing from metadata and NA check is unfeasible')
    } else {
      if (any(md_cols$IsNA)) {
        step <- c(step, 'Metadata variables NA presence')
        status <- c(status, 'INFO')
        description <- c(description, 'Some variables have no value')
      } else {
        step <- c(step, 'Metadata variables NA presence')
        status <- c(status, 'PASS')
        description <- c(description, 'No NAs in metadata')
      }
    }

    # STEP 3
    # Metadata factor values
    # 3.1 Wrong value
    if (any(!factor_values$Check_result & !factor_values$NA_presence)) {
      step <- c(step, 'Metadata factor variable values')
      status <- c(status, 'ERROR')
      description <- c(description, 'One or more metadata factor variables have values not accepted')
    } else {
      step <- c(step, 'Metadata factor variable values')
      status <- c(status, 'PASS')
      description <- c(description, 'All factor variables have accepted values')
    }

    # STEP 4
    # Metadata email
    if (all(is.na(email_check$Is_correct)) | any(!email_check$Is_correct, na.rm = TRUE)) {
      step <- c(step, 'Email check')
      status <- c(status, 'WARNING')
      description <- c(description, 'Email is missing or in wrong format')
    } else {
      step <- c(step, 'Email check')
      status <- c(status, 'PASS')
      description <- c(description, 'Correct email format')
    }

    # STEP 5
    # Coordinates check
    if (!site_md_coordfix$is_inside_country) {
      step <- c(step, 'Site coordinates')
      status <- c(status, 'WARNING')
      description <- c(description, 'Site provided coordinates are incorrect and not fixable')
    } else {
      step <- c(step, 'Site coordinates')
      status <- c(status, 'PASS')
      description <- c(description, 'Site provided correct or fixable coordinates')
    }

    # STEP 6
    # Species names
    # 6.1 species md
    if (any(!species_md_spnames$Concordance)) {
      step <- c(step, 'Species names spelling (species_md)')
      status <- c(status, 'WARNING')
      description <- c(description, 'Species names in Species metadata are mispelled')
    } else {
      step <- c(step, 'Species names spelling (species_md)')
      status <- c(status, 'PASS')
      description <- c(description, 'No mispelling in species names')
    }


    # if (isTRUE(tryCatch(sapfluxnetQC1::qc_species_names(species_md$sp_name),
    #                     error = function(e) return(TRUE)))) {
    #   step <- c(step, 'Species names spelling (species_md)')
    #   status <- c(status, 'WARNING')
    #   description <- c(description, 'Species names in Species metadata are mispelled')
    # } else {
    #   step <- c(step, 'Species names spelling (species_md)')
    #   status <- c(status, 'PASS')
    #   description <- c(description, 'No mispelling in species names')
    # }

    # 6.2 plant md
    if (any(!plant_md_spnames$Concordance)) {
      step <- c(step, 'Species names spelling (plant_md)')
      status <- c(status, 'WARNING')
      description <- c(description, 'Species names in Plant metadata are mispelled')
    } else {
      step <- c(step, 'Species names spelling (plant_md)')
      status <- c(status, 'PASS')
      description <- c(description, 'No mispelling in species names')
    }

    # if (isTRUE(tryCatch(sapfluxnetQC1::qc_species_names(plant_md$pl_species),
    #                     error = function(e) return(TRUE)))) {
    #   step <- c(step, 'Species names spelling (plant_md)')
    #   status <- c(status, 'WARNING')
    #   description <- c(description, 'Species names in Plant metadata are mispelled')
    # } else {
    #   step <- c(step, 'Species names spelling (plant_md)')
    #   status <- c(status, 'PASS')
    #   description <- c(description, 'No mispelling in species names')
    # }

    # STEP 7
    # Species verification
    if (any(!sp_verification$Concordance)) {
      step <- c(step, 'Species names presence in Plant and Species metadata')
      status <- c(status, 'ERROR')
      description <- c(description, 'Species in Plant metadata not match species in Species metadata')
    } else {
      step <- c(step, 'Species names presence in Plant and Species metadata')
      status <- c(status, 'PASS')
      description <- c(description, 'Species are the same in Plant and Species metadata')
    }

    # STEP 8
    # Environmental variables presence
    if (any(!env_var_presence$Concordance)) {
      step <- c(step, 'Environmental variables presence')
      status <- c(status, 'ERROR')
      description <- c(description, 'Data and Metadata environmental variables do not agree')
    } else {
      step <- c(step, 'Environmental variables presence')
      status <- c(status, 'PASS')
      description <- c(description, 'Data and Metadata environmental variables agree')
    }

    # STEP 9
    # Create the results data frame
    res <- data.frame(QC_Step = step, Status = status, Description = description,
                      stringsAsFactors = FALSE)

    # STEP 10
    # Return the datatable
    res_table <- DT::datatable(res, class = 'display', rownames = FALSE,
                               extensions = c('Scroller', 'FixedColumns'),
                               caption = 'Table 1: Metadata Quality Check Summary',
                               options = list(dom = 't',
                                              columnDefs = list(list(className = 'dt-center',
                                                                     targets = 1),
                                                                list(className = 'dt-right',
                                                                     targets = 0),
                                                                list(width = '45%',
                                                                     targets = c(0, 2)),
                                                                list(width = '10%',
                                                                     targets = 1)),
                                              pageLength = 25,
                                              scrollY = 650, fixedColumns = TRUE, scrollCollapse = TRUE)) %>%
      DT::formatStyle('Status',
                      backgroundColor = DT::styleEqual(c('PASS', 'INFO',
                                                         'WARNING', 'ERROR'),
                                                       c('#26a65b', '#89c4f4',
                                                         '#f39c12', '#d91e18')))

    return(res_table)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_md_results_table', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_md_results_table', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_md_results_table', sep = '.'))})


}

################################################################################
#' Main function to resume Data QC in one data frame
#'
#' Data QC codified results in one data frame
#'
#' @family Quality Checks Functions
#'
#' @param sapf_data_fixed
#'
#' @param env_data_fixed
#'
#' @param timestamp_errors_sapf
#'
#' @param timestamp_errors_env
#'
#' @param sapw_md
#'
#' @param timestamp_concordance
#'
#' @param sapf_gaps_info
#'
#' @param env_gaps_info
#'
#' @param sapf_timestamp_nas
#'
#' @param env_timestamp_nas
#'
#' @param transformations_table
#'
#' @export

# START
# Function declaration
qc_data_results_table <- function(sapf_data_fixed, env_data_fixed, timestamp_errors_sapf,
                                  timestamp_errors_env, sapw_md,
                                  timestamp_concordance, sapf_gaps_info,
                                  env_gaps_info, sapf_timestamp_nas, env_timestamp_nas,
                                  transformations_table,
                                  parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Create the result vectors
    step <- vector()
    status <- vector()
    description <- vector()

    # STEP 2
    # Timestamps
    # 2.1 correct format sapf
    if (!qc_is_timestamp(sapf_data_fixed, FALSE, parent_logger = parent_logger)) {
      step <- c(step, 'TIMESTAMP Format Sapflow data')
      status <- c(status, 'ERROR')
      description <- c(description, 'TIMESTAMP format is incorrect and unfixable')
    } else {
      step <- c(step, 'TIMESTAMP Format Sapflow data')
      status <- c(status, 'PASS')
      description <- c(description, 'TIMESTAMP format is correct or has been fixed')
    }

    # 2.2 correct format env
    if (!qc_is_timestamp(env_data_fixed, FALSE, parent_logger = parent_logger)) {
      step <- c(step, 'TIMESTAMP Format Environmental data')
      status <- c(status, 'ERROR')
      description <- c(description, 'TIMESTAMP format is incorrect and unfixable')
    } else {
      step <- c(step, 'TIMESTAMP Format Environmental data')
      status <- c(status, 'PASS')
      description <- c(description, 'TIMESTAMP format is correct or has been fixed')
    }

    # 2.3 TIMESTAMP NAs sapf
    if (is.logical(sapf_timestamp_nas)) {
      step <- c(step, 'Sapflow TIMESTAMP NAs presence')
      status <- c(status, 'PASS')
      description <- c(description, 'No NAs detected in TIMESTAMP')
    } else {
      step <- c(step, 'Sapflow TIMESTAMP NAs presence')
      status <- c(status, 'ERROR')
      description <- c(description, 'TIMESTAMP has NAs')
    }

    # 2.3b TIMESTAMP NAs env
    if (is.logical(env_timestamp_nas)) {
      step <- c(step, 'Environmental TIMESTAMP NAs presence')
      status <- c(status, 'PASS')
      description <- c(description, 'No NAs detected in TIMESTAMP')
    } else {
      step <- c(step, 'Environmental TIMESTAMP NAs presence')
      status <- c(status, 'ERROR')
      description <- c(description, 'TIMESTAMP has NAs')
    }

    # 2.4 TIMESTAMP errors sapf
    if (length(timestamp_errors_sapf[[1]]) > 0) {
      step <- c(step, 'TIMESTAMP continuity errors Sapflow data')
      status <- c(status, 'WARNING')
      description <- c(description, 'TIMESTAMP continuity presents errors')
    } else {
      step <- c(step, 'TIMESTAMP continuity errors Sapflow data')
      status <- c(status, 'PASS')
      description <- c(description, 'TIMESTAMP continuity is fine')
    }

    # 2.5 TIMESTAMP errors env
    if (length(timestamp_errors_env[[1]]) > 0) {
      step <- c(step, 'TIMESTAMP continuity errors Environmental data')
      status <- c(status, 'WARNING')
      description <- c(description, 'TIMESTAMP continuity presents errors')
    } else {
      step <- c(step, 'TIMESTAMP continuity errors Environmental data')
      status <- c(status, 'PASS')
      description <- c(description, 'TIMESTAMP continuity is fine')
    }

    # 2.6 TIMESTAMP concordance
    if (all(timestamp_concordance$t0 %in% timestamp_concordance$t0[[1]]) &&
        all(timestamp_concordance$tf %in% timestamp_concordance$tf[[1]])) {
      step <- c(step, 'TIMESTAMP concordance between sapflow and environmental variables')
      status <- c(status, 'PASS')
      description <- c(description, 'Concordance OK')
    } else {
      step <- c(step, 'TIMESTAMP concordance between sapflow and environmental variables')
      status <- c(status, 'WARNING')
      description <- c(description, 'Concordance failed for one or more variables')
    }

    # 2.7 Gaps info, sapflow
    if (all(is.na(sapf_gaps_info$gap_coverage)) | length(sapf_gaps_info$gap_coverage) == 0) {
      step <- c(step, 'Sapflow gaps coverage')
      status <- c(status, 'PASS')
      description <- c(description, 'No gaps')
    } else {
      if (any(sapf_gaps_info$gap_coverage > 0.25)) {
        step <- c(step, 'Sapflow gaps coverage')
        status <- c(status, 'ERROR')
        description <- c(description, 'Presence of gaps covering more than 25% of the TIMESTAMP')
      } else {
        if (any(sapf_gaps_info$gap_coverage > 0.05)) {
          step <- c(step, 'Sapflow gaps coverage')
          status <- c(status, 'WARNING')
          description <- c(description, 'Presence of gaps covering 5-25% of the TIMESTAMP')
        } else {
          step <- c(step, 'Sapflow gaps coverage')
          status <- c(status, 'INFO')
          description <- c(description, 'Presence of gaps covering less than 5% of the TIMESTAMP')
        }
      }
    }

    # 2.8 Gaps info, environmental
    if (all(is.na(env_gaps_info$gap_coverage)) | length(env_gaps_info$gap_coverage) == 0) {
      step <- c(step, 'Environmental gaps coverage')
      status <- c(status, 'PASS')
      description <- c(description, 'No gaps')
    } else {
      if (any(env_gaps_info$gap_coverage > 0.25)) {
        step <- c(step, 'Environmental gaps coverage')
        status <- c(status, 'ERROR')
        description <- c(description, 'Presence of gaps covering more than 25% of the TIMESTAMP')
      } else {
        if (any(env_gaps_info$gap_coverage > 0.05)) {
          step <- c(step, 'Environmental gaps coverage')
          status <- c(status, 'WARNING')
          description <- c(description, 'Presence of gaps covering 5-25% of the TIMESTAMP')
        } else {
          step <- c(step, 'Environmental gaps coverage')
          status <- c(status, 'INFO')
          description <- c(description, 'Presence of gaps covering less than 5% of the TIMESTAMP')
        }
      }
    }

    # 2.9 transformation table
    if (any(!transformations_table$Available)) {
      step <- c(step, 'Data conversion and transformations')
      status <- c(status, 'WARNING')
      description <- c(description, 'One or more conversions/transformations are not available')
    } else {
      step <- c(step, 'Data conversion and transformations')
      status <- c(status, 'PASS')
      description <- c(description, 'All conversions/transformations are available')
    }

    # FINAL STEP
    # create the results object
    res <- data.frame(
      QC_Step = step, Status = status, Description = description,
      stringsAsFactors = FALSE
    )

    # return the table
    res_table <- DT::datatable(res, class = 'display', rownames = FALSE,
                               extensions = c('Scroller', 'FixedColumns'),
                               caption = 'Table 1: Data Quality Check Summary',
                               options = list(dom = 't',
                                              columnDefs = list(list(className = 'dt-center',
                                                                     targets = 1),
                                                                list(className = 'dt-right',
                                                                     targets = 0),
                                                                list(width = '45%',
                                                                     targets = c(0, 2)),
                                                                list(width = '10%',
                                                                     targets = 1)),
                                              pageLength = 25,
                                              scrollY = 650, fixedColumns = TRUE,
                                              scrollCollapse = TRUE)) %>%
      DT::formatStyle('Status',
                      backgroundColor = DT::styleEqual(c('PASS', 'INFO',
                                                         'WARNING', 'ERROR'),
                                                       c('#26a65b', '#89c4f4',
                                                         '#f39c12', '#d91e18')))

    return(res_table)

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_data_results_table', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_data_results_table', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_data_results_table', sep = '.'))})
}

################################################################################
#' Start QC process
#'
#' Start QC process from the site code.
#'
#' This function check the status of the site, starts it if it does not exists,
#' and start the QC process
#'
#' @family Quality Checks Functions
#'
#' @param folder Character string with the route to the folder to start QC
#'   process
#'
#' @param rdata Logical indicating if objects created in the QC must be saved in
#'   a file
#'
#' @return Invisible TRUE if all the process is ok, and invisible FALSE if there
#'   was some error in the process
#'
#' @export

# START
# Function declaration
qc_start_process <- function(folder = '.', rdata = TRUE,
                             parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if(!is.character(folder)) {
      stop('folder provided is not a character string')
    }

    # STEP 1
    # Get the files names, code and status of the site
    code_and_files <- dl_get_si_code(folder, parent_logger = parent_logger)
    status <- df_get_status(code_and_files[['si_code']],
                            parent_logger = parent_logger)

    # 1.1 Info message
    message('Starting process for ', code_and_files[['si_code']], ' site')

    # STEP 2
    # 2.1 if status exists and QC is DONE, don't do anything
    if (!is.logical(status)) {
      if(status$QC$DONE) {
        message(code_and_files[['si_code']],
                ' already passed QC, not doing anything else')
        return(invisible(FALSE))
      } else {

        # 2.2 if status exists but QC is not DONE, lets do it
        # # 2.2.1 log setup
        # log_sapfluxnet_setup('Logs/sapfluxnet.log',
        #                      logger = code_and_files[['si_code']],
        #                      level = "DEBUG")
        ## Log setup not necessary, it is done outside, in the main script

        # 2.2.2 report folder
        df_report_folder_creation(code_and_files[['si_code']],
                                  parent_logger = parent_logger)

        # 2.2.3 report
        rep_sfn_render('QC_report.Rmd',
                       output_file = file.path(
                         paste(format(Sys.time(), '%Y%m%d%H%M'),
                               code_and_files[['si_code']],
                               'QC_report.html', sep = '_')
                       ),
                       output_dir = file.path('Reports',
                                              code_and_files[['si_code']]),
                       parent_logger = parent_logger,
                       md_file = code_and_files[['md_file']],
                       sapf_data_file = code_and_files[['sapf_file']],
                       env_data_file = code_and_files[['env_file']],
                       code = code_and_files[['si_code']],
                       rdata = rdata)

        # 2.2.4 set status
        df_set_status(code_and_files[['si_code']],
                      QC = list(DONE = TRUE, DATE = as.character(Sys.Date())),
                      parent_logger = parent_logger)

        # 2.2.7 return invisible TRUE
        return(invisible(TRUE))
      }

    } else {

      # 2.3 If status does not exist, create it and perform the QC
      # 2.3.1 start status
      df_start_status(code_and_files[['si_code']], parent_logger = parent_logger)

      # 2.3.2 log setup
      log_sapfluxnet_setup('Logs/sapfluxnet.log',
                           logger = code_and_files[['si_code']],
                           level = "DEBUG")

      # 2.3.3 report folder
      df_report_folder_creation(code_and_files[['si_code']],
                                parent_logger = parent_logger)

      # 2.3.4 report
      rep_sfn_render('QC_report.Rmd',
                     output_file = file.path(
                       paste(format(Sys.time(), '%Y%m%d%H%M'),
                             code_and_files[['si_code']],
                             'QC_report.html', sep = '_')
                     ),
                     output_dir = file.path('Reports',
                                            code_and_files[['si_code']]),
                     parent_logger = parent_logger,
                     md_file = code_and_files[['md_file']],
                     sapf_data_file = code_and_files[['sapf_file']],
                     env_data_file = code_and_files[['env_file']],
                     code = code_and_files[['si_code']],
                     rdata = rdata)

      # 2.3.5 set status
      df_set_status(code_and_files[['si_code']],
                    QC = list(DONE = TRUE, DATE = as.character(Sys.Date())),
                    parent_logger = parent_logger)

      # 2.3.8 return invisible TRUE
      return(invisible(TRUE))

    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_start_process',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_start_process',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_start_process',
                                                        sep = '.'))})
}
