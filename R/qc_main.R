################################################################################
#' Main function to resume QC in one data frame
#'
#' QC codified results in one data frame
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
    if (any(!md_cols$ClassOK & !md_cols$IsNA)) {
      step <- c(step, 'Metadata variables expected class')
      status <- c(status, 'ERROR')
      description <- c(description, 'One or more variables have the wrong class')
    } else {
      step <- c(step, 'Metadata variables expected class')
      status <- c(status, 'PASS')
      description <- c(description, 'All metadata variables have the correct class')
    }

    # 2.3 NAs
    if (any(md_cols$IsNA)) {
      step <- c(step, 'Metadata variables NA presence')
      status <- c(status, 'INFO')
      description <- c(description, 'Some variables have no value')
    } else {
      step <- c(step, 'Metadata variables NA presence')
      status <- c(status, 'PASS')
      description <- c(description, 'No NAs in metadata')
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


    # if (isTRUE(tryCatch(sapfluxnetr::qc_species_names(species_md$sp_name),
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

    # if (isTRUE(tryCatch(sapfluxnetr::qc_species_names(plant_md$pl_species),
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
    res <- data.frame(QC_Step = step, Status = status, Description = description)

    # STEP 10
    # Return the datatable
    res_table <- DT::datatable(res, class = 'display', rownames = FALSE,
                               caption = 'Table 1: Metadata Quality Check Summary',
                               options = list(dom = 't',
                                              columnDefs = list(list(className = 'dt-center',
                                                                     targets = 1:2),
                                                                list(className = 'dt-right',
                                                                     targets = 0)),
                                              pageLength = 25,
                                              autoWidth = TRUE)) %>%
      DT::formatStyle('Status',
                      backgroundColor = DT::styleEqual(c('PASS', 'INFO', 'WARNING', 'ERROR'),
                                                       c('#26a65b', '#89c4f4', '#f39c12', '#d91e18')))

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
