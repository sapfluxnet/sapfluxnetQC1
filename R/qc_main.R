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
    }

    # 2.2 ClassOK
    if (any(!md_cols$ClassOK & !md_cols$IsNA)) {
      step <- c(step, 'Metadata variables expected class')
      status <- c(status, 'ERROR')
      description <- c(description, 'One or more variables have the wrong class')
    }

    # 2.3 NAs
    if (any(md_cols$IsNA)) {
      step <- c(step, 'Metadata variables NA presence')
      status <- c(status, 'INFO')
      description <- c(description, 'Some variables have no value')
    }

    # STEP 3
    # Metadata factor values
    # 3.1 Wrong value
    if (any(!factor_values$Check_result & !factor_values$NA_presence)) {
      step <- c(step, 'Metadata factor variable values')
      status <- c(status, 'ERROR')
      description <- c(description, 'One or more metadata factor variables have values not accepted')
    }

    # STEP 4
    # Metadata email
    if (all(is.na(email_check$Is_correct)) | any(!email_check$Is_correct, na.rm = TRUE)) {
      step <- c(step, 'Email check')
      status <- c(status, 'WARNING')
      description <- c(description, 'Email is missing or in worng format')
    }

    # STEP 5
    # Coordinates check
    if (!site_md_coordfix$is_inside_country) {
      step <- c(step, 'Site coordinates')
      status <- c(status, 'WARNING')
      description <- c(description, 'Site provided coordinates are incorrect and not fixable')
    }

    # STEP 6
    # Species names
    # 6.1 species md
    if (isTRUE(tryCatch(sapfluxnetr::qc_species_names(species_md$sp_name),
                        error = function(e) return(TRUE)))) {
      step <- c(step, 'Species names spelling')
      status <- c(status, 'WARNING')
      description <- c(description, 'Species names in Species metadata are mispelled')
    }

    # 6.2 plant md
    if (isTRUE(tryCatch(sapfluxnetr::qc_species_names(plant_md$pl_species),
                        error = function(e) return(TRUE)))) {
      step <- c(step, 'Species names spelling')
      status <- c(status, 'WARNING')
      description <- c(description, 'Species names in Plant metadata are mispelled')
    }

    # STEP 7
    # Species verification
    if (!sp_verification$coincidence) {
      step <- c(step, 'Species names presence in Plant and Species metadata')
      status <- c(status, 'ERROR')
      description <- c(description, 'Species in Plant metadata not match species in Species metadata')
    }

    # STEP 8
    # Environmental variables presence
    if (any(!env_var_presence$Concordance)) {
      step <- c(step, 'Environmental variables presence')
      status <- c(status, 'ERROR')
      description <- c(description, 'Metadata and Data environmental variables do not agree')
    }

    # STEP 9
    # Create the results data frame
    res <- data.frame(QC_Step = step, Status = status, Description = description)

    # STEP 10
    # Return the datatable
    res_table <- DT::datatable(res, class = 'display', rownames = FALSE,
                               caption = 'Table: Metadata Quality Check Summary',
                               options = list(dom = 't')) %>%
      DT::formatStyle('Status',
                      backgroundColor = DT::styleEqual(c('INFO', 'WARNING', 'ERROR'),
                                                       c('#89C4F4', '#F5AB35', '#F22613')))

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
