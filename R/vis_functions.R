################################################################################
# VISUALIZATION FUNCTIONS                                                      #
#                                                                              #
# Functions to visualize the data                                              #
################################################################################

################################################################################
#' Gaps density visualization
#'
#' Heatmap calendar to visualize gaps density
#'
#' @family Visualization functions
#'
#' @param data Data frame containing the data (env or sf) with the TIMESTAMP
#'   column and the environmental variables or the sapflow measures for the
#'   trees
#'
#' @return Plot is printed and a ggplot2 object is created if there is
#'   assignation
#'
#' @export

# START
# Function declaration
vis_gaps_calendar <- function(data, parent_logger = 'test') {

  # Using callin handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(data)) {
      stop('data provided is not a data frame')
    }

    # STEP 1
    # modify the data shape to be able to represent the visualization
    data %>%
      tidyr::gather(Id, Value, -TIMESTAMP) %>%

      # STEP 2
      # Create new variables containing time information
      dplyr::mutate(
        Year = lubridate::year(TIMESTAMP),
        Month = lubridate::month(TIMESTAMP, label = TRUE),
        Week = factor(lubridate::isoweek(TIMESTAMP)),
        Day = factor(lubridate::wday(TIMESTAMP, label = TRUE),
                     levels = rev(c('Mon', 'Tues', 'Wed', 'Thurs',
                                    'Fri', 'Sat', 'Sun')),
                     ordered = TRUE)
      ) %>%

      # STEP 3
      # Group by interest variables and summarise by n
      dplyr::group_by(Year, Month, Week, Day) %>%
      dplyr::summarise(n = sum(!is.na(Value))) %>%

      # STEP 4
      # Plot
      ggplot(aes(x = Week, y = Day, fill = n)) +
        geom_tile() +
        facet_grid(Year ~ Month, scales = 'free_x') +
        scale_fill_gradient(low = "#C8F7C5", high = "#26A65B") +
        theme_bw()

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'vis_gaps_calendar', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'vis_gaps_calendar', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'vis_gaps_calendar', sep = '.'))})
}

################################################################################
#' Plotting an histogram for gaps intervals/gaps coverage
#'
#' Wrapper for ggplot to plot an histogram of gaps info
#'
#' This function is a simple wrapper for ggplot + geom_histogram. It produces
#' a ggplot object that can be modified adding layers, like any other ggplot
#' object.
#'
#' @family Visualization Functions
#'
#' @param gaps_info Data frame as obtained from \code{\link{qc_mind_the_gap}}
#'
#' @param type Character indicating what to represent, \code{gap_interval} or
#'   \code{gap_coverage}
#'
#' @param binwidth Bin width as stated in geom_histogram, default to NULL to
#'   use the geom_histrogram default. Change it if more or less resolution is
#'   needed
#'
#' @return a ggplot object with the basic histogram, no themes added.
#'
#' @export

# START
# Function declaration
vis_plot_the_gap <- function(gaps_info, type = 'gap_interval', binwidth = NULL,
                            parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument check
    # Is gaps_info a data frame?
    if (!is.data.frame(gaps_info)) {
      stop('gaps_info is not a data frame')
    }
    # Has it the necessary variables, as produced by mind_the_gap?
    if (any(is.null(gaps_info$gap_interval), is.null(gaps_info$gap_coverage))) {
      stop('gaps_info has not the necessary variables,',
           ' see function help (?qc_plot_the_gap)')
    }

    # STEP 1
    # Create the ggplot object
    res_plot <- ggplot(gaps_info, aes_(x = type)) +
      geom_histogram(binwidth = binwidth)

    # STEP 2
    # Return the plot
    return(res_plot)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_plot_the_gap', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_plot_the_gap', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_plot_the_gap', sep = '.'))})
}
