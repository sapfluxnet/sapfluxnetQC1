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
