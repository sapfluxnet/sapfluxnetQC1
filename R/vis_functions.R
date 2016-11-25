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
        # viridis::scale_fill_viridis() +
        theme_sfn()

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
#'   needed. Only works for \code{type = 'gap_interval'}.
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
           ' see function help (?vis_plot_the_gap)')
    }
    # Is data empty (no gaps found)
    if (gaps_info[1,1] == 'No gaps found') {

      # create an empty plot
      res_plot <- ggplot(data.frame(x = c(1,5,10), y =c(1,5,10)),
                         aes(x = x, y = y)) +
        geom_blank() +
        annotate('text', x = 5, y = 5, label = 'No gaps found') +
        theme_void()

      # return empty plot
      return(res_plot)
    }

    # STEP 1
    # Create the ggplot object

    # 1.1 gap_coverage special effects
    if (type == 'gap_coverage') {
      res_plot <- gaps_info %>%
        dplyr::mutate(gap_coverage = gap_coverage * 100) %>%
        ggplot(aes_string(x = type)) +
        geom_histogram(binwidth = 5,
                       fill = viridis::viridis(1)) +
        scale_x_continuous(limits = c(NA, 105)) +
        labs(x = 'Gap coverage (%)', y = 'Count') +
        theme_sfn()
    } else {

      # 1.2 gap_interval special effects
      res_plot <- ggplot(gaps_info, aes_string(x = type)) +
        geom_histogram(binwidth = binwidth,
                       fill = viridis::viridis(1)) +
        labs(x = 'Gap interval (minutes)', y = 'Count') +
        theme_sfn()
    }

    # STEP 2
    # Return the plot
    return(res_plot)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'vis_plot_the_gap', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'vis_plot_the_gap', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'vis_plot_the_gap', sep = '.'))})
}

################################################################################
#' TIMESTAMP with gaps visualization
#'
#' Concordance lines plot with gaps
#'
#' @family Visualization Functions
#'
#' @param sapf_gaps Data frame with the sapflow gap info as obtained from
#'   \code{\link{qc_mind_the_gap}}
#'
#' @param env_gaps Data frame with the env gap info as obtained from
#'   \code{\link{qc_mind_the_gap}}
#'
#' @param sapf_intervals Data frame with the sapflow intervals info as obtained from
#'   \code{\link{qc_time_interval}}
#'
#' @param env_intervals Data frame with the env intervals info as obtained from
#'   \code{\link{qc_time_interval}}
#'
#' @return A ggplot object with the basic lines plot, no themes added.
#'
#' @export

# START
# Function declaration
vis_gap_lines <- function(sapf_gaps = NULL, env_gaps = NULL,
                          sapf_intervals = NULL, env_intervals = NULL,
                          parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Get the data ready to plot
    # 1.1 sapf
    # sapf_intervals <- qc_time_interval(sapf_data)
    sapf_intervals$Object[1] <- 'Total_sapf'

    # sapf_gaps <- qc_mind_the_gap(sapf_data)

    # if no gaps, no join
    if (sapf_gaps$Object[1] == 'No gaps found') {
      sapf_tmp_data <- sapf_intervals
    } else {
      sapf_tmp_data <- dplyr::full_join(sapf_intervals, sapf_gaps, by = 'Object')
    }

    # 1.2 env
    # env_intervals <- qc_time_interval(env_data)
    env_intervals$Object[1] <- 'Total_env'

    # env_gaps <- qc_mind_the_gap(env_data)

    # if no gaps, no join
    if (env_gaps$Object[1] == 'No gaps found') {
      env_tmp_data <- env_intervals
    } else {
      env_tmp_data <- dplyr::full_join(env_intervals, env_gaps, by = 'Object')
    }

    # 1.3 all
    gaps_info <- dplyr::bind_rows(env_tmp_data, sapf_tmp_data)

    # STEP 2
    # For loop
    # 2.1 Initiate res vectors
    x_start <- vector()
    x_end <- vector()
    y_start <- vector()
    y_end <- vector()

    # 2.3 Get the object names
    object_names <- unique(gaps_info$Object)

    # 2.4 For loop
    for (obj in object_names) {
      # data by object
      tmp_data <- gaps_info %>%
        dplyr::filter(Object == obj)
      # update the vectors
      # 2.4.1 no gaps
      if (all(is.na(tmp_data$gap_start))) {
        x_start <- c(x_start, tmp_data$t0)
        x_end <- c(x_end, tmp_data$tf)
        y_start <- c(y_start, as.character(tmp_data$Object))
        y_end <- c(y_end, as.character(tmp_data$Object))
      } else {
        # 2.4.2 gaps
        x_start <- c(x_start, tmp_data$timestamp_start[[1]], tmp_data$gap_end)
        x_end <- c(x_end, tmp_data$gap_start, tmp_data$timestamp_end[[1]])
        y_start <- c(y_start, as.character(tmp_data$Object),
                     as.character(tmp_data$Object[[1]]))
        y_end <- c(y_end, as.character(tmp_data$Object), as.character(tmp_data$Object[[1]]))
      }

    }

    # STEP 3
    # Build the plot data
    plot_data <- data.frame(
      x_start = as.POSIXct(x_start, origin = lubridate::origin),
      x_end = as.POSIXct(x_end, origin = lubridate::origin),
      y_start = y_start,
      y_end = y_end,
      stringsAsFactors = FALSE
    ) %>%
      dplyr::mutate(y_start = factor(y_start, levels = rev(unique(y_start)))) %>%
      dplyr::mutate(y_end = factor(y_end, levels = rev(unique(y_end))))

    # STEP 4
    # Build the plot
    res_plot <- ggplot(plot_data, aes(x = x_start, y = y_start, color = y_start)) +
      geom_segment(aes(xend = x_end, yend = y_end), size = 2) +
      geom_point(aes(x = x_start, y = y_start)) +
      geom_point(aes(x = x_end, y = y_end)) +
      scale_x_datetime(date_breaks = '1 month') +
      scale_colour_manual(values = c(rep(viridis::viridis(1),
                                         length(unique(sapf_intervals$Object))),
                                     rep(viridis::viridis(3)[2],
                                         length(unique(env_intervals$Object))))) +
      labs(x = 'TIMESTAMP', y = 'Object') +
      theme_sfn() +
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 30, margin = margin(t = 15)))

    # 3.1 And return it, by the power of return!!
    return(res_plot)

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'vis_gap_lines', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'vis_gap_lines', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'vis_gap_lines', sep = '.'))})
}

################################################################################
#' ggplot2 theme for SAPFLUXNET plots
#'
#' Custom ggplot2 theme for uniformization of plot visuals
#'
#' @export

theme_sfn <- function(base_size = 10, base_family = "Lato") {
  half_line <- base_size/2
  theme(line = element_line(colour = "black", size = 1,
                            linetype = 1, lineend = "butt"),
        rect = element_rect(fill = NA, colour = "black",
                            size = 1, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5,
                            vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE),
        axis.line = element_blank(),
        # axis.line.x = element_line(),
        # axis.line.y = element_line(),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line*2.5),
                                   vjust = 1),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line*2),
                                   hjust = 1),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(-half_line, "pt"),
        axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
                                                    b = 0.8 * half_line/2)),
        axis.title.y = element_text(angle = 90,
                                    margin = margin(r = 0.8 * half_line,
                                                    l = 0.8 * half_line/2)),
        legend.background = element_rect(colour  = NA, fill = ),
        legend.spacing = unit(1, "pt"),
        legend.key = element_rect(colour = NA),
        legend.key.size = unit(1, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.8)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0.5),
        legend.title.align = 0,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "top",
        legend.box = NULL,
        panel.background = element_blank(),
        panel.border = element_rect(),
        panel.grid = element_blank(),
        # panel.grid.major = element_line(colour = "black", size = rel(0.3),
        #                                 linetype = 2),
        # panel.grid.minor = element_blank(),
        # panel.grid.major.x = element_blank(),
        panel.spacing = unit(half_line, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = TRUE,
        strip.background = element_rect(size = rel(0.3)),
        strip.text = element_text(colour = "grey10", size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line,
                                                    b = half_line)),
        strip.text.y = element_text(angle = -90,
                                    margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_blank(),
        plot.title = element_text(size = rel(1.2),
                                  margin = margin(b = half_line * 1.2)),
        plot.margin = margin(half_line, half_line, half_line, half_line),

        complete = TRUE)
}

################################################################################
#' Plotting a diagram of biomes
#'
#' This function produces a ggplot object showing the biomes as colored areas
#' according to mean annual temperature (MAT) and mean annual precipitation (MAP)
#' using a SpatialPolygonsDataFrame object obtained with
#' \code{\link{qc_get_biomes_spdf}}
#'
#' @family Visualization Functions
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#' in a single biome. By default, deserts are not merged.
#'
#' @return a ggplot object showing the biomes.
#'
#' @export

# START
# Function declaration
vis_biome <- function(merge_deserts = FALSE, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is merge_deserts logical?
    if (!(is.logical(merge_deserts))) {
      stop('merge_deserts must be logical')
    }
    # Is merge_deserts NA?
    if (is.na(merge_deserts)) {
      stop('merge_deserts must be either TRUE or FALSE')
    }

    # STEP 1
    # Get biomes SpatialPointsDataFrame object
    suppressMessages(
      biomes_df <- fortify(qc_get_biomes_spdf(merge_deserts = merge_deserts))
    )

    # STEP 2
    # Make and return the plot object
    # 2.1 Make color palette
    if (merge_deserts){

      pal <- viridis::viridis(9)[c(2,9,3,4,6,7,8,1)]

    } else {

      pal <- viridis::viridis(9)[c(2,3,5,4,9,6,7,8,1)]

    }

    # 2.2 Make the plot object
    plot <- ggplot() + geom_polygon(data = biomes_df, aes(x = long, y = lat, group = id, fill = id)) +
      scale_fill_manual('Biomes', values = pal) + xlab('Mean annual precipitation (mm)') +
      ylab('Mean annual temperature (ºC)')

    # 2.3 Return the plot object
    return(plot)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'vis_biome', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'vis_biome', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'vis_biome', sep = '.'))})

}

################################################################################
#' Plotting a diagram of biomes with sites as dots
#'
#' This function produces a ggplot object showing the biomes as colored areas
#' according to mean annual temperature (MAT) and mean annual precipitation (MAP),
#' using the function \code{\link{vis_biome}}, and adds the sites on it according
#' to their values of MAT and MAP.
#'
#' @family Visualization Functions
#'
#' @param data Data frame of site metadata, including mean annual temperature
#' (si_mat) and mean annual precipitation (si_map) columns, or at least
#' latitude (si_lat) and longitude (si_long) columns that will be used to obtain
#' climatic data with \code{\link{qc_get_biome}}.
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#' in a single biome. By default, deserts are not merged.
#'
#' @return a ggplot object showing the biomes.
#'
#' @export

# START
# Function declaration
vis_location_biome <- function(data, merge_deserts = FALSE,
                               parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data.frame?
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data.frame.',
           ' Please verify if it is the correct object')
    }
    # Does data contains a longitude variable?
    if (is.null(data$si_long)) {
      stop('There is no longitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    # Does data contains a latitude variable?
    if (is.null(data$si_lat)) {
      stop('There is no latitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    # Is merge_deserts logical?
    if (!(is.logical(merge_deserts))) {
      stop('merge_deserts must be logical')
    }
    # Is merge_deserts NA?
    if (is.na(merge_deserts)) {
      stop('merge_deserts must be either TRUE or FALSE')
    }

    # STEP 1
    # Get MAT and MAP if not provided
    if (!all(c('si_mat', 'si_map') %in% names(data))){
      data <- qc_get_biome(data, merge_deserts = merge_deserts)
    }

    # STEP 2
    # Make the plot
    # 2.1 Get biome plot
    plot <- vis_biome(merge_deserts = merge_deserts)

    # 2.2 Make the plot object
    plot <- plot +
      geom_point(data = data, aes(x = si_map, y = si_mat
                                  # , tooltip = si_code
                                  ),
                 color = 'black', shape = 21, fill = 'white', size = 2, stroke = 0.5) +
      theme_bw() + coord_cartesian(xlim = c (0, 4500), ylim = c(-16, 30), expand = FALSE)

    # 2.3 Return the plot object
    return(plot)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'vis_location_biome', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'vis_location_biome', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'vis_location_biome', sep = '.'))})

}
