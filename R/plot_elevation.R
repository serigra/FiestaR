#' Plot Elevation Profile
#'
#' Generates elevation profile visualizations for a journey between
#' two locations.
#'
#' @param data Data frame containing the a distance and cumuluative distance ('cumulative_distance')
#' and elevation data ('elevation').
#' @param origin Character string specifying the route starting location.
#' @param destination Character string specifying the route destination.
#' @param max_ylim Numeric value setting the maximum y-axis limit for the elevation plot
#'   in meters (default: 1100).
#' @param add_text Logical indicating whether to add a text labels to the plot (default: TRUE).
#' @param add_box Logical indicating whether to add a box around the plot (default: TRUE).
#' @param color_profile Character string specifying the color for the elevation profile line (default: "#235347").
#' @param text_size Numeric value specifying the size of the text labels (default: 3.7).
#' @param .ggplot Additional ggplot arguments.
#'
#' @return ggplot object showing the elevation profile with LOESS smoothing.
#'
#' @details
#' The function generates a visualization of the elevation profile for a specified route.
#' It uses LOESS smoothing to create a smooth curve representing the elevation changes along the route.
#'
#' @note
#' This function requires the following packages: dplyr, stringr, ggplot2, monochromeR, patchwork, ggforce, ggmap, sf, elevatr, and geosphere.
#' A API key to the Google cloud platform is needed! see tutorial https://www.appsilon.com/post/r-ggmap.
#' An active internet connection is required to retrieve route and elevation data.
#'
#'
#' @export

plot_elevation <- function(data,
                           origin = "Origin",
                           destination = "Destination",
                           max_ylim = 1100,
                           add_text = TRUE,
                           add_box = TRUE,
                           color_profile = "#235347",
                           text_size = 3.7,
                           .ggplot = NULL,
                           ...
                           ) {


    if(!base::all(c("distance", "cumulative_distance", "elevation") %in% names(data))) {
      stop("Data must contain 'distance', 'cumulative_distance' and 'elevation' column")
    }

   # ---------------------------- PREPARE LABELS -------------------------------

   if(base::all(c("origin", "destination") %in% names(data))) {
     message("Data frame contains 'origin' and 'destination' columns
             -> overwrites 'origin' and 'destination' arguments.")
     origin <- data$origin[1]
     destination <- data$destination[1]
   }

   origin_label <- stringr::str_to_title(stringr::str_extract(origin, "^[^,]+"))
   destination_label <- stringr::str_extract(destination, "^[^,]+")

   # --------------------------- PLOT COMPONENTS -------------------------------

   .theme.default <- list(

         ggplot2::theme_void(),
         ggplot2::theme(plot.margin = ggplot2::unit(c(0.2, 0.4, 0.2, 0.4), "cm"),
                        panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                        legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
                        legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA)
                        )
   )

  # ---------------------------- PLOT ELEVATION PROFILE ------------------------

  p.elevation <-
    ggplot2::ggplot(data, ggplot2::aes(x = cumulative_distance, y = elevation)) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, span = 0.1, color = color_profile, linewidth = 1) +
    .theme.default +
    .ggplot


  # --------------------------- ADD TEXT ANNOTATIONS ---------------------------

  if(add_text){

    range_elevation <- data |>
      dplyr::summarise(min_elev = min(elevation),
                max_elev = max(elevation),
                plot_min = min_elev - 0.18 * (max_elev - min_elev),
                plot_max = max_elev + 0.8 * (max_elev - min_elev))

    p.elevation <- p.elevation +
      ggplot2::geom_text(data = data |> dplyr::filter(distance == 0),
                         ggplot2::aes(x = distance,
                                      y = elevation,
                                      label = paste0(origin_label, '\n', round(elevation), ' m')),
                                      vjust = -0.3 , hjust = 0.1,
                                      color = color_profile, size = text_size) +
      ggplot2::geom_text(data = data |> tail(1),
                         ggplot2::aes(x = cumulative_distance,
                                      y = elevation,
                                      label = paste0(destination_label, '\n', round(elevation), ' m')),
                                      vjust = -0.3 , hjust = 0.8,
                                      color = color_profile, size = text_size) +
      ggplot2::ylim(range_elevation$plot_min, range_elevation$plot_max) +
      ggplot2::coord_cartesian(clip = 'off')

  }

  # ------------------------ ADD BOX & DELTA ELEVATION  ------------------------

  if(add_box){

    p.box <- plot_box(...)

    # calculate elevation difference between start and end point
    delta_elevation <- tail(data$elevation, 1) - head(data$elevation, 1)
    delta_elevation <- paste0(delta_elevation, " m")

    color_profile_dark <- monochromeR::generate_palette(color_profile,
                                                        modification = "go_darker", n_colors = 5)[2]
    p.delta <- ggplot2::ggplot() +
      ggplot2::annotate("text",
               x = 0.5, y = 0.5,
               label = sprintf('atop(Delta~Altitude, bold("%s"))', delta_elevation),
               parse = TRUE,
               size = 5,
               color = color_profile_dark) +
      .theme.default

    p.elev.delta <- p.elevation + p.delta +
      patchwork::plot_layout(widths = ggplot2::unit(c(4, 1), c("null")))

    p.elevation <- p.box +
      patchwork::inset_element(p.elev.delta, left = 0.005, bottom = 0, right = 0.93, top = 1)

  }

  return(p.elevation)

}
