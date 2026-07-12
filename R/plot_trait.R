#' Create Gradient Sliders for Traits
#'
#' Generates a visualization of five character traits (Strength, Intelligence,
#' Agility, Charisma, Endurance) with gradient-colored horizontal sliders showing
#' the corresponding values from 0 to 10. Optionally adds a decorative box around the plot.
#'
#' @param data Data for a single person including the name of the person as well
#' as the 5 traits in separate columns. Trait values range between 0 and 10.
#' @param color_traits A named character vector of length 5 specifying the colors for
#'   each trait. Default colors are: red ("#D7263D") for Adventurer, teal
#'   ("#1B9AAA") for Optimist, yellow ("#F4D35E") for Dreamer, purple
#'   ("#6A4C93") for Hipster, and green ("#3F784C") for Nerd.
#' @param add_box Logical. If \code{TRUE} (default), wraps the plot in a
#'   decorative box using \code{plot_box()}.
#' @param ... Additional arguments passed to \code{box_plot()} when
#'   \code{add_box = TRUE}.
#'
#' @return A \pkg{ggplot2} object (or patchwork composition if \code{add_box = TRUE})
#'   displaying horizontal slider-style bars for each trait with gradient
#'   coloring, tick marks, and value labels.
#'
#' @importFrom rlang .data
#'
#' @export

plot_trait <- function(
    data = NULL,
    color_traits = c(Adventurer = "#D7263D",
                     Optimist = "#1B9AAA",
                     Dreamer = "#F4D35E",
                     Hipster = "#6A4C93",
                     Nerd = "#3F784C"),
    add_box = TRUE,
    ...
  ){

  # ------------------------------- CHECK ARGUMENTS ----------------------------

  # error if more than one row is provided
  stopifnot(nrow(data) == 1)

  # data: wide to long format
  trait_names <- names(data)[names(data) != "Name"]

  # check whether color_traits is a named vector
  if(!all(attributes(color_traits)$names %in% trait_names)) {
    missing_traits <- setdiff(names(color_traits), trait_names)
    stop(
      "color_traits must be a named vector with names matching trait names in the input data.\n ",
      "Trait names in input data: ", paste(trait_names, collapse = ", "), ".\n",
      "Unknown trait(s) in color_traits: ", paste(missing_traits, collapse = ", ")
    )
  }

  # ------------------------------ PREPARE DATA --------------------------------

  d.traits <- data |>
    tidyr::pivot_longer(cols = -.data$Name, names_to = 'trait') |>
    dplyr::mutate(
      trait = factor(.data$trait, levels = trait_names)
    )

  d.bar_segments <- d.traits |>
    dplyr::rowwise() |>
    dplyr::do({
      value <- .data$value
      data.frame(
        trait = .data$trait,
        x = seq(0, value - 0.1, by = 0.1),
        xend = seq(0.1, value, by = 0.1),
        alpha = seq(0.1, value, by = 0.1) / 10
      )
    }) |>
    dplyr::ungroup()

  d.ticks <- d.traits |>
    dplyr::select(.data$trait) |>
    tidyr::crossing(x = seq(0, 10, by = 2.5))


  # ------------------------------- PLOT SLIDERS -------------------------------

  p.sliders <- ggplot2::ggplot() +

    # 1. grey background bars (full length)
    ggplot2::geom_segment(
      data = d.traits,
      ggplot2::aes(x = 0, xend = 10, y = .data$trait, yend = .data$trait),
      linewidth = 7.5, color = "grey80", lineend = "round") +

    # 2. colored gradient bars (only until value)
    ggplot2::geom_segment(
      data = d.bar_segments,
      ggplot2::aes(x = .data$x, xend = .data$xend,
                   y = .data$trait, yend = .data$trait,
                   color = .data$trait, alpha = .data$alpha),
      linewidth = 6, lineend = "round") +

    # 3. add ticks
    ggplot2::geom_segment(
      data = d.ticks,
      ggplot2::aes(x = .data$x, xend = .data$x,
                   y = as.numeric(.data$trait) - 0.17, yend = as.numeric(.data$trait) - 0.10),
      color = "grey40", linewidth = 0.6, alpha = 0.7) +

    # 4. add value dots
    ggplot2::geom_point(
      data = d.traits,
      ggplot2::aes(x = .data$value, y = .data$trait),
      shape = 21, size = 6, stroke = 1.6, color = "grey50", fill = "grey80") +

    # 5. add text to value dot
    ggplot2::geom_text(
      data = d.traits,
      ggplot2::aes(x = .data$value, y = .data$trait,
                   label = round(.data$value, 1), color = .data$trait),
      vjust = 0.4, hjust = -2.4, size = 3.7, fontface = "bold") +

    # colors & format
    ggplot2::scale_color_manual(values = color_traits) +
    ggplot2::scale_alpha_continuous(range = c(0.05, 1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )

  # ------------------------------ ADD BOX -------------------------------------

  if(add_box){

    p.box <- plot_box(...)

    p.sliders <- p.box +
      patchwork::inset_element(p.sliders, left = 0.04, bottom = 0, right = 0.96, top = 1)

  }

  return(p.sliders)

}
