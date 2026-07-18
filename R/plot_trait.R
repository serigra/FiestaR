#' Create Gradient Sliders for Traits
#'
#' Generates a visualization of five character traits (Strength, Intelligence,
#' Agility, Charisma, Endurance) with gradient-colored horizontal sliders showing
#' the corresponding values from 0 to 10. Optionally adds a decorative box around the plot.
#'
#' @param data Data for a single person including the name of the person as well
#' as the 5 traits in separate columns. Trait values range between 0 and 10.
#' @param trait_mean Optional named numeric vector of length 5 specifying the mean values for each trait.
#' @param trait_color A named character vector of length 5 specifying the colors for
#'   each trait. Default colors are: red ("#D7263D") for Adventurer, teal
#'   ("#1B9AAA") for Optimist, yellow ("#F4D35E") for Dreamer, purple
#'   ("#6A4C93") for Hipster, and green ("#3F784C") for Nerd.
#' @param trait_size Numeric value controlling the size of the trait labels.
#' @param trait_text_font Character string specifying the font family for the trait labels.
#' @param trait_text_color Character string specifying the color for the trait labels.
#' @param trait_point_color Color for the point indicating the trait value.
#' @param trait_value_color Color for the text label on the point indicating the trait value.
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
    data,
    trait_mean = NULL,
    trait_color = c(Adventurer = "#D7263D",
                     Optimist = "#1B9AAA",
                     Dreamer = "#F4D35E",
                     Hipster = "#6A4C93",
                     Nerd = "#3F784C"),
    trait_point_color = "#235347",
    trait_value_color = "white",
    trait_text_font = "quicksand",
    trait_text_color = "#235347",
    trait_size = 14,
    add_box = TRUE,
    ...
  ){

  # ------------------------------- CHECK ARGUMENTS ----------------------------

  # error if more than one row is provided
  stopifnot(nrow(data) == 1)

  # data: wide to long format
  trait_names <- names(data)[names(data) != "name"]

  # check whether color_traits is a named vector
  if(!all(attributes(trait_color)$names %in% trait_names)) {
    missing_traits <- setdiff(names(trait_color), trait_names)
    stop(
      "color_traits must be a named vector with names matching trait names in the input data.\n ",
      "Trait names in input data: ", paste(trait_names, collapse = ", "), ".\n",
      "Unknown trait(s) in trait_color: ", paste(missing_traits, collapse = ", ")
    )
  }

  # NEW: validate trait_mean if supplied
  if (!is.null(trait_mean)) {
    if (is.null(names(trait_mean)) || !all(names(trait_mean) %in% trait_names)) {
      stop(
        "trait_mean must be a named numeric vector with names matching trait names in the input data.\n ",
        "Trait names in input data: ", paste(trait_names, collapse = ", ")
      )
    }
  }

  # ------------------------------ PREPARE DATA --------------------------------

  d.traits <- data |>
    tidyr::pivot_longer(cols = -.data$name, names_to = 'trait') |>
    dplyr::mutate(
      trait = factor(.data$trait, levels = trait_names)
    )

  # NEW: prepare mean data (only for traits present in trait_mean)
  if (!is.null(trait_mean)) {
    d.means <- data.frame(
      trait = factor(names(trait_mean), levels = trait_names),
      value = as.numeric(trait_mean)
    )
  }

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

    # 4. add value dots
    ggplot2::geom_point(
      data = d.traits,
      ggplot2::aes(x = .data$value, y = .data$trait),
      shape = 21, size = 12, stroke = 1.6, fill = trait_point_color, color = "white" #, fill = "grey80"
      ) +

    # 5. add text to value dot
    ggplot2::geom_text(
      data = d.traits,
      ggplot2::aes(x = .data$value, y = .data$trait,
                   label = paste0(round(.data$value, 1), '0%')#, color = .data$trait
                   ),
      vjust = 0.5, hjust = 0.50, size = 3.2, fontface = "bold", color = trait_value_color)

    # 6.  overlay mean diamonds, if supplied
    if (!is.null(trait_mean)) {

      col_dot <-monochromeR::generate_palette("#235347", "go_lighter", 5)[2]

      p.sliders <- p.sliders +
      ggplot2::geom_point(
        data = d.means,
        ggplot2::aes(x = .data$value, y = .data$trait),
        shape = 21, size = 3, stroke = 1, color = col_dot, fill = "transparent"
        )

      # --- small legend in bottom-right ---
      # use numeric y so we can place it slightly below the lowest trait
      y_num <- Inf

      p.sliders <- p.sliders +
        ggplot2::annotate(
          "point",
          x = 9.6, y = y_num - 2,
          shape = 21, size = 3, stroke = 1,
          color = col_dot, fill = "transparent"
        ) +
        ggplot2::annotate(
          "text",
          x = 9.3, y = y_num - 2,
          label = "overall mean",
          hjust = 1, vjust = 0.5,
          size = 3
        )
    }

  # add colors & format
  p.sliders <- p.sliders +
    ggplot2::scale_color_manual(values = trait_color) +
    ggplot2::scale_fill_manual(values = trait_color) +
    ggplot2::scale_alpha_continuous(range = c(0.05, 1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = trait_size) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold", color = trait_text_color,
                                          family = trait_text_font),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 15, 10, 15)
    )

  # ------------------------------ ADD BOX -------------------------------------

  if(add_box){

    p.box <- plot_box(...)

    p.sliders <- p.box +
      patchwork::inset_element(p.sliders, left = 0.04, bottom = 0.04, right = 0.96, top = 0.96)

  }

  return(p.sliders)

}
