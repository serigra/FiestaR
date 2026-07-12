#' Plot Match and Anti-Match Labels
#'
#' Creates a two-panel stacked plot displaying a "match" label and an "anti-match" label
#' (both prefixed with an emoji), each optionally enclosed in a decorative box,
#' and combined vertically using \pkg{patchwork}.
#'
#' @param data Data frame including the columns "match" and "antimatch" with the
#' names or text to display in the corresponding panels.
#' @param add_box Logical. If \code{TRUE} (default), a box (generated via
#'   \code{plot_box()}) is drawn behind each label panel.
#' @param text_color Character. Color used for the label text. Defaults to
#'   \code{"#235347"}.
#' @param text_size Numeric. Size of the label text. Defaults to \code{15}.
#' @param margin_size Numeric. Plot margin (applied equally to all four sides, in
#'   points) for each panel. Defaults to \code{10}.
#' @param ... Additional arguments passed to \code{box_box()} when
#'   \code{add_box = TRUE}.
#'
#' @details
#' The function builds a small data frame containing the match and
#' anti-match labels, then creates two separate \pkg{ggplot2} plots (one per
#' label) using \code{geom_text()} with \code{theme_void()}. If
#' \code{add_box = TRUE}, each label plot is inset onto a box plot produced
#' by \code{box_plot(...)} using \code{patchwork::inset_element()}. The two
#' resulting plots are then stacked vertically with \code{patchwork} using
#' equal heights.
#'
#' @return A \pkg{patchwork} object combining the match and anti-match plots
#'   stacked vertically.
#'
#' @importFrom rlang .data
#'
#' @export
plot_match_antimatch <- function(data = NULL,
                                 add_box = TRUE,
                                 text_color = "#235347",
                                 text_size = 15,
                                 margin_size = 10,
                                 ...) {

  # ------------------------------- CHECK ARGUMENTS ----------------------------

  # error if more than one row is provided
  stopifnot(nrow(data) == 1)

  # check for required columns
  required_cols <- c("match", "antimatch")
  if(!all(required_cols %in% names(data))) {
    missing_traits <- setdiff(required_cols, names(data))
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "), "\n",
         "Missing are: ", paste(missing_traits, collapse = ", ")
         )
  }

  # ---------------------------- PREPARE DATA ----------------------------------

  df <- data.frame(
    x = 0.5, y = 0.5,
    label = c( paste(emoji::emoji("heart-with-arrow"), data$match),
               paste(emoji::emoji("broken-heart"), data$antimatch)),
    type = c("match", "antimatch")
  )

  # -------------------------- PLOT COMPONENTS ---------------------------------

  plot_name <- function(data_){

    ggplot2::ggplot() +
      ggplot2::geom_text(data = data_,
                         ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                         size = text_size, color = text_color, fontface = "bold") +
      ggplot2::xlim(0, 1) +
      ggplot2::ylim(0, 1) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(rep(margin_size, 4)))

  }

  # ------------------------ PLOT MATCH and ANTI-MATCH -------------------------

  p.match <- plot_name(data_ = df[df$type == "match", ])
  p.antimatch <- plot_name(data_ = df[df$type == "antimatch", ])


  # -------------------------------- ADD BOX -----------------------------------

  if(add_box){

    p.box <- plot_box(...)

    p.match <- p.box +
      patchwork::inset_element(p.match, left = 0.04, bottom = 0, right = 0.96, top = 1)

    p.antimatch <- p.box +
      patchwork::inset_element(p.antimatch, left = 0.04, bottom = 0, right = 0.96, top = 1)

  }

  p.output <- p.match / p.antimatch +
    patchwork::plot_layout(heights = ggplot2::unit(c(1, 1), c("null")))

  return(p.output)

}

