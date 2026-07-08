#' Create a Box with Rounded Corners
#'
#' This function creates a simple box visualization using ggplot2 with a
#' rounded rectangle shape and customizable colors for the box and background.
#'
#' @param color_box Character string specifying the color of the box.
#'   Default is "darkgrey".
#' @param background_box Character string specifying the background color
#'   of the box. Default is "#f8f8f6" (light grey).
#'
#' @return A ggplot object containing a box plot with rounded corners.
#'
#' @examples
#' # Create a box with default colors
#' plot_box()
#'
#' # Create a box with custom colors
#' plot_box(color_box = "steelblue", background_box = "white")
#'
#' @importFrom ggplot2 ggplot aes geom_polygon theme_void theme margin
#' @importFrom ggforce geom_shape
#'
#' @export
plot_box <- function(box_color = "darkgrey",
                     box_background = "#f8f8f6",
                     box_linewidth_cm = 0.1){

  box_shape <- data.frame(
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1)
  )

  plot_box <- ggplot2::ggplot(box_shape, ggplot2::aes(x = x, y = y)) +
    ggforce::geom_shape(expand = ggplot2::unit(box_linewidth_cm, 'cm'),
                        radius = ggplot2::unit(box_linewidth_cm * 10, 'cm'),
                        fill = box_color) +
    ggforce::geom_shape(expand = ggplot2::unit(0, 'cm'),
                        radius = ggplot2::unit(box_linewidth_cm * 9.0, 'cm'),
                        fill = box_background) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0.02, 0.02, 0.02, 0.02, 'cm'))

  return(plot_box)

}
