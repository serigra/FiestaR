#' Create a Box with Rounded Corners
#'
#' This function creates a simple box visualization using ggplot2 with a
#' rounded rectangle shape and customizable colors for the box and background.
#'
#' @param box_color Character string specifying the color of the box.
#'   Default is "darkgrey".
#' @param box_background Character string specifying the background color
#'   of the box. Default is "#f8f8f6" (light grey).
#' @param box_linewidth_cm Linewidth of box in cm.
#'
#' @return A ggplot object containing a box plot with rounded corners.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' plot_box()
#' }
#'
#' # Create a box with custom colors
#' \dontrun{
#' plot_box(box_color = "steelblue",
#'          box_background = "white",
#'          box_linewidth_cm = 0.01)
#' }
#'
plot_box <- function(box_color = "darkgrey",
                     box_background = "#f8f8f6",
                     box_linewidth_cm = 0.1){

  box_shape <- data.frame(
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1)
  )

  plot_box <- ggplot2::ggplot(box_shape, ggplot2::aes(x = .data$x, y = .data$y)) +
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
