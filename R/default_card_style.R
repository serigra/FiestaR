#' Default Styling for Quartet Cards
#' @export
default_card_style <- function() {
  list(
    accent_color = "#235347",
    trait_bar_color = rep(c("#7B9790"), 5),
    trait_text_size = 15,
    trait_point_color = "#4E756B",
    trait_value_color = "white",
    name_size = 13,
    name_font = "quicksand",
    match_size = 9,
    add_box = TRUE,
    box_background = "#f8f8f6",
    box_linewidth = 0.7,
    box_radius_elevation = 0.15,
    box_radius_traits = 0.06,
    box_radius_names = 0.25
  )
}
