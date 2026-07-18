#' Default Styling for Quartet Cards
#' @export
default_card_style <- function() {
  list(
    accent_color = "#235347",
    trait_color = rep(c("#235347"), 5),
    # trait_color = c(
    #   Hipster    = "#D7263D",
    #   Nerd       = "#1B9AAA",
    #   Dreamer    = "#F4D35E",
    #   Optimist   = "#6A4C93",
    #   Adventurer = "#3F784C"
    # ),
    name_size = 12,
    add_box = TRUE,
    box_background = "#f8f8f6",
    box_linewidth = 0.7,
    box_radius_names = 0.25,
    box_radius_traits = 0.1
  )
}
