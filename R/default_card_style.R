#' Default Styling for Quartet Cards
#' @export
default_card_style <- function() {
  list(
    accent_color = "#235347",
    trait_color = c(
      Hipster    = "#D7263D",
      Nerd       = "#1B9AAA",
      Dreamer    = "#F4D35E",
      Optimist   = "#6A4C93",
      Adventurer = "#3F784C"
    ),
    name_size = 15,
    name_margin = 10,
    add_box = TRUE,
    box_linewidth_cm = 0.01
  )
}
