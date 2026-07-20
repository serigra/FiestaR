#' Compose a Full Quartet Card for One Guest
#'
#' @description Returns a quartet-style card  plot for a guest containing an elevation profile,
#' gradient-colored horizontal sliders representing trait values, as well as the
#' names of the match and antimatch.
#'
#' @param data A one-row data frame for a single guest, containing `name`,
#'   the five trait columns, `data_elevation` (list-column), `match`, and
#'   `antimatch`.
#' @param trait_mean Optional named numeric vector of length 5 specifying the mean values for each trait.
#' @param trait_cols Character vector naming the trait columns in `data`.
#' @param style A list of shared styling parameters (see `default_card_style()`).
#' @param save_file Boolean indicating whether to save the created card as png. Default is FALSE.
#'
#' @return A patchwork object: the complete stacked card.
#'
#' @export
plot_card <- function(data,
                      trait_cols = c("Hipster", "Nerd", "Dreamer", "Optimist", "Adventurer"),
                      trait_mean = NULL,
                      style = default_card_style(),
                      save_file = FALSE) {

  # ----------------------------- CHECK ARGUMENTS ------------------------------

  stopifnot(nrow(data) == 1)
  req_columns <- c("name", "data_elevation", trait_cols, "match", "antimatch")

  if(!base::all(req_columns %in% names(data))) {
    stop("Data must contain the following columns: ", paste0(req_columns, collapse = ', '))
  }

  # ------------------------------- PlOT NAME ----------------------------------

  p.name <- plot_name(name = data$name,
                      name_color = "white",
                      name_size = style$name_size,
                      name_font = style$name_font,
                      add_box = FALSE,
                      box_color = style$accent_color,
                      box_background = style$box_background,
                      box_linewidth = style$box_linewidth,
                      box_radius = style$box_radius_names)

  # ------------------------ PlOT ELEVATION PROFILE ----------------------------

  p.elev <- plot_elevation(
    data = data$data_elevation[[1]],
    color_profile  = style$accent_color,
    add_text = TRUE,
    add_box  = style$add_box,
    box_color = style$accent_color,
    box_linewidth = style$box_linewidth,
    box_radius = style$box_radius_elevation,
    elevation_text_size = 2.5
  )

  p.traits <- plot_trait(
    data = data[, c("name", trait_cols)],
    trait_mean = trait_mean,
    trait_bar_color = style$trait_bar_color,
    trait_text_font = style$name_font,
    trait_text_color = style$accent_color,
    trait_text_size = style$trait_text_size,
    trait_value_color = style$trait_value_color,
    trait_point_color = style$trait_point_color,
    add_box = style$add_box,
    box_color = style$accent_color,
    box_linewidth = style$box_linewidth,
    box_radius = style$box_radius_traits
  )

  # ---------------------------- PLOT MATCH & ANTI-MATCH -----------------------

  heart_path <- "inst/images/heart_green.png"
  thumb_down_path <- "inst/images/thumb-down-green.png"

  p.match <- plot_name(name = paste0(" <img src='", heart_path, "' width='20'/>  ", data$match),
                       name_color = style$accent_color,
                       name_size = style$match_size,
                       name_font = style$name_font,
                       add_box = style$add_box,
                       box_color = style$accent_color,
                       box_background = style$box_background,
                       box_linewidth = style$box_linewidth,
                       box_radius = style$box_radius_names)

  p.antimatch <- plot_name(name = paste0(" <img src='", thumb_down_path, "' width='20'/>   ", data$antimatch),
                           name_color = style$accent_color,
                           name_size = style$match_size,
                           name_font = style$name_font,
                           add_box = style$add_box,
                           box_color = style$accent_color,
                           box_background = style$box_background,
                           box_linewidth = style$box_linewidth,
                           box_radius = style$box_radius_names)

  p.match.antimatch <- p.match / p.antimatch +
    patchwork::plot_layout(heights = ggplot2::unit(c(1, 1), c("null")))


  # -------------------- FINAL PLOT with green background box ------------------

  p.card <- p.name / p.elev / p.traits / p.match.antimatch +
    patchwork::plot_layout(heights = ggplot2::unit(c(1.5, 2, 5.25, 2.25), "null"))

  p.outbox <- plot_box(box_color = style$accent_color,
                       box_background = style$accent_color,
                       box_radius = 0.05)

  p.out <- p.outbox +
    patchwork::inset_element(p.card, left = 0.02, bottom = 0.05, right = 0.98, top = 0.95)

  # -------------------- FINAL PLOT with green background box ------------------

  if(save_file){

    showtext::showtext_opts(dpi = 450) # for plotting when saved as png/jpg

    ggplot2::ggsave(
      filename = paste0("cards/card_", data$name, ".jpg"),
      plot = p.out,
      dpi = 450,
      # width = 5.6, height = 9.6,
      width = 70, height = 120, unit = "mm", scale = 2.0,
      bg = "transparent"
    )
  }

  showtext::showtext_opts(dpi = 96) # default, for plotting in RStudio Viewer
  p.out

}
