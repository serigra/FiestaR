
plot_match_antimatch <- function(name_match = NULL,
                                 name_antimatch = NULL,
                                 add_box = TRUE,
                                 text_color = "#235347",
                                 text_size = 15,
                                 margin = 10,
                                 ...) {
  
  # ---------------------------- PREPARE DATA ----------------------------------
  
  df <- data.frame(
    x = 0.5, y = 0.5,
    label = c( paste0("😀 ",  name_match),
               paste0("🚀 ", name_antimatch)
               ),
    type = c("match", "antimatch")
  )
  
  # -------------------------- PLOT Match and Anti-Match -----------------------
  
  plot_match <- 
    ggplot() +
    geom_text(data = df |> filter(type == 'match'), aes(x = x, y = y, label = label),
              size = text_size, color = text_color, fontface = "bold") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(rep(margin, 4)))
  
  plot_antimatch <- 
    ggplot() +
    geom_text(data = df |> filter(type == 'antimatch'), aes(x = x, y = y, label = label),
              size = text_size, color = text_color, fontface = "bold") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(rep(margin, 4)))
  
  
  # -------------------------------- ADD BOX -----------------------------------
  
  if(add_box){
    
    plot_box <- box_plot(...)
    
    plot_match <- plot_box + 
      patchwork::inset_element(plot_match, left = 0.04, bottom = 0, right = 0.96, top = 1)
    
    plot_antimatch <- plot_box + 
      patchwork::inset_element(plot_antimatch, left = 0.04, bottom = 0, right = 0.96, top = 1)
    
  }
  
  plot_output <- plot_match / plot_antimatch +
    patchwork::plot_layout(heights = unit(c(1, 1), c("null"))) 
  
  return(plot_output)
  
}

