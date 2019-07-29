heat_theme <- function() {
  grey_shade <- "grey92"
  
  theme(
    # all text
    text = element_text(
      color = "grey7"  
    ),
    
    # axes
    axis.title.y = element_text(
      vjust = 1.5,
      margin = margin(r = 0.4, unit = "cm")
    ),
    axis.title.x = element_text(
      margin = margin(t = 0.4, unit = "cm")
    ),
    axis.text.x = element_text(
      size = 10,
      margin = margin(t = 0.2, unit = "cm")
    ),
    axis.text.y = element_text(
      size = 10,
      margin = margin(r = 0.2, unit = "cm")
    ),
    axis.ticks = element_line(color = grey_shade),
    axis.ticks.length = unit(0.2, "cm"),
    
    # legends
    legend.spacing = unit(0.2, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.key.size = unit(0.9, "lines"),
    
    # panels
    panel.grid.major = element_line(
      color = grey_shade
    ),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.75, "cm"),
    panel.background = element_rect(
      fill = "white",
      color = grey_shade,
      size = 1
    ),
    
    # plot (general)
    plot.title = element_text(
      size = 15, 
      face = "bold", 
      hjust = 0.5,
      margin = margin(10, 0, 10, 0)
    ),
    
    # facet labels
    strip.background = element_rect(
      fill = "white"
    ),
    strip.text = element_text(
      angle = 0, 
      size = 10
    ),
    strip.text.x = element_text(
      hjust = 0.5,
      vjust = 0.65,
      margin = margin(10, 0, 10, 0)
    ),
    strip.text.y = element_text(
      hjust = 0.5, 
      vjust = 0.5, 
      angle = 0
    )
  )
}
