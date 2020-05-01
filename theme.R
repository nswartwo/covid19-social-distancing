cspec_theme_shiny <- function() {
  cspec_pal<-c("#1c133c", "#0089b8", "#be1622", "#ffffff")
  
  theme(
    legend.title = element_text(family = "sans", colour = cspec_pal[2], size = 10),
    legend.background = element_rect(fill = cspec_pal[4]),
    legend.key = element_rect(fill = cspec_pal[4], colour = cspec_pal[4]),
    legend.text = element_text(family = "sans", colour = cspec_pal[2], size = 10),
    plot.background = element_rect(fill = cspec_pal[4], colour = cspec_pal[4]),
    panel.background = element_rect(fill = cspec_pal[4]),
    axis.text = element_text(colour = cspec_pal[2], family = "sans"),
    plot.title = element_text(colour = cspec_pal[3], face = "bold", size = 18, vjust = 1, family = "sans"),
    axis.title = element_text(colour = cspec_pal[3], face = "bold", size = 13, family = "sans"),
    panel.grid.major.y = element_line(colour = cspec_pal[1]),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = cspec_pal[1]),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "sans", colour = cspec_pal[2]),
    strip.background = element_rect(fill = cspec_pal[2]),
    axis.ticks = element_line(colour = cspec_pal[4])
  )
}