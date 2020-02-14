
primary <- 
  viridis::viridis(1, alpha = 1, begin = 0.35, end = 0.35, direction = 1)
secondary <- 
  viridis::viridis(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)

primary_light <- xaringanthemer::lighten_color(primary, 0.9)
secondary_light <- xaringanthemer::lighten_color(secondary, 0.5)

yel <- viridis::magma(1, alpha = 1, begin = 0.9, end = 0.9, direction = 1)
purp <-  viridis::magma(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)

# slide accent colors
black <- xaringanthemer::darken_color(primary, 0.3)
white <- primary_light

# graphics theme
theme_set(
  ggthemes::theme_base(base_family = "Source Sans Pro", base_size = 14) + 
  theme(
    legend.background = element_rect(fill = white),
    legend.key = element_rect(fill = white),
    strip.background = element_rect(fill = white),
    panel.background = element_rect(fill = white),
    plot.background = element_rect(fill = white, color = NA), 
    axis.ticks = element_line(lineend = "square"), 
    axis.ticks.length = unit(0.15, "lines")
  )
)
update_geom_defaults(
  "text", 
  list(family = "Source Sans Pro", size = 4, color = black)
)
