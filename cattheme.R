#Cattheme
cattheme <- theme(
  panel.background = element_rect(fill = "#2B3E50", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "#2B3E50"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "#2B3E50"),
  plot.background = element_rect(fill = "#4E5D6C"),
  text = element_text(colour = 'white'),
  axis.text = element_text(colour = 'white'),
  axis.ticks = element_line(colour = 'white')
  )

# p <- ggplot(cars, aes(x=speed,y=dist)) + geom_point(colour='white') 
# p+cattheme
