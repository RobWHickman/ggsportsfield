library(ggplot2)
xmin = 0
xmax = 100 * 4/9
ymin = 0
ymax = 100
thickness = 1.5
markings = "full"
palette = "classic"
layout = "landscape"
portion = "full"

colscale <- c("#538032", "#ffffff", "#000000")
names(colscale) = c("grass", "lines", "features")

length = ymax - ymin
width = xmax - xmin

#end zones
endzones_y <- 100/12 * c(-1, 1) + c(ymax, ymin)

#field lines
lines_y <- seq(0, 100, by = 100/24)

p <- ggplot() +
  geom_rect(aes(xmin=xmin-0.05*xmax,xmax=xmax+0.05*xmax,ymin=ymin-0.05*xmax,ymax=ymax+0.05*xmax),
            fill = colscale["grass"]) +
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill = colscale["grass"],
            colour = colscale["lines"], size = thickness) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = c(ymax, ymin), ymax = endzones_y), fill = colscale["features"],
            colour = colscale["lines"], size = thickness) +
  geom_segment(aes(y = lines_y[3:23], yend = lines_y[3:23], x = xmin, xend = xmax),
               colour = colscale["lines"], size = thickness - 0.5) +
  geom_text(aes(label = as.character(c(seq(10,50,10), seq(40,10,-10))), x = width/10, y = lines_y[seq(5, 21, 2)]),
            colour = colscale["lines"], size = 10) +
  geom_text(aes(label = as.character(c(seq(10,50,10), seq(40,10,-10))), x = width - (width/10), y = lines_y[seq(5, 21, 2)]),
            colour = colscale["lines"], size = 10, angle = 180) +
  coord_flip()
