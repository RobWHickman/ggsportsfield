#' Ggplot2 Basketball Court Markings
#' @param xmin The origin of x for the markings to begin. X is 'across' i.e. along the halfway line
#' @param xmax The maximum value of x for the markings to end
#' @param ymin The origin of y for the markings to begin. Y is 'along' i.e. up the touchline
#' @param ymax The maximum value of y for the markings to end
#' @param thickness The thickness of the markings
#' @param markings Which markings to plot from the most basic rectangle to full pitch markings
#' @param palette The palette to use to colour the field/markings.
#'   Standard arguments are classic (the 'natural' colours), bw (a greyscale plot), or blues (darkblue),
#'   but custom palettes can be supplied
#' @param layout The orientation of the final plot. Defaults to landscape (so coord_flip() s the plot)
#' @param portion The portion of the field to plot. 
#'   Standard arguments are the whole pitch (full) or half-pitch (half) but custom coordinates can be supplied.
#' @examples
#' @author Robert Hickman
#' @export

#https://upload.wikimedia.org/wikipedia/commons/6/6c/Basketball_courts.svg
draw_bballcourt <- function(xmin = 0,
                            xmax = 50,
                            ymin = 0,
                            ymax = 94,
                            thickness = 1.5,
                            markings = "full",
                            palette = "classic",
                            layout = "landscape",
                            portion = "full") {
  
  colscale = switch(palette,
                    "classic" = c("#cc8400", "#ffffff", "#000000", "#4169e1"),
                    "bw" = c("#fbf7f5", "#a8a8a8", "#7e7e7e", "#7e7e7e"),
                    "blues" = c("#00004c", "#ffffff", "#ffffff", "#ff0000"))
  names(colscale) = c("floor", "lines", "features", "paint")
  
  
  length = ymax - ymin
  width = xmax - xmin
  
  #halfway features
  halfway_line_y = ymin + length / 2
  centre_x = xmin + width / 2
  centrecircle_x = (cos(seq(0,2*pi,length.out = 500)) * width/(25/3)) + centre_x
  centrecircle_y = (sin(seq(0,2*pi,length.out = 500)) * width/(25/3)) + halfway_line_y
  restrain_circle_x = (cos(seq(0,2*pi,length.out = 500)) * width/25) + centre_x
  restrain_circle_y = (sin(seq(0,2*pi,length.out = 500)) * width/25) + halfway_line_y
  
  #hoops
  backboard_x = xmin + (width / 2 + (c(-1, 1) * (width / (50/6)) / 2))
  backboard_y = ymin + ((length / 23.5) * c(-1, 1)) + c(ymax, ymin)
  hoop_x = (cos(seq(0,2*pi,length.out = 500)) * ((backboard_x[2] - backboard_x[1]) / 5)) + mean(backboard_x)
  hoop_y1 = (sin(seq(0,2*pi,length.out = 500)) * ((backboard_x[2] - backboard_x[1]) / 5)) + (length * 1.6 / (94 / 3.2808))
  hoop_y2 = (sin(seq(0,2*pi,length.out = 500)) * ((backboard_x[2] - backboard_x[1]) / 5))  + ymax - (length * 1.6 / (94 / 3.2808))
  
  #three point line
  three_pt_sides_y = ((length * (14/94)) * c(-1, 1)) + c(ymax, ymin)
  three_pt_sides_x = ((width * (3/50)) * c(-1, 1)) + c(xmax, xmin)
  three_pt_circle_y2 = (sin(seq(0,2*pi,length.out = 500)) * (width * 7.24 / (50 / 3.2808))) + mean(hoop_y2)
  three_pt_circle_y1 = (sin(seq(0,2*pi,length.out = 500)) * (width * 7.24 / (50 / 3.2808))) + mean(hoop_y1)
  three_pt_circle_x = (cos(seq(0,2*pi,length.out = 500)) * (width * 7.24 / (50 / 3.2808))) + mean(hoop_x)
  
  #paint
  paint_rect_y = (ymax * (19/94) * c(-1, 1)) + c(ymax, ymin)
  paint_rect1_x = (xmax * (3/25) * c(-1, 1)) + xmax/2
  paint_rect2_x = (xmax * (4/25) * c(-1, 1)) + xmax/2
  
  paint_loop_y2 = sin(seq(0,2*pi,length.out = 500)) * (width * 2/25) + mean(hoop_y2)
  paint_loop_y1 = sin(seq(0,2*pi,length.out = 500)) * (width * 2/25) + mean(hoop_y1)
  paint_loop_x = cos(seq(0,2*pi,length.out = 500)) * (width * 2/25) + mean(hoop_x)
  
  paint_circle_y1 = sin(seq(0,2*pi,length.out = 500)) * (width * 3/25) + paint_rect_y[1]
  paint_circle_y2 = sin(seq(0,2*pi,length.out = 500)) * (width * 3/25) + paint_rect_y[2]
  paint_circle_x = cos(seq(0,2*pi,length.out = 500)) * (width * 3/25) + mean(hoop_x)
  
  #paint markings
  key_line_y = c(min(paint_circle_y2), max(paint_circle_y1))
  paint_markings_x = paint_rect2_x + c(-width/50, width/50)
  paint_markings_y = (c(7/94, 8/94, 11/94, 14/94) * length) * rep(c(-1, 1), each = 4) + rep(c(ymax, ymin), each = 4)
  inbound_x = (xmax * (11/50) * c(-1, 1)) + xmax/2
  inbound_y = width/50 * c(-1, 1) + c(ymax, ymin)
  
  
  
  bball_markings <- list(
    #outside
    geom_rect(aes(xmin=xmin-0.05*xmax,xmax=xmax+0.05*xmax,ymin=ymin-0.05*xmax,ymax=ymax+0.05*xmax),
              fill = colscale["floor"]),
    geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill = colscale["floor"],
              colour = colscale["lines"], size = thickness)
  )
  
  if(markings == "major" | markings == "full") {
    bball_markings <- append(bball_markings, list(
      #halfway markings
      geom_path(aes(x = centrecircle_x, y = centrecircle_y),
                colour = colscale["lines"], size = thickness),
      geom_segment(aes(y = halfway_line_y, yend = halfway_line_y, x = xmin, xend = xmax),
                   colour = colscale["lines"], size = thickness),
      #three point markings
      geom_segment(aes(y = rep(c(ymax, ymin), 2), yend = rep(three_pt_sides_y, 2), x = rep(three_pt_sides_x, each = 2), xend = rep(three_pt_sides_x, each = 2)),
                   colour = colscale["lines"], size = thickness),
      geom_path(aes(x = three_pt_circle_x[which(three_pt_circle_y2 <= three_pt_sides_y[1])], y = three_pt_circle_y2[which(three_pt_circle_y2 <= three_pt_sides_y[1])]),
                colour = colscale["lines"], size = thickness),
      geom_path(aes(x = three_pt_circle_x[which(three_pt_circle_y1 >= three_pt_sides_y[2])], y = three_pt_circle_y1[which(three_pt_circle_y1 >= three_pt_sides_y[2])]),
                colour = colscale["lines"], size = thickness),
      #paint markings
      geom_rect(aes(xmin=paint_rect1_x[1],xmax=paint_rect1_x[2],ymin=c(ymax,ymin),ymax=c(paint_rect_y[1],paint_rect_y[2])), fill = colscale["paint"],
                colour = colscale["lines"], size = thickness),
      geom_rect(aes(xmin=paint_rect2_x[1],xmax=paint_rect2_x[2],ymin=c(ymax,ymin),ymax=c(paint_rect_y[1],paint_rect_y[2])), fill = NA,
                colour = colscale["lines"], size = thickness),
      geom_path(aes(x = paint_circle_x[which(paint_circle_y1 < paint_rect_y[1])], y = paint_circle_y1[which(paint_circle_y1 < paint_rect_y[1])]),
                colour = colscale["lines"], size = thickness),
      geom_path(aes(x = paint_circle_x[which(paint_circle_y1 > paint_rect_y[1])], y = paint_circle_y1[which(paint_circle_y1 > paint_rect_y[1])]),
                colour = colscale["lines"], size = thickness, linetype = 2),
      geom_path(aes(x = paint_circle_x[which(paint_circle_y2 > paint_rect_y[2])], y = paint_circle_y2[which(paint_circle_y2 > paint_rect_y[2])]),
                colour = colscale["lines"], size = thickness),
      geom_path(aes(x = paint_circle_x[which(paint_circle_y2 < paint_rect_y[2])], y = paint_circle_y2[which(paint_circle_y2 < paint_rect_y[2])]),
                colour = colscale["lines"], size = thickness, linetype = 2)
    ))}
  
  if(markings == "full") {
    bball_markings <- append(bball_markings, list(
      #halfway markings
      geom_path(aes(x = restrain_circle_x, y = restrain_circle_y),
                colour = colscale["lines"], size = thickness),
      #paint markings
      geom_path(aes(x = paint_loop_x[which(paint_loop_y2 < mean(hoop_y2))], y = paint_loop_y2[which(paint_loop_y2 < mean(hoop_y2))]),
                colour = colscale["lines"], size = thickness),
      geom_path(aes(x = paint_loop_x[which(paint_loop_y1 > mean(hoop_y1))], y = paint_loop_y1[which(paint_loop_y1 > mean(hoop_y1))]),
                colour = colscale["lines"], size = thickness),
      #court markings
      geom_segment(aes(y = key_line_y, yend = key_line_y, x = rep(paint_rect1_x[1], each = 2), xend = rep(paint_rect1_x[2], each = 2)),
                   colour = colscale["lines"], size = thickness, linetype = 2),
      geom_segment(aes(y = rep(paint_markings_y, 2), yend = rep(paint_markings_y, 2), x = rep(paint_markings_x, each = 8), xend = rep(paint_rect2_x, each = 8)),
                   colour = colscale["lines"], size = thickness),
      geom_segment(aes(x = rep(c(xmax, xmin),2), xend = rep(three_pt_sides_x,2), y = rep(c(max(three_pt_circle_y1), min(three_pt_circle_y2)), each = 2), yend = rep(c(max(three_pt_circle_y1), min(three_pt_circle_y2)), each = 2)),
                   colour = colscale["lines"], size = thickness),
      geom_segment(aes(x = rep(inbound_x, 2), xend = rep(inbound_x, 2), y = rep(inbound_y, each = 2), yend = rep(c(ymax, ymin), each = 2)),
                   colour = colscale["lines"], size = thickness)
    ))}
  
  bball_markings <- append(bball_markings, list(
    #hoops
    geom_segment(aes(y = backboard_y, yend = backboard_y, x = backboard_x[1], xend = backboard_x[2]),
                 colour = colscale["features"], size = thickness),
    geom_path(aes(x = hoop_x, y = hoop_y1),
              colour = colscale["features"], size = thickness - 0.5),
    geom_path(aes(x = hoop_x, y = hoop_y2),
              colour = colscale["features"], size = thickness - 0.5) 
  ))
  
  if(layout == "landscape") {
    bball_markings <- append(bball_markings, coord_flip())
  }
  
  return(bball_markings)
}



