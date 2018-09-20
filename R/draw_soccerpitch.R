#' Ggplot2 Soccer Field Markings
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

draw_soccerpitch <- function(xmin = 0,
                             xmax = 100 * 2/3,
                             ymin = 0,
                             ymax = 100,
                             thickness = 1.5,
                             markings = "full",
                             palette = "classic",
                             layout = "landscape",
                             portion = "full") {
  
  #set the colour scale based on the palette supplied
  colscale = switch(palette,
                    "classic" = c("#538032", "#ffffff", "#000000"),
                    "bw" = c("#fbf7f5", "#a8a8a8", "#7e7e7e"),
                    "blues" = c("#00004c", "#ffffff", "#ff0000"))
  names(colscale) = c("grass", "lines", "features")
  
  length = ymax - ymin
  width = xmax - xmin
  
  #halfway features
  halfway_line_y = ymin + length / 2
  centre_x = xmin + width / 2
  centrecircle_x = (cos(seq(0,2*pi,length.out = 500)) * width/8) + centre_x
  centrecircle_y = (sin(seq(0,2*pi,length.out = 500)) * width/8) + halfway_line_y

  #goals
  goal_x = xmin + (width / 2 + (c(-1, 1) * (width / 10) / 2))
  goal_y = ((c(-1, 1) / 100) * ymax) + c(ymin, ymax)
  
  #penalty box
  p_box_top_y = ((length * (3/20)) * c(-1, 1)) + c(ymax, ymin)
  p_box_sides_y = rep(p_box_top_y, 2)
  p_box_top_x = ((abs(c(xmin, xmax) - goal_x) / 2) * c(-1, 1)) + goal_x
  p_box_sides_x = rep(p_box_top_x, each = 2)
  
  pen_spot_x = xmin + width / 2
  pen_spot_y = (c(-1, 1) * length / 10) + c(ymax, ymin)

  p_circle_x = (cos(seq(0,2*pi,length.out = 500)) * width/8) + pen_spot_x
  p_circle_y1 = (sin(seq(0,2*pi,length.out = 500)) * width/8) + pen_spot_y[1]
  p_circle_y2 = (sin(seq(0,2*pi,length.out = 500)) * width/8) + pen_spot_y[2]
  
  #6 yard box
  six_box_top_y = ((length / 20) * c(-1, 1)) + c(ymax, ymin)
  six_box_sides_y = rep(six_box_top_y, 2)
  six_box_top_x = goal_x - (goal_x - p_box_top_x) / 3
  six_box_sides_x = rep(six_box_top_x, each = 2)
  
  soccer_markings <- list(
      #pitch
      geom_rect(aes(xmin=xmin-0.05*xmax,xmax=xmax+0.05*xmax,ymin=ymin-0.05*xmax,ymax=ymax+0.05*xmax),
                fill = colscale["grass"]),
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill = colscale["grass"],
                colour = colscale["lines"], size = thickness)
      )

  if(markings != "plain") {
    soccer_markings <- append(soccer_markings, list(
      #halfway
      geom_path(aes(x = centrecircle_x, y = centrecircle_y),
                colour = colscale["lines"], size = thickness),
      geom_segment(aes(y = halfway_line_y, yend = halfway_line_y, x = xmin, xend = xmax),
                   colour = colscale["lines"], size = thickness),
      #penalty box
      geom_segment(aes(y = rep(c(ymax, ymin), 2), yend = p_box_sides_y, x = p_box_sides_x, xend = p_box_sides_x),
                   colour = colscale["lines"], size = thickness),
      geom_segment(aes(y = p_box_top_y, yend = p_box_top_y, x = p_box_top_x[1], xend = p_box_top_x[2]),
                   colour = colscale["lines"], size = thickness),
      #goals
      geom_segment(aes(y = goal_y, yend = goal_y, x = goal_x[1], xend = goal_x[2]),
                   colour = colscale["features"], size = thickness + 0.5)
    ))}

  if(markings == "full") {
    soccer_markings <- append(soccer_markings, list(
      #halfway line
      geom_point(aes(y = halfway_line_y, x = centre_x),
                 colour = colscale["lines"], size = thickness + 1),
      #penalty area
      geom_point(aes(x = pen_spot_x, y = pen_spot_y),
                 colour = colscale["lines"], size = thickness + 1),
      geom_path(aes(x = p_circle_x[which(p_circle_y1 <= p_box_top_y[1])], y = p_circle_y1[which(p_circle_y1 <= p_box_top_y[1])]),
                colour = colscale["lines"], size = thickness),
      geom_path(aes(x = p_circle_x[which(p_circle_y2 >= p_box_top_y[2])], y = p_circle_y2[which(p_circle_y2 >= p_box_top_y[2])]),
                colour = colscale["lines"], size = thickness),
      #6 yard box
      geom_segment(aes(y = rep(c(ymax, ymin), 2), yend = six_box_sides_y, x = six_box_sides_x, xend = six_box_sides_x),
                   colour = colscale["lines"], size = thickness),
      geom_segment(aes(y = six_box_top_y, yend = six_box_top_y, x = six_box_top_x[1], xend = six_box_top_x[2]),
                   colour = colscale["lines"], size = thickness)
    ))}

  if(layout == "landscape") {
    soccer_markings <- append(soccer_markings, coord_flip())
  }

  return(soccer_markings)
}


