theme_sportsfield <- function(ratio = 1) {
  theme <- theme_void() +
    theme(plot.margin = margin(0, 0 ,0 ,0))
  
  if(ratio %in% c("soccer", "football", "baseball", "basketball", "cricket", "rugby")) {
    aspect_ratio = switch(ratio,
                          "soccer" = 68/105,
                          "football" = 1,
                          "baseball" = 1,
                          "basketball" = 1,
                          "cricket" = 1,
                          "rugby" = 1)
    theme <- theme + theme(aspect.ratio = aspect_ratio)
  } else if(is.numeric(ratio) & length(ratio) == 1) {
    aspect_ratio = ratio
    theme <- theme + theme(aspect.ratio = aspect_ratio)
  } 
}