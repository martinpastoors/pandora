
# Taken from http://www.hafro.is/~einarhj/education/tcrenv2017/b_highresolutiongridding.html
# 3/9/2017

encode_zchords <- function(x, y, dx = 1, dy = 0.5 * dx, invalids = TRUE) {
  
  x.brks <- seq(floor(min(x)), max(ceiling(max(x)), ceiling(min(x)+dx)), dx)
  # x.brks <- seq(floor(min(x)),ceiling(max(x)),dx)
  
  x.ints <- findInterval(x, x.brks, all.inside = TRUE)
  x <- (x.brks[x.ints] + x.brks[x.ints + 1]) / 2
  
  y.brks <- seq(floor(min(y)), max(ceiling(max(y)), ceiling(min(y)+dy)), dy)
  # y.brks <- seq(floor(min(y)),ceiling(max(y)),dy)
  y.ints <- findInterval(y, y.brks, all.inside = TRUE)
  y <- (y.brks[y.ints] + y.brks[y.ints + 1]) / 2
  
  if(invalids) {
    x <- ifelse(x >= -180 & x <= 180, x, NA)
    y <- ifelse(y >= -90  & y <= 90 , y, NA)
  }
  
  return(paste(round(x,6), round(y,6), sep = ":"))
  
}

# df <- 
#   data_frame(lon =    rnorm(n = 1e6, mean =  -28, sd =   6),
#                   lat =    rnorm(n = 1e6, mean =   64, sd =   0.3),
#                   effort = rnorm(n = 1e6, mean = 1000, sd = 200))  %>% 
#   mutate(sq = encode_zchords(lon, lat, dx = 0.05, dy = 0.025)) %>% 
#   group_by(sq) %>% 
#   summarise(effort = sum(effort) / 60/1000) %>% # scale to lets say thousand hours
#   separate(sq, c("lon", "lat"), sep = ":", convert = TRUE, remove = FALSE)
