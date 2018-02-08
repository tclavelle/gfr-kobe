library(dplyr)
library(googleVis)
library(ggvis)

# read kobe data list object
load("gfr-kobe/data/KobeAppData.rdata")

df <- data$lumped %>%
  filter(Year == 2012) %>%
  dplyr::select(BvBmsy, FvFmsy, IdOrig,Dbase, Country, SciName, MSY, g, k) %>%
  mutate(BvBmsy = round(BvBmsy, digits = 3),
         FvFmsy = round(FvFmsy, digits = 3),
         MSY    = round(MSY),
         k      = round(k),
         g      = round(g, digits = 4),
         id     = 1:nrow(df))

df$FvFmsy[df$FvFmsy > 4] <- 4
df$BvBmsy[df$BvBmsy > 2.5] <- 2.5

################################################
## ggviz Kobe

kobe_lines <- data_frame(xline1 = c(1,1),
                    yline1 = c(0,4),
                    xline2 = c(0,2.5),
                    yline2 = c(1,1))

rect_dat = data.frame(panel = c('bottom_left','top_right','bottom_right',
                                'top_left'),
                      x_min = c(0,1,1,0),x_max = c(1,2.5,2.5,1),
                      y_min = c(0,1,0,1), y_max = c(1,4,1,4),
                      fills = c("#ffec8b", "ffec8b",'#32cd32',"#FF4500"))


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- df[df$id == x$id, ]
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

df %>%
  ggvis() %>% 
  layer_rects(x = ~x_min, y = ~y_min, x2 = ~x_max, y2 = ~y_max, fill := ~fills, opacity := 0.4,  data = rect_dat) %>%
  layer_points(x = ~BvBmsy,
        y = ~FvFmsy,
        size = ~MSY,
        fill = ~Dbase,
        stroke = 'black',
        key := ~id,
        opacity := 0.4) %>%
  add_tooltip(all_values, 'hover') %>%
  # add_tooltip(html = function(data){
  #   paste('Country: ', data$Country, '<br>', 'B/Bmsy: ', data$BvBmsy, '<br>', 'F/Fmsy: ', data$FvFmsy, sep = '')},'hover') %>%
  layer_paths(x = ~xline1, y = ~yline1, data = kobe_lines) %>%
  layer_paths(x = ~xline2, y = ~yline2, data = kobe_lines)

  
  
# ################################################
# ## Google Charts Kobe
# kobe <- gvisBubbleChart(df,
#                          xvar = 'BvBmsy',
#                          yvar = 'FvFmsy',
#                          colorvar = 'Dbase',
#                          sizevar = 'MSY',
#                          options = list(
#                            hAxis = '{minValue:0, maxValue:2.5}',
#                            vAxis.maxValue = 4,
#                            width = '800px',
#                            height = '800px',
#                            bubble.opacity = 0.2)) %>%
#   layer_lines
# 
# plot(kobe)


