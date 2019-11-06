library(tidyverse)
library(plotly)
library(viridis)
library(highcharter)

commute <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv") 
commute$state <- gsub("Ca$", "California", commute$state)
commute$state <- gsub("Massachusett$", "Massachusetts", commute$state)


compiled <- commute %>% 
  group_by(state,mode,city_size) %>% 
  summarise(n=sum(n),percent=mean(percent))

final <- compiled %>% 
  pivot_wider(names_from=mode,values_from=c(`n`,percent)) %>% 
  ungroup() %>% 
  mutate(total=percent_Walk+percent_Bike)

final$percent_Bike <- as.numeric(format(round(final$percent_Bike, 2), nsmall = 2))
final$percent_Walk <- as.numeric(format(round(final$percent_Walk, 2), nsmall = 2))
final$total <- as.numeric(format(round(final$total, 2), nsmall = 2))



fntltp <- JS("function(){
             return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
             Highcharts.numberFormat(this.point.value, 2);
             }")

chart_total <- hchart(final, "heatmap", hcaes(x = city_size, y = state, value = total)) %>% 
  hc_colorAxis(stops = color_stops(40, rev(inferno(30)))) %>% 
  hc_yAxis(reversed = T, offset = 0, tickLength = 0,
           gridLineWidth = 0, minorGridLineWidth = 0,
           labels = list(style = list(fontSize = "10px"))) %>% 
  hc_tooltip(formatter = fntltp) %>% 
  hc_title(text = "Percent of population using eco-friendly methods to travel to work") %>% 
  hc_legend(layout = "vertical", verticalAlign = "top",
            align = "right", valueDecimals = 0) %>% 
  hc_size(height = 800)
chart_total






