load("output_files/Data_set_1/Waterfall.RData")
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
# https://blog.exploratory.io/making-maps-for-uk-countries-and-local-authorities-areas-in-r-b7d222939597
library(ggmap)
library(rgdal)
library(spdplyr)
library(sp)
# 
API_KEY = "AIzaSyCzk2FoBaNzkPoGUcRzfzbVN0YOqq2S_aA"
# 

names(labelled.df.geo)[names(labelled.df.geo) == "lad16nm_clean"] <- "District"

labelled.df.geo <- subset(labelled.df.geo, source != "News")

labelled.df.geo <- labelled.df.geo[,c("original_content",
                                      fourteen.allergen.names,
                                      other.allergen.names,
                                      "Month",
                                      "Week",
                                      "objectid",
                                      "food_labelling",
                                      "lat",
                                      "long",
                                      "District",
                                      "source",
                                      "sentiment_class")]

library(tidyr)
labelled.df.geo.long <- gather(labelled.df.geo, 
                               Allergen, 
                               "Mentions", 
                               c(fourteen.allergen.names,other.allergen.names), 
                               factor_key = TRUE
)
library(dplyr)
labelled.df.geo.summary <- labelled.df.geo.long %>%
  group_by(sentiment_class, Allergen, District, Week, source, long, lat) %>%
  summarise(count=sum(Mentions))

labelled.df.geo.summary <- subset(labelled.df.geo.summary, count > 0)
labelled.df.geo.summary$sentiment_class <- factor(labelled.df.geo.summary$sentiment_class, levels = c("negative", "neutral", "positive"))
labelled.df.geo.summary <- labelled.df.geo.summary[!is.na(labelled.df.geo.summary$lat),]

library(shiny)
library(leaflet)
library(RColorBrewer)

cluster.map <- leaflet(data = labelled.df.geo) %>%
  setView(lng = -3.69531, lat = 53.75844, zoom = 6) %>% 
  addTiles() %>%
  addMarkers(lng = ~long, lat = ~lat,
             clusterOptions = markerClusterOptions(),
             popup = ~as.character(original_content))
cluster.map


pal <- colorFactor(c("red", "grey60", "navy"), domain = c("negative", "neutral", "positive"))

radius.by.count.sentiment <- leaflet(data = labelled.df.geo.summary) %>%
  setView(lng = -3.69531, lat = 53.75844, zoom = 6) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircleMarkers(lng = ~long, lat = ~lat,
                   radius = ~count,
                   color = ~pal(sentiment_class),
                   stroke = FALSE, fillOpacity = 0.3,
                   popup = ~as.character(paste(Allergen, ": ", count)))
radius.by.count.sentiment

labelled.df.geo.summary.14 <- subset(labelled.df.geo.summary, Allergen %in% fourteen.allergen.names)
no.colours <- length(fourteen.allergen.names) 

fal <- colorFactor(palette = "Set1", domain = fourteen.allergen.names)

radius.by.count.14 <- leaflet(data = labelled.df.geo.summary.14) %>%
  setView(lng = -3.69531, lat = 53.75844, zoom = 6) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircleMarkers(lng = ~long, lat = ~lat,
                   radius = ~count,
                   color = ~fal(Allergen),
                   stroke = FALSE, fillOpacity = 0.5,
                   popup = ~as.character(paste(Allergen, ": ", count)))
radius.by.count.14


dev.off()
dev.set(dev.next())
dev.set(dev.next())
dev.off()

## Static Plots
register_google(key = "AIzaSyB72eS6FLuG9IMoCtXKViGkougdlUT0HFE")

UK <- geocode("UK")
UKrefmap <- get_map(location = c(lon = UK$lon, lat = UK$lat),
                    maptype = "terrain", color="bw", zoom = 5 )

london <- get_map(location = c(lon = -0.129, lat = 51.51),
                  maptype = "terrain", color="bw", api_key = API_KEY, zoom = 10 )

fourteen.summary.geo <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  #geom_polygon(data = shape.df, aes(x = long, y=lat, group=group),
  #              fill="blue", alpha=0.2) +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group),
            color="gray50", size=0.3) +
  geom_point(data = labelled.df.geo.summary.14, aes(x = long, y = lat, colour = Allergen, size = count),
              alpha = 0.5)
fourteen.summary.geo
