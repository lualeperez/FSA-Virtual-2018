library(ggmap)

register_google(key = "AIzaSyB72eS6FLuG9IMoCtXKViGkougdlUT0HFE")

UK <- geocode("UK")
lon_coor <- UK$lon
lat_coor <- UK$lat
UKrefmap_terrain_bw_zoom5_nolables <- get_googlemap(center  = c(lon = lon_coor, lat = lat_coor),
                                                    maptype = "terrain",
                                                    color   = "bw",
                                                    zoom    = 5,
                                                    scale   = 4,
                                                    style   = c(feature="all",element="labels",visibility="off"))
save.image(file = "resources/Static_maps.RData")
