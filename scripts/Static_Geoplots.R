### Script with static summary plots (i.e. map-based)

# load labelled data
load(image_analysis)

# output file where the plots are saved
out.dir <- file.path(plots_output_dir)

# Removal of News as a datasource from our data
labelled.df.geo <- subset(labelled.df.geo, source != "News")

# putting the data in a format for choropleth map plotting
library(dplyr)

# All contextaul issues put in a single data.frame grouped by local authority names, long and lat
labelled.df.geo.stream1_summary <- labelled.df.geo %>%
  group_by(objectid, long, lat, District, lad16nm) %>%
  summarise(count_allergy_enquiries     = sum(allergy_enquiries),
            count_neg_allergy_enquiries = sum(allergy_enquiries & sentiment_class == "negative"),
            count_pos_allergy_enquiries = sum(allergy_enquiries & sentiment_class == "positive"),
            count_food_labelling        = sum(food_labelling),
            count_neg_food_labelling    = sum(food_labelling & sentiment_class == "negative"),
            count_pos_food_labelling    = sum(food_labelling & sentiment_class == "positive"),
            count_mild_reaction         = sum(mild_reaction),
            count_severe_reaction       = sum(severe_reaction),
            count_adverse_reaction      = sum(severe_reaction | mild_reaction))

options(warn=-1) # to stop receiving warnings from coercion and from reseting x and y axis during plotting

# add the number of businesses info per local authority
labelled.df.geo.stream1_summary <- left_join(labelled.df.geo.stream1_summary,
                                             restaurants_per_local_authority.df[,c("District","TotalEstablishments")], "District")

# add the population info per local authority
labelled.df.geo.stream1_summary <- left_join(labelled.df.geo.stream1_summary,
                                             population_per_local_authority.df[,c("District","all_ages")], "District")

# Number of businesses plot
shape.df.nbusinesses <- left_join(shape.df,restaurants_per_local_authority.df[,c("District","TotalEstablishments")], by = "District")
# Population plot
shape.df.population  <- left_join(shape.df,population_per_local_authority.df[,c("District","all_ages")],             by = "District")

# Joing stream 1 issues with map polygons
names_to_join    <- c("District","TotalEstablishments","all_ages",
                      "count_allergy_enquiries",
                      "count_neg_allergy_enquiries",
                      "count_pos_allergy_enquiries",
                      "count_food_labelling",
                      "count_neg_food_labelling",
                      "count_pos_food_labelling",
                      "count_mild_reaction",
                      "count_severe_reaction",
                      "count_adverse_reaction")
shape.df.stream1 <- left_join(shape.df,labelled.df.geo.stream1_summary[,names_to_join],by = "District")

library(ggmap)
library(scales)

# load the maps extracted from google
load("resources/Static_maps.RData")
UKrefmap <- UKrefmap_terrain_bw_zoom5_nolables

# get the latitude/longitude ranges for the plots
lon_range <- range(shape.df$long)
lat_range <- range(shape.df$lat)

scale_lon <- c(0.4,0.05)
delta     <- lon_range[2] - lon_range[1]
for(i in 1:2) lon_range[i] <- lon_range[i] + (-1)^i*delta*scale_lon[i]

scale_lat <- c(0.05,0.05)
delta     <- lat_range[2] - lat_range[1]
for(i in 1:2) lat_range[i] <- lat_range[i] + (-1)^i*delta*scale_lat[i]

# Plot with the number of businesses per local authority
number_of_businesses.geo <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.nbusinesses, aes(x=long, y=lat, group=group, fill=TotalEstablishments), color = "black", size=0.2) +
  scale_fill_distiller(name = "# businesses",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5), labels=comma) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of businesses", fill="")
)
number_of_businesses.geo

ggsave(paste("number_of_businesses_map.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the population per local authority
population.geo <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.population, aes(x=long, y=lat, group=group, fill=all_ages), color = "black", size=0.2) +
  scale_fill_distiller(name = "Poulation (2016)",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5), labels=comma) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Population estimated for 2016", fill="")
)
population.geo

ggsave(paste("population_map.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)


# Plot with the Allergy enquiries raw mentions per local authority
selection_allergy_enquiries <- shape.df.stream1$count_allergy_enquiries > 0
allergy_enquiries.summary.geo.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=count_allergy_enquiries), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Allergy enquiries raw mentions", fill="")
)
allergy_enquiries.summary.geo.raw

ggsave(paste("allergy_enquiries_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries mentions normalized by number of establishments per local authority
allergy_enquiries.summary.geo.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_allergy_enquiries/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Allergy enquiries mentions \n(Per ",
                   norm_factor_businesses, " Establishments)"),
       fill="")
)
allergy_enquiries.summary.geo.norm

ggsave(paste("allergy_enquiries_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries mentions normalized by population per local authority
allergy_enquiries.summary.geo.norm.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_allergy_enquiries/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Allergy enquiries mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
allergy_enquiries.summary.geo.norm.pop

ggsave(paste("allergy_enquiries_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries raw mentions with positive sentiment per local authority
selection_allergy_enquiries <- shape.df.stream1$count_pos_allergy_enquiries > 0
allergy_enquiries.summary.geo.pos.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=count_pos_allergy_enquiries), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Positive allergy enquiries raw mentions", fill="")
)
allergy_enquiries.summary.geo.pos.raw

ggsave(paste("allergy_enquiries_positive_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries mentions normalized by number of establishments with positive sentiment per local authority
allergy_enquiries.summary.geo.pos.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_pos_allergy_enquiries/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Positive allergy enquiries mentions \n(Per ",
                   norm_factor_businesses, " Establishments)"),
       fill="")
)
allergy_enquiries.summary.geo.pos.norm

ggsave(paste("allergy_enquiries_positive_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries mentions normalized by population with positive sentiment per local authority
allergy_enquiries.summary.geo.norm.pos.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_pos_allergy_enquiries/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Posotive allergy enquiries mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
allergy_enquiries.summary.geo.norm.pos.pop

ggsave(paste("allergy_enquiries_positive_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries raw mentions with negative sentiment per local authority
selection_allergy_enquiries <- shape.df.stream1$count_neg_allergy_enquiries > 0
allergy_enquiries.summary.geo.neg.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=count_neg_allergy_enquiries), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Negative allergy enquiries raw mentions", fill="")
)
allergy_enquiries.summary.geo.neg.raw

ggsave(paste("allergy_enquiries_negative_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries mentions normalized by number of establishments with negative sentiment per local authority
allergy_enquiries.summary.geo.neg.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_neg_allergy_enquiries/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Negative allergy enquiries mentions \n(Per ",
                   norm_factor_businesses, " Establishments)"),
       fill="")
)
allergy_enquiries.summary.geo.neg.norm

ggsave(paste("allergy_enquiries_negative_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the Allergy enquiries mentions normalized by population with negative sentiment per local authority
allergy_enquiries.summary.geo.norm.neg.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_neg_allergy_enquiries/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Negative allergy enquiries mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
allergy_enquiries.summary.geo.norm.neg.pop

ggsave(paste("allergy_enquiries_negative_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling raw mentions per local authority
selection_food_labelling <- shape.df.stream1$count_food_labelling > 0
food_labelling.summary.geo.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=count_food_labelling), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Food labelling raw mentions", fill="")
)
food_labelling.summary.geo.raw

ggsave(paste("food_labelling_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling mentions normalized by number of establishments local authority
food_labelling.summary.geo.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_food_labelling/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Food labelling mentions \n(Per ",
                   norm_factor_businesses,
                   " Establishments)"), fill="")
)
food_labelling.summary.geo.norm

ggsave(paste("food_labelling_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling mentions normalized by population local authority
food_labelling.summary.geo.norm.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_food_labelling/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Food labelling mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
food_labelling.summary.geo.norm.pop

ggsave(paste("food_labelling_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling raw mentions with positive sentiment per local authority
selection_food_labelling <- shape.df.stream1$count_pos_food_labelling > 0
food_labelling.summary.geo.pos.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=count_pos_food_labelling), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Positive food labelling raw mentions", fill="")
)
food_labelling.summary.geo.pos.raw

ggsave(paste("food_labelling_positive_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling mentions normalized by number of establishments with positive sentiment per local authority
food_labelling.summary.geo.pos.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_pos_food_labelling/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Positive food labelling mentions \n(Per ",
                   norm_factor_businesses,
                   " Establishments)"), fill="")
)
food_labelling.summary.geo.pos.norm

ggsave(paste("food_labelling_positive_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling mentions normalized by population with positive sentiment per local authority
food_labelling.summary.geo.pos.norm.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_pos_food_labelling/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Positive food labelling mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
food_labelling.summary.geo.pos.norm.pop

ggsave(paste("food_labelling_positive_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling raw mentions with negative sentiment per local authority
selection_food_labelling <- shape.df.stream1$count_neg_food_labelling > 0
food_labelling.summary.geo.neg.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=count_neg_food_labelling), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Negative food labelling raw mentions", fill="")
)
food_labelling.summary.geo.neg.raw

ggsave(paste("food_labelling_negative_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling mentions normalized by number of establishments with negative sentiment per local authority
food_labelling.summary.geo.neg.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_neg_food_labelling/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Negative food labelling mentions \n(Per ",
                   norm_factor_businesses,
                   " Establishments)"), fill="")
)
food_labelling.summary.geo.neg.norm

ggsave(paste("food_labelling_negative_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Plot with the food labelling mentions normalized by population with negative sentiment per local authority
food_labelling.summary.geo.neg.norm.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_neg_food_labelling/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Negative food labelling mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
food_labelling.summary.geo.neg.norm.pop

ggsave(paste("food_labelling_negative_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Mild reactions raw mentions per local authority
selection_mild_reaction <- shape.df.stream1$count_mild_reaction > 0
mild_reaction.summary.geo.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_mild_reaction,], aes(x=long, y=lat, group=group, fill=count_mild_reaction), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Mild reaction raw mentions", fill="")
)
mild_reaction.summary.geo.raw

ggsave(paste("mild_reaction_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Mild reactions mentions normalized by number of establishments per local authority
mild_reaction.summary.geo.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_mild_reaction,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_mild_reaction/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Mild reaction mentions \n(Per ",
                   norm_factor_businesses,
                   " Establishments)"), fill="")
)
mild_reaction.summary.geo.norm

ggsave(paste("mild_reaction_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Mild reactions mentions normalized by population per local authority
mild_reaction.summary.geo.norm.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_mild_reaction,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_mild_reaction/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Mild reaction mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
mild_reaction.summary.geo.norm.pop

ggsave(paste("mild_reaction_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Severe reactions raw mentions per local authority
selection_severe_reaction <- shape.df.stream1$count_severe_reaction > 0
severe_reaction.summary.geo.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_severe_reaction,], aes(x=long, y=lat, group=group, fill=count_severe_reaction), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Severe reaction raw mentions", fill="")
)
severe_reaction.summary.geo.raw

ggsave(paste("severe_reaction_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Severe reactions mentions normalized by number of establishments per local authority
severe_reaction.summary.geo.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_severe_reaction,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_severe_reaction/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Severe reaction mentions \n(Per ",
                   norm_factor_businesses,
                   " Establishments)"), fill="")
)
severe_reaction.summary.geo.norm

ggsave(paste("severe_reaction_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Severe reactions mentions normalized by population per local authority
severe_reaction.summary.geo.norm.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_severe_reaction,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_severe_reaction/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Severe reaction mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
severe_reaction.summary.geo.norm

ggsave(paste("severe_reaction_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Mild+Severe reactions raw mentions per local authority
selection_adverse_reaction <- shape.df.stream1$count_adverse_reaction > 0
adverse_reaction.summary.geo.raw <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_adverse_reaction,], aes(x=long, y=lat, group=group, fill=count_adverse_reaction), color = "black", size=0.2) +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title="Adverse reaction raw mentions", fill="")
)
adverse_reaction.summary.geo.raw

ggsave(paste("adverse_reaction_map_raw.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Mild+Severe reactions mentions normalized by number of establishments per local authority
adverse_reaction.summary.geo.norm <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_adverse_reaction,], aes(x=long, y=lat, group=group, fill=norm_factor_businesses*count_adverse_reaction/TotalEstablishments),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Adverse reaction mentions \n(Per ",
                   norm_factor_businesses,
                   " Establishments)"), fill="")
)
adverse_reaction.summary.geo.norm

ggsave(paste("adverse_reaction_map_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Mild+Severe reactions mentions normalized by population per local authority
adverse_reaction.summary.geo.norm.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_adverse_reaction,], aes(x=long, y=lat, group=group, fill=norm_factor_population*count_adverse_reaction/all_ages),
               color = "black", size=0.2) +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  xlim(lon_range) +
  ylim(lat_range) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Adverse reaction mentions \n(Per ",
                   norm_factor_population/1000,
                   "k people)"), fill="")
)
adverse_reaction.summary.geo.norm

ggsave(paste("adverse_reaction_map_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

######################### 14 Allergen Analysis ########################################
# Prepare two dataframes for plotting
# labelled.df.geo.allergenSummary for centroid based plots
# shape.df for Choropleth plots by local authority

labelled.df.geo.allergens <- labelled.df.geo[,c("original_content",
                                                fourteen.allergen.names,
                                                other.allergen.names,
                                                "objectid",
                                                "food_labelling",
                                                "reactions_report",
                                                "allergy_enquiries",
                                                "lat", #latitude of District, not of original entry
                                                "long", #longitude of District, not of original entry
                                                "District",
                                                "objectid",
                                                "sentiment_class")]

library(tidyr)
labelled.df.geo.allergenSummary <- gather(labelled.df.geo.allergens,
                               Allergen,
                               "Mentions",
                               c(fourteen.allergen.names,other.allergen.names),
                               factor_key = TRUE
                               )

labelled.df.geo.allergenSummary <- labelled.df.geo.allergenSummary %>%
  group_by(sentiment_class, Allergen, District, long, lat, objectid, food_labelling, reactions_report, allergy_enquiries) %>%
  summarise(count=sum(Mentions))

labelled.df.geo.allergenSummary <- subset(labelled.df.geo.allergenSummary, count > 0)
labelled.df.geo.allergenSummary$sentiment_class <- factor(labelled.df.geo.allergenSummary$sentiment_class, levels = c("negative", "neutral", "positive"))
labelled.df.geo.allergenSummary <- labelled.df.geo.allergenSummary[!is.na(labelled.df.geo.allergenSummary$lat),]

# join the normalization infomation
labelled.df.geo.allergenSummary <- left_join(labelled.df.geo.allergenSummary,
                                             restaurants_per_local_authority.df[,c("District","TotalEstablishments")], "District")

# add another normalization information: population and demographics
labelled.df.geo.allergenSummary <- left_join(labelled.df.geo.allergenSummary,
                                             population_per_local_authority.df[,c("District","all_ages")], "District")

# Join allergens with map polygons
allergens_to_join    <- c("District","TotalEstablishments","all_ages",
                          "Allergen","sentiment_class", "count", "food_labelling",
                          "allergy_enquiries", "reactions_report")

shape.df.allergens <- left_join(shape.df,labelled.df.geo.allergenSummary[,allergens_to_join], by = "District")

# 14 Allergens by sentiment
# (each point is centered inside local authority with size proportional to normalized count)
# 14 Allergens:
fourteen.summary.geo <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group),
            color="black", size=0.3) +
  geom_point(data = subset(labelled.df.geo.allergenSummary, Allergen %in% fourteen.allergen.names & Allergen != "nuts"),
             aes(x = long, y = lat, colour = gsub("_"," ",sentiment_class),
                 size = norm_factor_businesses*count/TotalEstablishments),
             alpha = 0.3) +
  scale_colour_manual(values = c("red","white", "navy"))+
  scale_x_discrete(limits = lon_range, expand = c(0,0)) +
  scale_y_discrete(limits = lat_range, expand = c(0,0)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("14 Allergen Mentions by Sentiment Class","\n", "(Per ", norm_factor_businesses, " Establishments)"),
       fill="", size = "Norm. Mentions", colour = "Sentiment Class")
)
fourteen.summary.geo

ggsave(paste("Fourteen_allergens_sentiment_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

fourteen.summary.geo.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group),
            color="black", size=0.3) +
  geom_point(data = subset(labelled.df.geo.allergenSummary, Allergen %in% fourteen.allergen.names & Allergen != "nuts"),
             aes(x = long, y = lat, colour = gsub("_"," ",sentiment_class),
                 size = norm_factor_population*count/all_ages),
             alpha = 0.3) +
  scale_colour_manual(values = c("red","white", "navy"))+
  scale_x_discrete(limits = lon_range, expand = c(0,0)) +
  scale_y_discrete(limits = lat_range, expand = c(0,0)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("14 Allergen Mentions by Sentiment Class","\n", "(Per ", norm_factor_population/1000, "k people)"),
       fill="", size = "Norm. Mentions", colour = "Sentiment Class")
)
fourteen.summary.geo.pop

ggsave(paste("Fourteen_allergens_sentiment_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Other Allergens:
other.sentiment.geo <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group),
            color="black", size=0.3) +
  geom_point(data = subset(labelled.df.geo.allergenSummary, Allergen %in% other.allergen.names),
             aes(x = long, y = lat, colour = sentiment_class,
                 size = norm_factor_businesses*count/TotalEstablishments),
             alpha = 0.5) +
  scale_colour_manual(values = c("red","white", "navy"))+
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Other Allergen Mentions by Sentiment Class","\n", "(Per ", norm_factor_businesses, " Establishments)"),
       fill="", size = "Norm. Mentions", colour = "Sentiment Class")
)
other.sentiment.geo

ggsave(paste("Other_allergens_sentiment_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

other.sentiment.geo.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group),
            color="black", size=0.3) +
  geom_point(data = subset(labelled.df.geo.allergenSummary, Allergen %in% other.allergen.names),
             aes(x = long, y = lat, colour = sentiment_class,
                 size = norm_factor_population*count/all_ages),
             alpha = 0.5) +
  scale_colour_manual(values = c("red","white", "navy"))+
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Other Allergen Mentions by Sentiment Class","\n", "(Per ", norm_factor_population/1000, "k people)"),
       fill="", size = "Norm. Mentions", colour = "Sentiment Class")
)
other.sentiment.geo.pop

ggsave(paste("Other_allergens_sentiment_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# All Allergens (14 Allergens, Other allergens, and non-descript Mentions of 'nuts'):
all.allergen.sentiment.geo <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group),
            color="black", size=0.3) +
  geom_point(data = labelled.df.geo.allergenSummary,
             aes(x = long, y = lat, colour = sentiment_class,
                 size = norm_factor_businesses*count/TotalEstablishments),
             alpha = 0.5) +
  scale_colour_manual(values = c("red","white", "navy"))+
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("All Allergen Mentions by Sentiment Class","\n", "(Per ", norm_factor_businesses, " Establishments)"),
       fill="", size = "Norm. Mentions", colour = "Sentiment Class")
)
all.allergen.sentiment.geo

ggsave(paste("All_allergens_sentiment_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 20, height = 20, units = "cm",
       dpi = 300)

all.allergen.sentiment.geo.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group),
            color="black", size=0.3) +
  geom_point(data = labelled.df.geo.allergenSummary,
             aes(x = long, y = lat, colour = sentiment_class,
                 size = norm_factor_population*count/all_ages),
             alpha = 0.5) +
  scale_colour_manual(values = c("red","white", "navy"))+
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("All Allergen Mentions by Sentiment Class","\n", "(Per ", norm_factor_population/1000, "k people)"),
       fill="", size = "Norm. Mentions", colour = "Sentiment Class")
)
all.allergen.sentiment.geo.pop

ggsave(paste("All_allergens_sentiment_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 20, height = 20, units = "cm",
       dpi = 300)


# Generate same plot as above but discarding neutral sentiment
all.allergen.sentiment.geo.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
    geom_path(data=shape.df, aes(x=long, y=lat, group=group),
              color="black", size=0.1) +
    geom_point(data = subset(labelled.df.geo.allergenSummary, sentiment_class %in% c("negative", "positive")),
               aes(x = long, y = lat, colour = sentiment_class,
                   size = norm_factor_population*count/all_ages),
               alpha = 0.5) +
    scale_colour_manual(values = c("red","blue", "navy"))+
    scale_x_continuous(limits = lon_range, expand = c(0,0)) +
    scale_y_continuous(limits = lat_range, expand = c(0,0)) +
    theme_nothing(legend=TRUE) +
    labs(title=paste("All Allergen Mentions by Sentiment Class","\n", "(Per ", norm_factor_population/1000, "k people)"),
         fill="", size = "Norm. Mentions", colour = "Sentiment Class")
)
all.allergen.sentiment.geo.pop

ggsave(paste("All_allergens_sentiment_pop_norm_pos_neg.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 20, height = 20, units = "cm",
       dpi = 300)





############################ Allergen Choropleths ####################################

# 14 Allergens:
fourteen.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names & Allergen != "nuts"),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions by Local Authority", "\n", "(Per", norm_factor_businesses, "Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.shape

ggsave(paste("Fourteen_allergens_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)


fourteen.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% top_4_names),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("TOP 4 Allergens Mentions by Local Authority", "\n", "(Per", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.shape.pop

ggsave(paste("TOP4_allergens_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)



# 14 Allergens in the context of food labelling:
fourteen.labelling.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               food_labelling > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions in the Context of Food labelling ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.labelling.shape

fourteen.labelling.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               food_labelling > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions in the Context of Food labelling ", "\n", "(Per ", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.labelling.shape.pop

ggsave(paste("Fourteen_allergens_labelling_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)

# 14 Allergens in the context of Allergy Enquiries
fourteen.enquiries.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               allergy_enquiries > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions in Association with an Enquiry ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.enquiries.shape

ggsave(paste("Fourteen_allergens_enquiries_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)

fourteen.enquiries.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               allergy_enquiries > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions in Association with an Enquiry ", "\n", "(Per ", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.enquiries.shape.pop

ggsave(paste("Fourteen_allergens_enquiries_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)

# 14 Allergens in the context of mild reactions:
fourteen.mild.reactions.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Mild-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions Overlapping with a Mild Reaction ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.mild.reactions.shape

ggsave(paste("Fourteen_allergens_mild_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)

fourteen.mild.reactions.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Mild-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions Overlapping with a Mild Reaction ", "\n", "(Per ", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.mild.reactions.shape.pop

ggsave(paste("Fourteen_allergens_mild_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)

# 14 Allergens in the context of severe reactions:
fourteen.severe.reactions.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Severe-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions Overlapping with A Severe Event ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.severe.reactions.shape

ggsave(paste("Fourteen_allergens_severe_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)

fourteen.severe.reactions.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% fourteen.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Severe-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Fourteen Allergen Mentions Overlapping with A Severe Event ", "\n", "(Per ", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
fourteen.severe.reactions.shape.pop

ggsave(paste("Fourteen_allergens_severe_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 20, units = "cm",
       dpi = 300)

# Other allergens:
other.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names & Allergen != "nuts"),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other Allergen' Mentions by Local Authority", "\n", "(Per", norm_factor_businesses, "Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
other.shape

ggsave(paste("Other_allergens_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 35, units = "cm",
       dpi = 300)

other.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names & Allergen != "nuts"),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other Allergen' Mentions by Local Authority", "\n", "(Per", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
other.shape.pop

ggsave(paste("Other_allergens_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 35, units = "cm",
       dpi = 300)

# Other Allergens in the Context of Food Labelling:
other.labelling.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               food_labelling > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other Allergen' Mentions in the Context of Food labelling ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 6)
)
other.labelling.shape

ggsave(paste("other_allergens_labelling_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)

other.labelling.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               food_labelling > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other Allergen' Mentions in the Context of Food labelling ", "\n", "(Per ", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 6)
)
other.labelling.shape.pop

ggsave(paste("other_allergens_labelling_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)

other.enquiries.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               Allergen != "nuts" &
                               allergy_enquiries > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Other Allergen' Mentions in the Context of Food labelling ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
other.enquiries.shape

ggsave(paste("other_allergens_enquiries_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)

other.enquiries.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               Allergen != "nuts" &
                               allergy_enquiries > 0 ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("Other Allergen' Mentions in the Context of Food labelling ", "\n", "(Per ", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 7)
)
other.enquiries.shape.pop

ggsave(paste("other_allergens_enquiries_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)


# Other Allergens associated with mild reactions:
other.mild.reactions.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Mild-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other Allergen' Mentions Overlapping with a Mild Reaction ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 6)
)
other.mild.reactions.shape

ggsave(paste("other_allergens_mild_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)

other.mild.reactions.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Mild-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other Allergen' Mentions Overlapping with a Mild Reaction ", "\n", "(Per ", norm_factor_population/1000, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 6)
)
other.mild.reactions.shape.pop

ggsave(paste("other_allergens_mild_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)

# Other Allergens associated with severe reactions:
other.severe.reactions.shape <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Severe-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_businesses*count/TotalEstablishments),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other' Allergen Mentions Overlapping with A Severe Event ", "\n", "(Per ", norm_factor_businesses, " Establishments)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 6)
)
other.severe.reactions.shape

ggsave(paste("other_allergens_severe_rest_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)

other.severe.reactions.shape.pop <- suppressMessages(
  ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_polygon(data = subset(shape.df.allergens, Allergen %in% other.allergen.names &
                               Allergen != "nuts" &
                               reactions_report == "Severe-reaction" ),
               aes(x=long, y=lat, group=group,
                   fill=norm_factor_population*count/all_ages),
               color = "black", size=0.01) +
  scale_fill_distiller(name = "Normalized Mentions", palette = "Spectral", breaks=pretty_breaks(n = 5)) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title=paste("'Other' Allergen Mentions Overlapping with A Severe Event ", "\n", "(Per ", norm_factor_population, "k people)")) +
  facet_wrap(~ Allergen, labeller=labeller(Allergen = remove_underscores), ncol = 6)
)
other.severe.reactions.shape.pop

ggsave(paste("other_allergens_severe_pop_norm.",output_format,sep=""), plot = last_plot(), device = NULL, path = out.dir,
       width = 35, height = 40, units = "cm",
       dpi = 300)

cat(paste("Finished static geoplots script","\n",sep=""))
cat("\n\n")

# You have reached the end of the script. how sad.
