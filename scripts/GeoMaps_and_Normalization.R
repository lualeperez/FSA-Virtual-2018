#================================================================

### CLEAN LOCAL AUTHORITIES NAMES

clean_UK_local_authorities_names <- function(text)
{

  # This function cleans the UK local authorities names
  # This is important to match the normalization information with the data

  library(stringi)

  text <- stri_trans_tolower(text)
  text <- stri_replace_all_fixed(text, " ?", "")
  text <- stri_replace_all_fixed(text, "-", " ")
  text <- stri_replace_all_fixed(text, "city", "")
  text <- stri_replace_all_fixed(text, "of", "")
  text <- stri_replace_all_fixed(text, "county", "")
  text <- stri_replace_all_fixed(text, "councils", "")
  text <- stri_replace_all_fixed(text, "council", "")
  text <- stri_replace_all_fixed(text, ",", "")
  text <- stri_replace_all_fixed(text, "|", "")
  text <- stri_replace_all_fixed(text, ".", "")
  text <- gsub("\\(", "",text)
  text <- gsub("\\)", "",text)
  text <- gsub("[[:digit:]]+","", text)
  text <- stri_replace_all_fixed(text, "london corporation", "london")
  text <- stri_replace_all_fixed(text, "isle  anglesey", "anglesey")
  text <- stri_replace_all_fixed(text, "south bucks", "south buckinghamshire")
  text <- stri_replace_all_fixed(text, "kingston upon hull", "hull")
  text <- stri_replace_all_fixed(text, "comhairle nan eilean siar western isles", "na h eileanan siar")
  text <- stri_replace_all_fixed(text, "na h eileanan siar", "comhairle nan eilean siar western isles")

  text <- stri_trim(text)

  return(text)
}
#================================================================

# Name of the csv files containing the number of food establishments per local authority from FSA
# Original information can be found in: https://data.food.gov.uk/catalog/datasets/069c7353-4fdd-4b4f-9c13-ec525753fb2c
restaurants_per_local_authority_file <- "resources/2016-17-enforcement-data-food-hygiene.csv" # 2016/2017 Enforcement Data - Food Hygiene

# Create data.frame with information number of establishments per local authorities
restaurants_per_local_authority.df <- read.csv(file = restaurants_per_local_authority_file, header = TRUE, sep=",")

# Select only the Local authority name and TotalEstablishments.IncludingNotYetRated.Outside.
original_list_of_variables <- c("LAName","TotalEstablishments.IncludingNotYetRated.Outside.")
restaurants_per_local_authority.df <- subset(restaurants_per_local_authority.df, select = original_list_of_variables)

# Rename column names
list_of_variables  <- c("LAName","TotalEstablishments")
names(restaurants_per_local_authority.df) <- list_of_variables

# Create a new column with the "cleaned version" (see function at the top of this script) of the local authority name (LAName)
restaurants_per_local_authority.df$District <- as.character(clean_UK_local_authorities_names(restaurants_per_local_authority.df$LAName))

# Name of the csv file containing the population estimated by mid 2016 per local authority
# Originl info from: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
population_per_local_authority_file <- "resources/ukmidyearestimates20122016.csv" # 2012 -> 2016 population estimate
# Create data.frame with local authorities information
population_per_local_authority.df <- read.csv(file = population_per_local_authority_file, header = TRUE, sep=",")

# Now load the UK map of local authorities
# Original infor from: https://blog.exploratory.io/making-maps-for-uk-countries-and-local-authorities-areas-in-r-b7d222939597

# local map related libraries
library(ggmap)
library(rgdal)
library(rgeos)
library(spdplyr)
library(sp)

# load UK local authorities map
uk_county_shapefiles   <- readOGR(dsn = "resources/UK_Local_Authority_2016", layer = "Local_Authority_Districts_December_2016_Super_Generalised_Clipped_Boundaries_in_the_UK")
wgs.84                 <- "+proj=longlat +datum=WGS84"
uk_county_shapefiles   <- spTransform(uk_county_shapefiles, CRS(wgs.84)) # Convert to WGS84 format

# Convert map boundaries to a data.frame format
shape.df               <- fortify(uk_county_shapefiles, region="lad16nm")
# Create a new column with the "cleaned version" (see function at the top of this script) of the local authority name (lad16nm)
shape.df$District <- clean_UK_local_authorities_names(shape.df$id)

library(dplyr)

# Some rows don't have Latitute and longitude. Drop them
labelled.df.geo <- labelled.df[!is.na(labelled.df$latitude),]

# Some entries without original longitude and latitude are assigned the generic "UK"
# latitude and longitude of 55.37805 and -3.435973, respectively. These spuriously inflate
# measurements in Dumfries and Galloway, so these are removed:
lat_UKcentre  <- 55.378050
long_UKcentre <- -3.435973
labelled.df.geo <- subset(labelled.df.geo,
                          latitude !=  lat_UKcentre & longitude != long_UKcentre)

# Some entries without original longitude and latitude are assigned the generic "London"
# latitude and longitude of 51.50732 and -0.127647, respectively. These spuriously inflate
# westminster district, so these are removed:
lat_LondonCentre  <- 51.50732
long_LondonCentre <- -0.127647
labelled.df.geo <- subset(labelled.df.geo,
                          round(latitude,5) !=  lat_LondonCentre & round(longitude,6) != long_LondonCentre)

# Letting R know that these are specifically spatial coordinates
sp <- SpatialPoints(labelled.df.geo[,c("longitude", "latitude")])
# To ensure the same coordinate system
proj4string(sp) <- "+proj=longlat +datum=WGS84"

# Match coordinates to each uk region in uk shapefile
local_authority <- over(sp, uk_county_shapefiles)
# Add local authority information to the data
labelled.df.geo <- cbind(labelled.df.geo, local_authority)

# Some rows don't have local authority name. Drop them
labelled.df.geo <- labelled.df.geo[!is.na(labelled.df.geo$lad16nm),]

# Create a new column with the "cleaned version" (see function at the top of this script) of the local authority name (lad16nm)
labelled.df.geo$District <- clean_UK_local_authorities_names(labelled.df.geo$lad16nm)

#
