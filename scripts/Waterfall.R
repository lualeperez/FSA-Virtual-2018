### Data labelling script

# load the previous preprocessed data
load(image_preprocessing)

# load needed libraries
library(quanteda)
library(tidyr)

## columns names from the original data to be merged with the labels produced in this part of the pipeline
retained_metadata <- c("id",
                       "latitude",
                       "longitude",
                       "date",
                       "sentiment",
                       "sentiment class",
                       "topics",
                       "user name",
                       "user screen name",
                       "user source id")

### =============LOAD DICTIONARIES================= ###
# (will only create if library(quanteda) is loaded correctly)

cat("\n\n")
cat(paste("Start loading dictionaries","\n",sep=""))

start_time1 <- Sys.time()
# Supporting local authorities dictionaries
# dictionary file name
supporting_local_authorities_dict_filename <- "dictionaries/supporting_local_authorities_dictionary.csv"
# Load dictionary and preprocessed => lowercasing and stemming as with the data
supporting_local_authorities.dict <- get_dictionary_from_file(supporting_local_authorities_dict_filename)
cat("\n\n")
cat(paste("Supporting local authorities dictionary","\n",sep=""))
print(supporting_local_authorities.dict)
cat("\n\n")

# Common allergens
# 14 allergen dictionary:
# dictionary file name
fourteen_allergens_dict_filename <- "dictionaries/fourteen_allergens_dictionary.csv"
# Load dictionary and preprocessed => lowercasing and stemming as with the data
fourteen_allergens.dict <- get_dictionary_from_file(fourteen_allergens_dict_filename)
cat("\n\n")
cat(paste("14 allergens dictionary","\n",sep=""))
print(fourteen_allergens.dict)
cat("\n\n")

# Other allergen dictionary:
# dictionary file name
other_allergens_dict_filename <- "dictionaries/other_allergens_dictionary.csv"
# Load dictionary and preprocessed => lowercasing and stemming as with the data
other_allergens.dict <- get_dictionary_from_file(other_allergens_dict_filename)
cat("\n\n")
cat(paste("other allergens dictionary","\n",sep=""))
print(other_allergens.dict)
cat("\n\n")

end_time1 <- Sys.time()
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("Loading dictionary time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished loading dictionaries","\n",sep=""))
cat("\n\n")

# labelled data.frame. Start with the currently pre-processed data.frame and
# will add new columns including the labelling information
labelled.df <- content.df

# Gobal Execution time:
start_time  <- Sys.time()

# Create a corpus including id as a record identifier
content.corpus <- corpus(content.df, docid_field = "id", text_field = "content") # Username and Hashtag metadata is retained

### Stream 1: Supporting Local Authorities
start_time1 <- Sys.time()
# Lookup the dictionary words into the corpus and create a data.frame with the findings
supporting_local_authorities.df.norm <- from_corpus_to_lookup_dataframe(content.corpus,supporting_local_authorities.dict)
supporting_local_authorities.names   <- colnames(supporting_local_authorities.df.norm)[-1]
# merge the findings data.frame with the original one
labelled.df                          <- cbind(labelled.df,supporting_local_authorities.df.norm[,supporting_local_authorities.names])

# Will now create columns related with the Supporting local authorities sub-tasks

# Allergy enquiries
# The label is the following combination: [allergy AND info AND (request OR response) AND restaurant]
labelled.df$allergy_enquiries <- ifelse(labelled.df[,"allergy"] &
                                        labelled.df[,"info"]    &
                                        (labelled.df[,"request"] | labelled.df[,"response"]) &
                                        labelled.df[,"restaurant"],1,0)
allergy.enquiries.names       <- c("allergy_enquiries")

# Food labelling:
# The label is the following combination: [(consurmer AND issue AND labelling)   OR
#                                          (incorrect AND allergy AND labelling) OR
#                                          (consumer AND allergy AND labelling)]
labelled.df$food_labelling <- ifelse((labelled.df[,"consumer"] &
                                      labelled.df[,"issue"] &
                                      labelled.df[,"labelling"])
                                      |
                                     (labelled.df[,"incorrect"] &
                                      labelled.df[,"allergy"] &
                                      labelled.df[,"labelling"])
                                      |
                                     (labelled.df[,"consumer"] &
                                      labelled.df[,"allergy"] &
                                      labelled.df[,"labelling"]) ,1,0)


# Reporting reactions:
# The labels are the following combinations:
#   mild-reaction   => [allergy & symptoms & ingestion AND NOT severe]
#   severe-reaction => [allergy & symptoms & ingestion AND     severe]
mild_reaction   <- ifelse(labelled.df[,"allergy"] & labelled.df[,"symptoms"] & labelled.df[,"ingestion"] &
                          !labelled.df[,"severe"]
                          ,1,0)
severe_reaction <- ifelse(labelled.df[,"allergy"] & labelled.df[,"symptoms"] & labelled.df[,"ingestion"] &
                          labelled.df[,"severe"]
                          ,1,0)
# If no adverse reaction found in text will label it as No-report
labelled.df$reactions_report <- rep("No-report",nrow(labelled.df))
# will label it as either Mild-rection or Severe-reactions
labelled.df$reactions_report[mild_reaction == 1 & severe_reaction == 0] <- "Mild-reaction"
labelled.df$reactions_report[mild_reaction == 0 & severe_reaction == 1] <- "Severe-reaction"
labelled.df$reactions_report <- factor(labelled.df$reactions_report, levels = c("No-report","Mild-reaction", "Severe-reaction"))
labelled.df$severe_reaction  <- ifelse(labelled.df$reactions_report == "Severe-reaction", 1, 0)
labelled.df$mild_reaction    <- ifelse(labelled.df$reactions_report == "Mild-reaction",   1, 0)
reaction.report.names <- c("reactions_report")

end_time1 <- Sys.time()
supporting_local_authorities_time <- as.difftime(end_time1 - start_time1, units = "secs")

### Stream 2: 14 allergens
# 14 allergens labelling
start_time1 <- Sys.time()
fourteen_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, fourteen_allergens.dict)
fourteen.allergen.names    <- colnames(fourteen_allergens.df.norm)[-1]
labelled.df                <- cbind(labelled.df,fourteen_allergens.df.norm[,fourteen.allergen.names])
end_time1 <- Sys.time()
fourteen_allergens_time <- as.difftime(end_time1 - start_time1, units = "secs")

# other allergens labelling
start_time1 <- Sys.time()
other_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, other_allergens.dict)
other.allergen.names    <- colnames(other_allergens.df.norm)[-1]
labelled.df             <- cbind(labelled.df,other_allergens.df.norm[,other.allergen.names])

# If nuts and tree_nut exist, Disambiguate Nuts and Seeds categories
if( ("nuts"      %in% names(labelled.df)) &
    ("tree_nuts" %in% names(labelled.df))
  ) {
  labelled.df$nuts  <- ifelse(labelled.df$nuts  == 1 & labelled.df$tree_nuts    == 0,1,0)
  labelled.df$seeds <- ifelse(labelled.df$seeds == 1 & labelled.df$sesame_seeds == 0,1,0)
}
end_time1 <- Sys.time()
other_allergens_time <- as.difftime(end_time1 - start_time1, units = "secs")

# Merge labelled tweets with other features from the original data.frame (data.df)
library(dplyr)

# Add the useful metadate to the labelled data
labelled.df       <- left_join(labelled.df, data.df[,retained_metadata], "id")

# Add time informations in specific formats
# Convert data to datatime format
labelled.df$date  <- as.Date(labelled.df$date, format= "%Y-%m-%d")
labelled.df$Month <- as.Date(cut(labelled.df$date, breaks = "month")) # add date columns in month format
labelled.df$Week  <- as.Date(cut(labelled.df$date, breaks = "week"))  # add date columns in week  format

#rename sentiment class to a single string ==> remove spaces by "_"
names(labelled.df)[names(labelled.df) == "sentiment class"] <- "sentiment_class"
# redefine sentiment class values not_evaluable and processing as neutral
labelled.df$sentiment_class[labelled.df$sentiment_class %in% c("not_evaluable", "processing")] <- "neutral" # collapse not evaluable and procesing into neutral

# get the UK local authorities map and normalization data
source("scripts/GeoMaps_and_Normalization.R")

# save labelled data to an image file
cat("\n\n")
cat(paste("Saving image of RData to ", image_analysis,"...", "\n",sep=""))
save.image(file = image_analysis)

end_time <- Sys.time()
global_time <- as.difftime(end_time - start_time, units = "secs")

# summary of the output data.frame
cat("\n\n")
cat(paste("Preprocessed data records with UK map information = ",nrow(labelled.df.geo),"\n",sep=""))
cat(paste("Labelled data.frame column names:","\n",sep=""))
print(colnames(labelled.df.geo))
cat("\n\n")

# summary of the excecution times
cat("\n\n")
the_time_unit <- get_time_units(supporting_local_authorities_time)
cat(paste("Supporting local authorities labelling time:  ",round(as.numeric(supporting_local_authorities_time,  units=the_time_unit),5), " ",the_time_unit,"\n",sep=""))
the_time_unit <- get_time_units(fourteen_allergens_time)
cat(paste("14 allergens labelling time:                  ",round(as.numeric(fourteen_allergens_time, units=the_time_unit),5), " ",the_time_unit,"\n",sep=""))
the_time_unit <- get_time_units(other_allergens_time)
cat(paste("Other allergens labelling time:               ",round(as.numeric(other_allergens_time,    units=the_time_unit),5), " ",the_time_unit,"\n",sep=""))
the_time_unit <- get_time_units(global_time)
cat(paste("Total labelling time:                         ",round(as.numeric(global_time,             units=the_time_unit),5), " ",the_time_unit,"\n",sep=""))
cat("\n\n")


#
