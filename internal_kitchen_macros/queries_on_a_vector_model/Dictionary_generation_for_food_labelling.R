source("Dictionary_generation.R")

# Food Labelling
# path where input files are
input_directory  <- "/Users/toufikswar/Documents/R/word2vec/final_data/"
# path where output file are going to be written
output_directory <- paste(getwd(),"/dictionaries/",sep="")
output_file      <- "food_labelling_dictionary.csv"
# file queries dictionary
file_list = list(
                 consumer    = c("consumer_cleaned.txt"),
                 incorrect       = c("incorrect_cleaned.txt"),
                 issue       = c("issue_cleaned.txt"),
                 labelling       = c("labelling_cleaned.txt")
                )

produce_dictionary_file(input_directory,file_list,
                        output_directory,output_file)


#
