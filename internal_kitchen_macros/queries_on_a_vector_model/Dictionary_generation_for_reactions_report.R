source("Dictionary_generation.R")

# Reporting reactions
# path where input files are
input_directory  <- "/home/aperez/Documentos_importantes/Cosas_Importantes/Reconversion_Profesional/S2DS_Virtual_2018/FSA_project/Code/Stream1_Dictionary/similarity_words/reaction_report_preprocessed_v2/"
# path where output file are going to be written
output_directory <- paste(getwd(),"/dictionaries/",sep="")
output_file      <- "reaction_report_dictionary.csv"
# file queries dictionary
file_list = list(
                 symptons  = c("symptoms_preprocessed_truncatedAt50.txt"),
                 ingestion = c("ingestion_preprocessed_truncatedAt50.txt"),
                 severe    = c("severe_preprocessed_truncatedAt50.txt")
                )

produce_dictionary_file(input_directory,file_list,
                        output_directory,output_file)

#
