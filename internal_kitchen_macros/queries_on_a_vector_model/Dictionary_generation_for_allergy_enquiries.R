source("Dictionary_generation.R")

# Allergy enquiries
# path where input files are
input_directory  <- "/home/aperez/Documentos_importantes/Cosas_Importantes/Reconversion_Profesional/S2DS_Virtual_2018/FSA_project/Code/Stream1_Dictionary/similarity_words/allergen_inquiries_preprocessed_v2/"
# path where output file are going to be written
output_directory <- paste(getwd(),"/dictionaries/",sep="")
output_file      <- "allergy_enquiries_dictionary.csv"
# file queries dictionary
file_list = list(
                 allergy    = c("allergy_preprocessed_truncatedAt50.txt"),
                 info       = c("info_preprocessed_truncatedAt50.txt"),
                 request    = c("request_preprocessed_truncatedAt50.txt"),
                 response   = c("response_preprocessed_truncatedAt50.txt"),
                 restaurant = c("restaurant_preprocessed_truncatedAt50.txt")
                )

produce_dictionary_file(input_directory,file_list,
                        output_directory,output_file)


#
