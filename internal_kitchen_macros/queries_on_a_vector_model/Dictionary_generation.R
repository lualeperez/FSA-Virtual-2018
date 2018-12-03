# library(quanteda)
library(stringi)
library(magrittr)

# =========================================================
add_directory_to_file_name = function(myList,directory)
{

  # take a directory path and add it to a list of file names

  for(i in 1:length(myList)) {
    myList[[i]] <- paste(directory,"/",myList[[i]],sep="")
  }

  return(myList)

}
# =========================================================
produce_line_out_of_vec = function(name,file)
{

  # combine list of similar words from file_list and perform some cleaning

  word_list <- c(name,as.vector(read.table(file,sep = ",")$V1))

  # replace _ by a white-space
  word_list <- stri_replace_all_fixed(word_list,"_"," ")

  line <- paste(word_list,collapse=",")
  line <- paste(line,",",sep="")

  return(line)

}
# =========================================================
produce_dictionary_file = function(input_dir,myList,output_dir,output_file)
{

  # take file directory as input, extract word list from seed word queries,
  # concatenate, perform some cleaning and write output lists into a file

  # adding full-path to file name
  myList <- add_directory_to_file_name(myList,input_dir)

  # comabine and clean similarity lists
  the_names <- names(myList)
  list_of_lines <- vector()
  for(i in 1:length(myList)) {
    line <- produce_line_out_of_vec(the_names[i],myList[[i]][1])
    list_of_lines <- c(list_of_lines,line)
  }

  dir.create(output_dir,showWarnings = FALSE)
  file_out <- paste(output_dir,"/",output_file,sep="")
  file.create(file_out)
  for(i in 1:length(list_of_lines)) {
    write(list_of_lines[i],file_out,append=TRUE)
  }

}
# =========================================================

# set to true if want to run the test
run_test <- FALSE
# run_test <- TRUE

if(run_test) {
  # path where input files are
  input_directory  <- "test/test_preprocessed/"
  # path where output file are going to be written
  output_directory <- "test/test_dictionary/"
  output_file      <- "test_dictionary.csv"
  # file queries dictionary
  test_file_list = list(
                        hero   = c("hero_preprocessed_truncatedAt50.txt"),
                        vilain = c("info_preprocessed_truncatedAt50.txt")
                       )

  produce_dictionary_file(input_directory,test_file_list,
                          output_directory,output_file)
}

#
