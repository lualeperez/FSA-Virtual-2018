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
print_list = function(myList)
{

  # print list content

  name_list <- names(myList)
  nelements <- length(myList)

  for(i in 1:nelements) {
    print("")
    print(paste("Looking at anchor-list: ",name_list[i],sep=""))
    for(j in 1:length(myList[[i]])) {
      print(paste("  ",j," ",myList[[i]][j],sep=""))
    }
    print("")
  }

}
# =========================================================
get_and_combine_similarity_lists = function(file_list,truncation = 0)
{

  # combine list of similar words from file_list and perform some cleaning

  word_list <- vector()  # vector of final word list
  for(i in 1:length(file_list)) {
    # extract word list from file
    vec <- as.vector(read.table(file_list[i],sep = ",")$V1)

    # perform truncation of original word list
    if(truncation > 0) {
      n <- truncation
      if(length(vec) > n) vec <- vec[1:n]
    }

    # concatenate word lists
    word_list <- c(word_list,vec)
  }

  # Do some cleaning

  # lower-casing
  word_list <- stri_trans_tolower(word_list)
  # remove some garbage words
  word_list <- word_list[!grepl("www",word_list)]
  word_list <- word_list[!grepl("#",word_list)]
  word_list <- word_list[!grepl("html",word_list)]
  word_list <- word_list[!grepl("@",word_list)]
  word_list <- word_list[!grepl("mail",word_list)]
  word_list <- word_list[!grepl("\\:",word_list)]
  word_list <- word_list[!grepl("\\.",word_list)]
  # remove duplicates
  word_list <- unique(word_list)
  # sort the list alphabetically
  word_list <- sort(word_list,decreasing=FALSE)

  return(word_list)

}
# =========================================================
preprocessed_achor_list = function(input_dir,myList,output_dir,truncation = 0)
{

  # take file directory as input, extract word list from seed word queries,
  # concatenate, perform some cleaning and write output lists into a file

  # adding full-path to file name
  myList <- add_directory_to_file_name(myList,input_dir)

  # comabine and clean similarity lists
  myList_preprocessed <- list()
  for(i in 1:length(myList)) {
    myList_preprocessed[[i]] = get_and_combine_similarity_lists(myList[[i]],truncation)
  }
  names(myList_preprocessed) <- names(myList)

  # print_list(myList_preprocess)

  # write lists to a file
  truncation_extension <- ""
  if(truncation > 0) truncation_extension <- paste("_truncatedAt",truncation,sep="")

  dir.create(output_dir,showWarnings = FALSE)
  the_names <- names(myList_preprocessed)
  for(i in 1:length(myList_preprocessed)) {
    file_out <- paste(output_dir,"/",the_names[i],"_preprocessed",truncation_extension,".txt",sep="")
    write.table(myList_preprocessed[[i]],file_out,sep = ",",row.names=FALSE,col.names=FALSE)
  }

}
# =========================================================

# Original seed words queries performed with a 200 top limit
truncations <- c(200,100,50)

# Allergen inquiries
# path where input files are
input_directory  <- "test/test/"
# path where output file are going to be written
output_directory <- "test/test_preprocessed/"
# file queries dictionary
test_file_list = list(
                      hero   = c("hero_hero_nsimil200.txt",
                                 "hero_stupid_nsimil200.txt"),
                      vilain = c("vilain_vilain_nsimil200.txt",
                                 "vilain_Trump_nsimil200.txt")
                      )
# perform the queries preprocessing and save it in outpur directy
for(i in 1:length(truncations)) {
  preprocessed_achor_list(input_directory,
                          allergy_inquiries_file_list,
                          output_directory,
                          truncations[i])

}



#
