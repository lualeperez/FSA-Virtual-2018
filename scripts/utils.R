# Utilities

## FUNCTIONS:

### LOADING DATA:

library(readxl)          # library to read xlsx files (excel)

#================================================================

### READ CONFIG FILE AND LOAD ANALYSIS CONFIGURATION

read_config_file <- function(config_file,configuration)
{

  # read the configuration file and sets the configuration variables

  # opening the config file
  con <- file(config_file,"r")
  while(TRUE) {
    # get the line
    line <- readLines(con,n=1)
    if(length(line) == 0) {
      # Exit if end of file
      break
    }
    # exclude empty lines
    if(line == "") next
    # exclude comment lines (starting with #)
    if(startsWith(line,"#")) next

    # the elements in a whitespace separated line
    # get a vector with the different elements
    vec_line <- unlist(strsplit(line," "))
    vec_line <- vec_line[vec_line != ""]

    # Extrat the configuration variables from config file
    if(length(vec_line) == 2 & vec_line[1] == "input_file_dir:") {
      # getting the input file directory
      configuration$input_dir <- vec_line[2]
    } else if(length(vec_line) >= 2 & vec_line[1] == "input_file_list:") {
      # getting the list of input files
      configuration$input_file_list <- vec_line[-1]
    } else if(length(vec_line) == 2 & vec_line[1] == "sheet_name:") {
      # getting the data sheet name
      configuration$sheet_name <- vec_line[2]
    } else if(length(vec_line) >= 2 & vec_line[1] == "project_name:") {
      # getting the list of input files
      configuration$project_name <- paste(vec_line[-1],collapse = "_")
    } else if(length(vec_line) == 2 & vec_line[1] == "rerun_data_preprocessing:") {
      # boolean to decide if re-run data pre-processing
      if(vec_line[2] == "YES" | vec_line[2] == "Yes" | vec_line[2] == "yes" | vec_line[2] == "y" | vec_line[2] == "Y") {
        configuration$rerun_data_preprocessing <- TRUE
      } else if(vec_line[2] == "NO" | vec_line[2] == "No" | vec_line[2] == "no" | vec_line[2] == "n" | vec_line[2] == "N") {
        configuration$rerun_data_preprocessing <- FALSE
      }
    } else if(length(vec_line) == 2 & vec_line[1] == "rerun_data_labelling:") {
      # boolean to decide if re-run data labelling
      if(vec_line[2] == "YES" | vec_line[2] == "Yes" | vec_line[2] == "yes" | vec_line[2] == "y" | vec_line[2] == "Y") {
        configuration$rerun_data_labelling <- TRUE
      } else if(vec_line[2] == "NO" | vec_line[2] == "No" | vec_line[2] == "no" | vec_line[2] == "n" | vec_line[2] == "N") {
        configuration$rerun_data_labelling <- FALSE
      }
    } else if(length(vec_line) == 2 & vec_line[1] == "do_static_plots:") {
      # boolean to decide if doing static plots
      if(vec_line[2] == "YES" | vec_line[2] == "Yes" | vec_line[2] == "yes" | vec_line[2] == "y" | vec_line[2] == "Y") {
        configuration$do_static_plots <- TRUE
      } else if(vec_line[2] == "NO" | vec_line[2] == "No" | vec_line[2] == "no" | vec_line[2] == "n" | vec_line[2] == "N") {
        configuration$do_static_plots <- FALSE
      }
    } else if(length(vec_line) == 2 & vec_line[1] == "launch_shiny_dash_board:") {
      # boolean to decide if launch the shiny dashboard
      if(vec_line[2] == "YES" | vec_line[2] == "Yes" | vec_line[2] == "yes" | vec_line[2] == "y" | vec_line[2] == "Y") {
        configuration$launch_shiny_dash_board <- TRUE
      } else if(vec_line[2] == "NO" | vec_line[2] == "No" | vec_line[2] == "no" | vec_line[2] == "n" | vec_line[2] == "N") {
        configuration$launch_shiny_dash_board <- FALSE
      }
    } else if(length(vec_line) == 2 & vec_line[1] == "static_plot_format:") {
      # static plots format
      configuration$static_plot_format <- vec_line[2]
    }

  }
  close(con)

  return(configuration)

}
#================================================================

### PRINT THE LOADED CONFIGURATION FROM CONFIG FILE

print_config <- function(configuration)
{

  # print the contents of the configuration data structure

  cat("\n")
  cat(paste("Data analysis configuration:","\n",sep=""))

  cat(paste("  Input directory:                ",configuration$input_dir,"\n",sep=""))
  file_list <- paste(configuration$input_file_list,collapse=", ")
  cat(paste("  Input file list:                ",file_list,"\n",sep=""))
  cat(paste("  Sheet name:                     ",configuration$sheet_name,"\n",sep=""))
  cat(paste("  Project name:                   ",configuration$project_name,"\n",sep=""))
  answer <- "No"
  if(configuration$rerun_data_preprocessing) answer <- "Yes"
  cat(paste("  Re-run data pre-processing:     ",answer,"\n",sep=""))
  if(configuration$rerun_data_labelling) answer <- "Yes"
  cat(paste("  Re-run data labelling:          ",answer,"\n",sep=""))
  answer <- "No"
  if(configuration$do_static_plots) answer <- "Yes"
  cat(paste("  Doing static plots:             ",answer,"\n",sep=""))
  if(configuration$do_static_plots) {
    cat(paste("   Static plots format:           ",configuration$static_plot_format,"\n",sep=""))
  }
  answer <- "No"
  if(configuration$launch_shiny_dash_board) answer <- "Yes"
  cat(paste("  Launching shiny dash-board:     ",answer,"\n",sep=""))
  cat("\n")

}
#================================================================

### LOADING DATA FUNCTION

load_list_of_xlsx_files <- function(filenames,
                                    sheet_name="Sheet1",
                                    verbose=FALSE)
{

  # reads the data from a list of files into a single data.frame

  nfiles    = length(filenames)  # number of filenames in list
  all_data  = list()             # empty list with all data.frames per file
  col_names = vector()           # empty vector with the columns names
  for(i in 1:nfiles) {

    if(i == 1) {
      # only the 1st file has the columns names
      all_data[[i]] <- read_excel(filenames[i],sheet=sheet_name,col_names=TRUE)
      # save column names
      col_names     <- names(all_data[[i]])

      if(verbose) {
        # print info about the column names of this data
        cat(paste("Data column names (from 1st file):","\n",sep=""))
        print(col_names)
        cat("\n")
      }

    }
    else {
      # extract the rest of the files without column names
      all_data[[i]] = read_excel(filenames[i],sheet=sheet_name,col_names=FALSE)
      # set the same column names as the 1st file
      colnames(all_data[[i]]) <- col_names
    }

    if(verbose) {
      # report on number of records on each dataset on list
      cat(paste("Extracting data from file: ",filenames[i],"\n",sep=""))
      cat(paste("N records in this files = ",nrow(all_data[[i]]),"\n\n",sep=""))
    }
  }

  # concatenate all the data.frames into a single one
  if(verbose) cat(paste("Merging all the data into a single data.frame","\n\n",sep=""))
  df <- do.call(rbind.data.frame, all_data)

  # return the concatenated data.frame
  return(df)

}
#================================================================

### TESTING ORIGINAL TWEET VS PROCESSED TWEET

test_text_preprocessing <- function(df,n.test.records)
{

  # Compares the original and preprocessed text for a random sampling of n.test.records

  # number of records to print
  n.test = n.test.records

  # The number of test records cannot be higher than the number of records
  nrecords = nrow(df)
  if(n.test > nrecords) n.test = nrecords # in such a case reset it to the total number of in the data

  # Set seed for test reproductivility
  set.seed(1)
  # randomly sample the records to be printed out
  sub_sample <- sample(1:nrecords,n.test,replace=FALSE)

  # Print the original and pre-processed content
  for(i in sub_sample) {
    cat("\n")
    cat(paste("Printing tweet id=",df$id[i],", (source = ",df$source[i],")\n",sep=""))
    cat(paste("Original     record:     ",as.character(df$original_content[i],"\n",sep="")))
    cat("\n")
    cat("\n")
    cat(paste("Preprocessed record:     ",as.character(df$content[i],"\n",sep="")))
    cat("\n")
    cat("\n")
    cat("\n")
  }

}
#================================================================

### BUILD A DICTIONARY FROM A CSV FILE

get_dictionary_from_file <- function(dict_filename)
{

  # build a dictionary from a cvs file

  # Vector with the dictionary groupping names
  the_names <- vector()
  # The output dictionary
  myList <- list()

  # opening the file with the dictionary in csv format
  con <- file(dict_filename,"r")
  counter_line      <- 0 # file line counter
  counter_groupping <- 0 # groupping line counter
  # read file line by line
  while(TRUE) {
    # get the line
    line <- readLines(con,n=1)
    if(length(line) == 0) {
      # Exit if end of file
      break
    }

    # Counting the lines already read
    counter_line <- counter_line + 1

    # exclude empty lines
    if(line == "") next
    # exclude comment lines (i.e. lines starting with #)
    if(startsWith(line,"#")) next

    # counting the number of dictionary groups in the file
    counter_groupping <- counter_groupping + 1
    # the elements in a line are comma separated
    # get a vector with the different elements
    vec_line <- unlist(strsplit(line,","))

    # the 1st element is the dictionary group name
    the_names <- c(the_names,vec_line[1])
    # the rest are the dictionary elements
    myList[[counter_groupping]] <- vec_line[-1]
  }
  close(con)

  # Set the dictionary group names
  names(myList) <- the_names

  # Stem the dictionary words
  myList <- stem_dictionary(myList)

  return(dictionary(myList))

}
#==================================================================

### STEM THE WORDS INSIDE A DICTIONARY

stem_dictionary <- function(myList)
{

  # This function takes a dictionary in list format and
  # perform stemming on its terms
  # Returns the stemmed dictionary

  library(quanteda)

  # loop over the dictionary lists
  for(i in 1:length(myList)) {
    # loop over the list elements
    for(j in 1:length(myList[[i]])) {
      term <- myList[[i]][j] # get the different terms

      # separate the terms into words (some dictionary elements can be multiple words)
      words <- unlist(strsplit(term," "))
      words <- words[words != ""]

      # loop over the words in this dictionary term
      for(k in 1:length(words)) {
        endsWithStart <- endsWith(words[k],"*") # check if word ends with *

        # if word starts with * remove it temporarily
        if(endsWithStart) words[k] <- substr(words[k],1,nchar(words[k])-1)

        # stem the word
        words[k] <- paste(tokens_wordstem(tokens(words[k])),collapse="")

        # reattach the * and the end of the word if started with it
        if(endsWithStart) words[k] <- paste(c(words[k],"*"),collapse="")
      }
      # combine all the words into a single term
      term <- paste(words,collapse=" ")
      myList[[i]][j] <- term
    }
    # remove duplicates from the list of term of a dictionary group
    myList[[i]] <- unique(myList[[i]])
    # order them alphabetically
    myList[[i]] <- sort(myList[[i]],decreasing=FALSE)
  }

  # return the stemmed dictonary
  return(myList)

}
#==================================================================

### GET DTM LOOKUP FROM A CORPUS AND A DICTIONARY

from_corpus_to_lookup_dataframe <- function(data.corpus, dict)
{

  # This function takes a <corpus> and a <dictonary>
  # Looks up for dictionary appearences into corpus
  # Returns a normalized data.frame with the dictionary group appearences

  # Document term matrix (DTM) with appearences of the dictionary groups in the corpus
  data.dfm <- dfm(data.corpus, tolower = FALSE, verbose = TRUE, dictionary=dict)

  # Convert appearences table into a data.frame
  processed.df <- convert(data.dfm, "data.frame")
  colnames(processed.df)[1] <- "id"
  processed.df.names <- colnames(processed.df)[-1]

  # Normalize appearences data.frame to have one mention of the dictionary groups per document
  norm.df <- data.frame(id = processed.df$id, ifelse(processed.df[,processed.df.names] > 0, 1, 0))

  return (norm.df)
}
#================================================================

### GET THE TIME UNITS OF A TIME DIFFERENCE

get_time_units <- function(timediff)
{

  # Returns the units of a time-difference

  # get the time-difference in seconds
  timediff <- as.numeric(timediff,units="secs")

  # standard time units in seconds
  amin  <- 60        # a minute
  ahour <- 60*amin   # an hour
  aday  <- 24*ahour  # a day

  the_time_unit = "secs"

  if(timediff/aday > 1) {
    # if the time-different is grater than a day => return "days"
    the_time_unit = "days"
  } else if(timediff/ahour > 1) {
    # if the time-different is smaller than a day but grater than an hour => return "hours"
    the_time_unit = "hours"
  } else if(timediff/amin > 1) {
    # if the time-different is smaller than an hour but grater than a minute => return "mins"
    the_time_unit = "mins"
  }

  return(the_time_unit)

}


### Simple function to remove underscores

remove_underscores <- function(string) {
  new.string <- gsub("_"," ", string)
  paste(new.string)
}



#================================================================


#
