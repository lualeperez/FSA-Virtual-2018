### Master Script for Finding Public Mentions of Allergens in Social Media Data

source("scripts/utils.R")

cat("\n")
cat("\n")
cat("\n")
cat("       ################################ GetNuts 1.0 ##################################")
cat("\n")

cat(c("       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,***/((#########((/**,,,,,,,,,,,,,,,,,,",
"       ,,,,,,#####((((###################((((//**,,.           .,,/(########(/*,,,,,,,",
"       ,,,,,,##                                                           .*/###*,,,,,",
"       ,,,,,,/#.        ,*//////*//*/*                                        (#,,,,,,",
"       ,,,,,,,#/     *///////////*/*////*        .....               //**/     ,#*,,,,",
"       ,,,,,,,##   ./////*/((/*,/(///(/.     ,*//////////*          /////(.    (#,,,,,",
"       ,,,,,,,(#   /////*(,                 //*////((///////*      /////       ##,,,,,",
"       ,,,,,,,(#. ,///////    *////////// ////////*   *//////*(///////////* .#(,,,,,,,",
"       ,,,,,,,/#, ./*/////    ./////**/// ////////**///*////* (//////(((/(  ,#/,,,,,,,",
"       ,,,,,,,/#,  ///////,    ,.../*/*// ,*//*//     .,       ,//////(      /#*,,,,,,,",
"       ,,,,,,,/#*  .///////***,***//////   *//////*///*/////  //////(,     .(#,,,,,,,,",
"       ,,,,,,,*#/    ////////*//*/////      ,(//////////    ,*/////(      *#(,,,,,,,,,",
"       ,,,,,,,,#(.     .*((((((((((*              ..,...     .*(((/(,      ##*,,,,,,,,",
"       ,,,,,,,,##*            .*((####################((*,.        .      .##,,,,,,,,,",
"       ,,,,,,,,*##    .,(############################%%%%%%#####/,        (#/,,,,,,,,,",
"       ,,,,,,,,,##,,########%#######################%,    /%########(,   .#(,,,,,,,,,,",
"       ,,,,,,,,,(###%%%%%#.    *%%#%%%%%%%###%%%%%%%&.    %#####%%%%%%%#((#//,,,,,,,,,",
"       ,,,,,,,(###%              /%%     %##%/   /       ,&(       #%###(//*,,,,,,,,,,",
"       ,,,,,,(####%,     .(#,     %%     #%*/     //.        *.       ,%######//,,,,,,",
"       ,,,,,*#####%#     #%,     (%      #%*/     /%,    (%%#     */(%%%######/*,,,,,,",
"       ,,,,,*######%.    #%#%     .%     #%*/     (%     %##%*         *%#####(,,,,,,,",
"       ,,,,,*(#####%/    ,%##      #     %%%%.    (%     ##%%%#(*     %#####*,,,,,,,,,",
"       ,,,,,,*/#####%     %##%*  .,(*            .%/    ,%##%*   .     ,%####*,,,,,,,,",
"       ,,,,,,,*/(###%,   .#%##%%%###%%.        .#%%/.   (%#%#        .#%###*,,,,,,,,,,",
"       ,,,,,,,,,,*//#%%#################%%%%%%%#########%####%%&%%&%%###(,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,*///(###########################################/,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,*/////(###############################(/*,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,***///////////(((/////**,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
), sep = "\n")
cat("       ###############################################################################")
cat("\n")
cat("       Authors:\n")
cat("         A. Newman\n")
cat("         A. Perez Perez\n")
cat("         O. Pervane\n")
cat("         T. Swar\n")
cat("       ###############################################################################")
cat("\n")
cat("\n")
cat("\n")


# Detect operating system
OS <- as.character(Sys.info()["sysname"])
if(OS == "Linux") {
  cat(paste("\n","It looks like you are running Linux","\n"))
} else if(OS == "Darwin") {
  cat(paste("\n","It looks like you are running macOS","\n"))
} else if(OS == "Windows") {
  cat(paste("\n","It looks like you are running Windows","\n"))
}
cat("\n")

# Data structure with all the configuration variables
configuration = list(input_dir                = "",
                     input_file_list          = vector(),
                     sheet_name               = "",
                     project_name             = "",
                     static_plot_format       = "png",
                     rerun_data_preprocessing = FALSE,
                     rerun_data_labelling     = FALSE,
                     do_static_plots          = FALSE,
                     launch_shiny_dash_board  = FALSE)

# generic ouput file folder
output_dir <- "output_files"
# If output_dir doesn't exist, create it.
if(!dir.exists(file.path(output_dir))) dir.create(file.path(output_dir))

# Name of the output static plots folder
plots_output_dir <- ""

# List with the possible static plot formats
possible_static_plots_formats <- c("eps","ps","pdf","jpeg","tif","png","bmp","svg")

# Names of the preprocessing and labelling image files
# used for later access to preprocessed and labelled data
image_preprocessing <- ""
image_analysis      <- ""

# nomaliation factors for number of Establishments and population
norm_factor_businesses <- 100      # numbers are expressed per 100  Establishments
norm_factor_population <- 100000   # numbers are expressed per 100k People

allergies_and_social_media <- function(config_file)
{

  # Master function controlling the pipeline of allergiesand social media analysis
  # It reads the input configuration file to define
  # - The data to be used
  # - The project name
  # - The set of outputs: static plots (with format) or shiny dashboard

  # Read the configuration file. Use deparse and assign functions to change the configuration global variable value
  configuration_tmp <- deparse(substitute(configuration))
  configuration     <- read_config_file(config_file,configuration)
  assign(configuration_tmp,configuration,pos=parent.frame())

  # Check if the inputs are reasonable
  if(length(configuration$input_file_list) == 0) {
    stop("Input file list has no elements.
         \n  You must specify one or more files (separated by white spaces) in the config file after 'input_file_list:'.")
  } else if(configuration$sheet_name == "") {
    stop("Sheet name not specified.
         \n  Please specify the sheet name of the excel data file under 'sheet_name:' in the config file")
  } else if(configuration$project_name == "") {
    stop("Project name not specified.
         \n  Please specify a project name under 'project_name:' in the config file.")
  }

  # Check if the specified static plots format is one of the predefined formats
  if(configuration$do_static_plots & !(configuration$static_plot_format %in% possible_static_plots_formats)) {
    all_formats <- paste(possible_static_plots_formats,collapse = ", ")
    stop(paste("Specified static plot output filetype ", configuration$static_plot_format,
               " is not a supported filetype.  (Plot output filetype can be one of ",all_formats,").",sep=""))
  }

  # Print the current configuration
  print_config(configuration)

  # Create the output directory. Use deparse and assign functions to change the configuration global variable value
  output_dir_tmp <- deparse(substitute(output_dir))
  output_dir     <- paste(output_dir,"/",configuration$project_name,"/",sep="")
  assign(output_dir_tmp,output_dir,pos=parent.frame())
  if(!dir.exists(file.path(output_dir))) dir.create(output_dir,showWarnings = FALSE)

  # Create the static plots output directory. Use deparse and assign functions to change the configuration global variable value
  plots_output_dir_tmp <- deparse(substitute(plots_output_dir))
  plots_output_dir     <- paste(output_dir,"plots/",sep="")
  assign(plots_output_dir_tmp,plots_output_dir,pos=parent.frame())
  if(!dir.exists(file.path(plots_output_dir))) dir.create(plots_output_dir,showWarnings = FALSE)

  # Set the preprocessing image file name. Use deparse and assign functions to change the configuration global variable value
  image_preprocessing_tmp <- deparse(substitute(image_preprocessing))
  image_preprocessing     <- paste(output_dir,"Preprocessing.RData",sep="")
  assign(image_preprocessing_tmp,image_preprocessing,pos=parent.frame())

  # Set the analysis image file name. Use deparse and assign functions to change the configuration global variable value
  image_analysis_tmp <- deparse(substitute(image_analysis))
  image_analysis     <- paste(output_dir,"Waterfall.RData",sep="")
  assign(image_analysis_tmp,image_analysis,pos=parent.frame())

  #####################################
  # Data pre-processing and labelling #
  #####################################

  # Check if preprocessing image file already exists
  file_exists_preprocessing <- file.exists(image_preprocessing)
  generated_labelling       <- FALSE
  if(!file_exists_preprocessing | configuration$rerun_data_preprocessing) {
    # if preprocessing image don't exits or if the user sets rerun_data_preprocessing to yes, then run preprocessing + labelling
    if(!file_exists_preprocessing) {
      cat("\n")
      cat(paste("Preprocessing .RData image file ",image_preprocessing," doesn't exist. Running preprocessing and labelling to generate it.","\n",sep=""))
      cat("\n")
    } else if(file_exists_preprocessing & configuration$rerun_data_preprocessing) {
      cat("\n")
      cat(paste("Preprocessing image file ",image_preprocessing," exists, but rerun_data_processing is set to 'yes'. Re-running preprocessing and labelling.","\n",sep=""))
      cat("\n")
    }

    # Data preprocessing script
    source("scripts/Preprocessing.R")

    # Data labelling script
    source("scripts/Waterfall.R")

    generated_labelling <- TRUE
  } else {
    cat("\n")
    cat(paste("Preprocessing .RData image file ",image_preprocessing," exists and rerun_data_preprocessing is set to 'no'. Passing to next pipe-line step.","\n",sep=""))
    cat("\n")
  }

  # Checks if labellig image file exists
  file_exists_analysis <- file.exists(image_analysis)
  if(!file_exists_analysis | (configuration$rerun_data_labelling & !generated_labelling)) {
    # if labelling image for labelling don't exits or if the user sets rerun_data_labelling to yes, then run labelling only
    if(!file_exists_analysis) {
      cat("\n")
      cat(paste("Labelling .RData image file ",image_analysis," doesn't exist. Running labelling to generate it.","\n",sep=""))
      cat("\n")
    } else if(file_exists_analysis & (configuration$rerun_data_labelling & !generated_labelling)) {
      cat("\n")
      cat(paste("Labelling .RData image file ",image_analysis," exists, but rerun_data_labelling is set to 'yes'. Re-running labelling.","\n",sep=""))
      cat("\n")
    }

    # Data labelling script
    source("scripts/Waterfall.R")

  } else if(!generated_labelling) {
    cat("\n")
    cat(paste("Labelling .RData image file ",image_analysis," exists and rerun_data_labelling is set to 'no'. Passing to next pipe-line step.","\n",sep=""))
    cat("\n")
  }

  # Set the format of static plots
  output_format <<- configuration$static_plot_format

  # Static plotting
  if(configuration$do_static_plots) {
    # Producing static plots

    # Summary plots script
    source("scripts/plots.R")

    # Map based plots script
    source("scripts/Static_Geoplots.R")
  }

  # Shiny Dashboard
  if(configuration$launch_shiny_dash_board) {
    # Launching shiny dashboard

    library(shiny)

    # launching shiny app
    runApp("Dashboard")
  }

}



#
