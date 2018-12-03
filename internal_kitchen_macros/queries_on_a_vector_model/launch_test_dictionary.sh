#! /bin/bash

# load set of functions
source functions_script.sh

topn=200 # number of similar words to be found
dir="test/test" # output directory to store the similar words list

# checks if output directory exist
check_directory $dir

# define the list of anchor word lists for the
# Allergen inquiries

# request anchor-list
declare -a hero_list=("hero"
                      "stupid");
declare -a villain_list=("villain"
                         "Trump");

# Variables to call the function are,
# - the query-list name
# - the actial query list
# - the top-n similar words limit
# - the output directory
get_similarities_for_seedwords  "hero"     "$(echo ${hero_list[@]})"     $topn   $dir
get_similarities_for_seedwords  "villain"  "$(echo ${villain_list[@]})"  $topn   $dir


#
