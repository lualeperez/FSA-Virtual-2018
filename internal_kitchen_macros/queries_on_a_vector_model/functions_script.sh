#! /bin/bash

check_directory()
{

  # checks if directory dir exist. If not it will create it.

  dir=$1

  if [ ! -d "$dir" ]; then
    echo
    echo "directory $dir doesn't exist. Creating it."
    echo

    mkdir -p $dir
  fi

}

script="stream1_dictionary_script.R"
get_similarities_for_seedwords()
{

  # loops over the seed words list
  # then calls a Rscript to get the top n similar words
  # to the seed words by using a vocabulary vector model

  key=$1    # seed-words list name
  list=$2   # list of seed words
  n=$3      # interger to set the limit of similar words
  dir=$4    # directory to save the lists of similar words

  echo
  echo "Begin keylist $key"
  for seed in $list; do  # loop over the list of seed words
    echo "  seed-word = $seed"

    # for each seed word, get the top n similar words to it
    ./$script --keyList $key --seedWord  $seed --topn $n --dir $dir
  done
  echo "End   keylist $key"
  echo

}


#
