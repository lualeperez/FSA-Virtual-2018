#! /usr/bin/env Rscript

library(rword2vec)

# =========================================================
get_similarities = function(keylist,seed_word,dir, n = 200)
{

  # returns the list of top n words (n) similar to the especified search-word (search_word)
  # For this it is used the google-news skip-gram vector model (see vector_model_file_name)
  # You can download the bin file from
  # https://drive.google.com/file/d/0B7XkCwpI5KDYNlNUTTlSS21pQmM/edit?usp=sharing

  vector_model_file_name = "Vocabulary_vector_models/GoogleNews-vectors-negative300.bin"
  similarities <- distance(file_name   = vector_model_file_name,
                           search_word = seed_word,
                           num         = n)

  vec <- c(seed_word,as.character(similarities$word))

  file_out <- paste(dir,"/",keylist,"_",seed_word,"_nsimil",n,".txt",sep="")
  write.table(vec,file_out,sep = ",",row.names=FALSE,col.names=FALSE)

  # to read vector,
  # vec_r <- as.vector(read.table(file_out,sep = ",")$V1)

}
# =========================================================

require("getopt", quietly=TRUE)

spec = matrix(c(
  "keyList",  "k", 1, "character",
  "seedWord", "s", 1, "character",
  "topn",     "n", 1, "integer",
  "dir",      "d", 1, "character"
), byrow=TRUE, ncol=4)

opt = getopt(spec);

if (is.null(opt$keyList)) {
  keylist <- "allergy"
} else {
  keylist <- opt$keyList
}

if (is.null(opt$seedWord)) {
  seed_word <- "allergy"
} else {
  seed_word <- opt$seedWord
}

if (is.null(opt$topn)) {
  topn <- 200
} else {
  topn <- opt$topn
}

if (is.null(opt$dir)) {
  dir <- "files"
} else {
  dir <- opt$dir
}


get_similarities(keylist = keylist, seed_word = seed_word, dir = dir, n = topn)



#
