rm(list = ls(all.names = TRUE))
cat('\14')

library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(tm)
library(ngram)
library(tidytext)
library(quanteda)
library(readtext)
library(stringi)
library(caret)
library(reshape2)
library(quanteda.textstats)
library(quanteda.textplots)
# require(quanteda.corpora)

# library(RWekajars)
# library(RWeka)

set.seed(299792854)
################################################################################
# Load complete data
txtpath = "D:/1_Studies/R and related subjects/Coursera 1/ProjectWork/Capstone/"
setwd(txtpath)

txtstrm = list()
txtstrm[[1]] = readLines(paste0(txtpath,"Data/final/en_US/en_US.twitter.txt",
                                collapse = ""),skipNul = T)
txtstrm[[2]] = readLines(paste0(txtpath,"Data/final/en_US/en_US.blogs.txt",
                                collapse = ""),skipNul = T)
txtstrm[[3]] = readLines(paste0(txtpath,"Data/final/en_US/en_US.news.txt",
                                collapse = ""),skipNul = T)

# saveRDS(object = txtstrm,file = paste0(txtpath,"/src/txtstrm.RDS",collapse = ""),ascii = F)
cat("loading data complete...\n")

################################################################################
## Data summary
Data_sources = c("Tweet","Blogs","News")
names(txtstrm) = Data_sources
Nlines = vector()
Memory = vector()
N_char = vector()
N_words = vector()
N_punc = vector()
N_wrd_per_line = list()
N_wrd_avg = vector()
# Number of lines in the data
Nlines[1] = length(txtstrm[[1]])
Nlines[2] = length(txtstrm[[2]])
Nlines[3] = length(txtstrm[[3]])
# Size of the variables in R environment
Memory[1] = object.size(txtstrm[[1]])
Memory[2] = object.size(txtstrm[[2]])
Memory[3] = object.size(txtstrm[[3]])
# Number of characters
N_char[1] = sum(nchar(txtstrm[[1]]))
N_char[2] = sum(nchar(txtstrm[[1]]))
N_char[3] = sum(nchar(txtstrm[[1]]))
# Number of words per line
N_wrd_per_line[[1]] = sapply(str_split(txtstrm[[1]],pattern = " "), length)
N_wrd_per_line[[2]] = sapply(str_split(txtstrm[[2]],pattern = " "), length)
N_wrd_per_line[[3]] = sapply(str_split(txtstrm[[3]],pattern = " "), length)

# Number of words per line on average
N_wrd_avg[1] = round(mean(N_wrd_per_line[[1]]))
N_wrd_avg[2] = round(mean(N_wrd_per_line[[2]]))
N_wrd_avg[3] = round(mean(N_wrd_per_line[[3]]))
rm(N_wrd_per_line)

# Count number of punctuations 
N_punc[1] = sum(str_count(txtstrm[[1]],'[[:punct:][:blank:]]+'))
N_punc[2] = sum(str_count(txtstrm[[2]],'[[:punct:][:blank:]]+'))
N_punc[3] = sum(str_count(txtstrm[[3]],'[[:punct:][:blank:]]+'))

Data_summary = data.table("Data sources" = Data_sources,
                          "Size in the memory" = Memory/10e5,
                          "No. of Lines" = Nlines,
                          "No. of Characters" = N_char,
                          "No. of words per line" = N_wrd_avg)
cat("Data summary complete...\n")
################################################################################
## Data partitioning
GetPartitionIndex = function(x){
  p = 0.1
  nx = length(x)
  xpart = sample.int(nx,size = floor(p*nx))
  return(xpart)
}
trainIndex = lapply(txtstrm, GetPartitionIndex)
txtstrm_train = list()
txtstrm_test = list()
for (i in 1:3) {
  txtstrm_train[[i]] = txtstrm[[i]][trainIndex[[i]]]
}
rm(txtstrm)
rm(trainIndex)
cat("Data partitioning done...")
################################################################################
## Creating corpus for quanteda
txtcorp_train = list()
txtcorp_test = list()
for(i in 1:3){
  txtcorp_train[[i]] = corpus(txtstrm_train[[i]])
  
  docvars(txtcorp_train[[i]], 'Source') = Data_sources[i]
  docnames(txtcorp_train[[i]]) = str_c(Data_sources[i],
                                       1:length(txtcorp_train[[i]]),
                                       collapse = "_")
  
}
rm(txtstrm_train)
txtcorp_train = txtcorp_train[[1]]+txtcorp_train[[2]]+txtcorp_train[[3]]
cat("Corpus created")
################################################################################
## Pre-processing
TokenizeAndClean = function(x){
  x = tokens(x,
            what = "word",
            remove_punct = T,
            remove_symbols = T,
            remove_numbers = F,
            remove_url = T,
            remove_separators = T,
            split_hyphens = F,
            include_docvars = F,
            padding = F,verbose = quanteda_options("verbose"))
  # Removing profanity
  badwordlist = readLines(paste0(txtpath,"rec/badwords.txt",collapse = ""),skipNul = T)
  x = tokens_remove(x = x,pattern = badwordlist)
  # Remove stem words
  x = tokens_wordstem(x)
  return(x)
}
txtcorp_train = TokenizeAndClean(txtcorp_train)
cat("cleaning and tokenization complete...")
# saveRDS(object = txtcorp_train,file = paste0(txtpath,"/src/txtcorp_train.RDS",collapse = ""),ascii = F)
# saveRDS(object = txtcorp_test,file = paste0(txtpath,"/src/txtcorp_test.RDS",collapse = ""),ascii = F)
################################################################################
# N-grams
Ngrams_freq = function(x, n_gram,n_terms, filename) {
  cat("1")
  tokWordNg = tokens_ngrams(x, n = n_gram, concatenator = ' ')
  cat("2")
  dfmWordNg = dfm(tokWordNg, tolower = F)
  cat("2.5")
  saveRDS(dfmWordNg,filename)
  cat("3")
  # nGram = textstat_frequency(dfmWordNg,n = n_terms)
  # cat("4")
  # fwrite(nGram, file = filename, row.names = F)
  # cat("5")
}


Ngrams_freq(txtcorp_train,n_gram = 2,n_terms = 100,"uniGram.csv")
# dfm(sents1, ngrams = 1:2, what = "word")
