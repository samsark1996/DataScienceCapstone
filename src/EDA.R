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
library(SnowballC)
library(textstem)

set.seed(299792854)

# Load complete data
txtpath = "D:/1_Studies/R and related subjects/Coursera 1/ProjectWork/Capstone/"

txtstrm = list()
txtstrm[[1]] = readLines(paste0(txtpath,"Data/final/en_US/en_US.twitter.txt",collapse = ""),skipNul = TRUE)
txtstrm[[2]] = readLines(paste0(txtpath,"Data/final/en_US/en_US.blogs.txt",collapse = ""),skipNul = TRUE)
txtstrm[[3]] = readLines(paste0(txtpath,"Data/final/en_US/en_US.news.txt",collapse = ""),skipNul = TRUE)

# saveRDS(object = txtstrm,file = paste0(txtpath,"/txtstrm.RDS",collapse = ""),ascii = F)

############# Preliminary EDA
## Data summary
Data_sources = c("Tweet","Blogs","News")
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
# As the total data size is considerable, the EDA will be done on a much smaller sample
# txtstrm = readRDS(paste0(txtpath,"src/txtstrm.RDS",collapse = ""))
pct = 0.1
txtstrm_samp = list()

txt_repo = data.table("text" = character(),
                      "source" = character())
for( i in 1:3){
  txt_repo = rbind(txt_repo,
                   data.table("text" = sample(txtstrm[[i]],
                                              size = round(Nlines[i]*pct)),
                              "source" = rep(Data_sources[i],
                                             round(Nlines[i]*pct))))
}
rm(txtstrm)
################################################################################
# Data pre-processing
data("stop_words")
bad_words = readLines(paste0(txtpath,
                             "rec/badwords.txt",
                             collapse = ""),
                      skipNul = TRUE)
bad_words = data.table("word" = bad_words)
reg_token = "[^[:alpha:][:space:]]*"
url_token = "http[^[:space:]]*"
aaa_token = "\\b(?=\\w*(\\w)\\1)\\w+\\b" 
txt_repo_clean = txt_repo %>%
  mutate(text = str_replace_all(text, reg_token, "")) %>%
  mutate(text = str_replace_all(text, url_token, "")) %>%
  mutate(text = str_replace_all(text, aaa_token, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

################################################################################
## N-grams
freq_by_source = function(x,filter_value){
  y = x %>%
    count(source, word) %>%
    group_by(source) %>%
    mutate(proportion = n / sum(n)) %>%
    filter(proportion >= filter_value)
  return(y)
}

freq_overall = function(x,cfd_coverage){
  y = x %>%
    count(word) %>%
    mutate(proportion = n / sum(n)) %>%
    arrange(-proportion) %>%
    mutate(CFD = cumsum(proportion))%>%
    filter(CFD <=cfd_coverage) 
  return(y)
}

# unigram
unigram = txt_repo_clean %>%
  unnest_tokens(word, text) %>%
  anti_join(bad_words) %>%
  anti_join(stop_words)

unigram_freq_by_source = freq_by_source(unigram,filter_value = 0.001)
unigram_freq_overall = freq_overall(unigram,cfd_coverage = 0.9)

# bigram
bigram = txt_repo_clean %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)

bigram_freq_by_source = freq_by_source(bigram,filter_value = 0.001)
bigram_freq_overall = freq_overall(bigram,cfd_coverage = 0.9)


# trigram
trigram = txt_repo_clean %>%
  unnest_tokens(word, text, token = "ngrams", n = 3)

trigram_freq_by_source = freq_by_source(trigram,filter_value = 0.01)
trigram_freq_overall = freq_overall(trigram,cfd_coverage = 0.9)

# quadgrams
quadgrams = txt_repo_clean %>%
  unnest_tokens(word, text, token = "ngrams", n = 4)
quadgrams_freq_by_source = freq_by_source(quadgrams,filter_value = 0.01)
quadgrams_freq_overall = freq_overall(quadgrams,cfd_coverage = 0.9)

  
