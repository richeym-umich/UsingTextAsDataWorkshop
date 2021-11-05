# Authors:     Jule Krüger
# Maintainers: Jule Krüger, Meghan Dailey
# Copyright:   2021
#
# Purpose: Pre-process/clean the UN data for analysis
# ============================================

#there are many R packages for processing and analyzing text, 
# we will work with the below in a tidy approach
library(dplyr)
library(tidytext)
library(tm)
library(textclean)
library(textstem)

setwd("~/git/UsingTextAsDataWorkshop/R-code/")
#load our data
load("import/output/UNdocuments-2014-nptconf-2015pc-iii.Rdata")
text_df <- tibble(documentDF)

#for our purposes, let's remove the words "state" and "states" from stopwords 
#  a Python dictionary of stopwords does not seem to contain these two words
#  removing these words allows us to keep some consistency in results between
#  the two portions of this workshop
stop_words <- stop_words %>% 
  filter(!grepl("^state", word))

clean_text_df <- text_df %>% 
  mutate(date = as.Date(date, format="%Y%m%d")) %>% 
  mutate(text = gsub("New York", "", text)) %>% 
  mutate(text = gsub("United Nations", "", text)) %>% 
  #paste hyphenated words together
  mutate(text = gsub("-", "", text)) %>% 
  #expand contractions
  mutate(text = textclean::replace_contraction(text)) %>% 
  mutate(text = tm::removeNumbers(text)) %>% 
  #unnest_tokens sets to lower, and removes punctuation
  tidytext::unnest_tokens(word, text) %>% 
  mutate(word = textstem::lemmatize_strings(word)) %>% 
  anti_join(stop_words, by="word") %>% 
  #let's also remove words that are just plain numbers after lemmatization
  filter(!grepl("[0-9]", word))

#let's save our clean text data file for analysis later on
write.csv(clean_text_df, file="clean/output/pc-iii-text-clean.csv") 
  
#let's list the ten most common words across the 71 documents
clean_text_df %>% 
  count(word, sort=TRUE)

#end of Rscript.