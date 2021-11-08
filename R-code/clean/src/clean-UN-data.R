# Authors:     Jule Krüger
# Maintainers: Jule Krüger
# Copyright:   2021
#
# Purpose: Pre-process/clean the UN data to prepare for analysis
# ============================================

#there are many R packages for processing and analyzing text, 
# we will work with the below in a tidy approach
library(dplyr)
library(tidytext)
library(tm)
library(textclean)
library(textstem)
library(stringr)
library(ggplot2)

setwd("~/git/UsingTextAsDataWorkshop/R-code/")
#load our data
load("import/output/UNdocuments-2014-nptconf-2015pc-iii.Rdata")
text_df <- tibble(documentDF)

clean_text_df <- text_df %>% 
  mutate(date = as.Date(date, format="%Y%m%d")) %>% 
  mutate(text = gsub("New York", "", text)) %>% 
  mutate(text = gsub("United Nations", "", text)) %>% 
  #paste hyphenated words together
  mutate(text = gsub("-", "", text)) %>% 
  #expand contractions
  mutate(text = textclean::replace_contraction(text)) %>% 
  mutate(text = tolower(text)) %>%
  mutate(text = tm::removePunctuation(text)) %>% 
  mutate(text = textstem::lemmatize_strings(text)) %>% 
  mutate(text = tm::removeWords(text, stopwords("english"))) %>% 
  mutate(text = tm::removeNumbers(text)) %>% 
  mutate(text = tm::stripWhitespace(text)) %>% 
  #let's count the number of words in each text document
  mutate(n_words_doc = stringr::str_count(text, "\\w+"))

#comparing text pre and post processing
c(text_df[text_df$jobno=="N1432775", "text"])
c(clean_text_df[clean_text_df$jobno=="N1432775", "text"])

#let's save our clean text data file for analysis later on
write.csv(clean_text_df, file="clean/output/pc-iii-text-clean.csv", row.names = FALSE)

# let's plot the number of words over time, out of pure curiosity
clean_text_df %>% 
  arrange(date) %>% 
  ggplot(aes(date,n_words_doc)) + 
  geom_col(col="black", fill="#2596be") +
  theme_bw() + 
  labs(x=NULL, y="N(words)") +
  ggtitle("Total number of words per day")
ggsave("clean/output/bg-total-number-words-day.pdf", height = 4, width = 10)
dev.off()

#end of Rscript.