# Authors:     Jule Krüger
# Maintainers: Jule Krüger
# Copyright:   2021
#
# Purpose: count common words and word combinations (n-grams)
# ============================================

library(dplyr)
library(tidytext)
library(ggplot2)

setwd("~/git/UsingTextAsDataWorkshop/R-code/")
df <- tibble(read.csv("clean/output/pc-iii-text-clean.csv", header = TRUE, 
                      stringsAsFactors = FALSE))

#tokenization separates given text into individual tokens
# below, we tokenize each document text into words

#let's identify the ten most common (individual) words across 71 documents
#  and plot them
df %>% 
  tidytext::unnest_tokens(keyword, text) %>% 
  count(keyword, sort = TRUE) %>% 
  mutate(keyword = reorder(keyword, n)) %>% 
  filter(n>1040) %>% 
  ggplot(aes(n, keyword)) +
  geom_col(fill="#2596be") +
  labs(y=NULL, x = "N(keyword)") +
  theme_bw(base_size = 20) +
  ggtitle("Ten most common keywords in select UN Documents")
ggsave("analyze/output/bg-10-most-common-words.pdf", height = 8, width = 12)
dev.off()

#next, let's identify which two words are common to appear together (bigrams),
# and plot them, too
df %>% 
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  filter(n>325) %>% 
  ggplot(aes(n,bigram)) +
  geom_col(fill="#2596be") +
  labs(y=NULL, x = "N(bigram)") +
  theme_bw(base_size = 20) +
  ggtitle("Ten most common bigrams in select UN Documents")
ggsave("analyze/output/bg-10-most-common-bigrams.pdf", height = 8, width = 12)
dev.off()

# let's identify the 10 most common trigrams and plot them
df %>% 
  tidytext::unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  mutate(trigram = reorder(trigram, n)) %>% 
  filter(n>134) %>% 
  ggplot(aes(n,trigram)) +
  geom_col(fill="#2596be") +
  labs(y=NULL, x = "N(trigram)") +
  theme_bw(base_size = 20) +
  ggtitle("Ten most common trigrams in select UN Documents")
ggsave("analyze/output/bg-10-most-common-trigrams.pdf", height = 8, width = 12)
dev.off()

#end of Rscript. 