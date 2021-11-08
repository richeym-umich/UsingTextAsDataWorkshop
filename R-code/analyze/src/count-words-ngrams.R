# Authors:     Jule Krüger
# Maintainers: Jule Krüger
# Copyright:   2021
#
# Purpose: Pre-process/clean the UN data to prepare for analysis
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
  tidytext::unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  filter(n>1040) %>% 
  ggplot(aes(n, word)) +
  geom_col(fill="#2596be") +
  labs(y=NULL, x = "N(words)") +
  theme_bw(base_size = 20) +
  ggtitle("Ten most common words in UN Documents")
ggsave("analyze/output/bg-10-most-common-words.pdf", height = 8, width = 12)
dev.off()


