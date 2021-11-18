# Authors:     Jule Krüger
# Maintainers: Jule Krüger
# Copyright:   2021
#
# Purpose: perform an unsupervised classification of UN documents
#          into two topics, and visualize the results
# ============================================


# resources used for topic modeling:
# https://www.tidytextmining.com/topicmodeling.html
# https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
# https://towardsdatascience.com/evaluate-topic-model-in-python-latent-dirichlet-allocation-lda-7d57484bb5d0
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)

setwd("~/git/UsingTextAsDataWorkshop/R-code/")
df <- tibble(read.csv("clean/output/pc-iii-text-clean.csv", header = TRUE, 
                      stringsAsFactors = FALSE))

#to calculate a topic model, we first need to create a document-term matrix (dtm)
undocs <- df %>% 
  #let's remove numbers first
  # mutate(text = gsub("nuclear", "", text)) %>% 
  # mutate(text = gsub("state", "", text)) %>% 
  # mutate(text = gsub("treaty", "", text)) %>% 
  # mutate(text = gsub("weapon", "", text)) %>% 
  tidytext::unnest_tokens(word, text) %>%
  count(filename, word) %>%
  cast_dtm(filename, word, n)

#perform topic modeling, we set a seed to reproduce the same result over and over
un_lda <- LDA(undocs, k = 2, control=list(seed=4264))
un_lda

# extract the per-topic-per-word probabilities, called β (“beta”), from the model
# the model computes the probability of that term being generated for each topic
un_topics <- tidytext::tidy(un_lda, matrix = "beta")
un_topics

#let's find the 10 terms that are most common for each topic, and visualize them
un_top_terms <- un_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

un_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme_bw(base_size = 20) +
  labs(y="Term", x="beta (Term-per-topic probability)") +
  ggtitle("Two topic-model for select UN Documents")
ggsave("analyze/output/bg-LDA-2-topics.pdf", height = 8, width = 12)
dev.off()
#end of Rscript.