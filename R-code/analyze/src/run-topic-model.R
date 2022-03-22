# Authors:     Jule Krüger
# Maintainers: Jule Krüger
# Copyright:   ARC 2022
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
library(tm)
library(topicmodels)
library(ggplot2)

setwd("~/git/UsingTextAsDataWorkshop/R-code/")
df <- tibble(read.csv("clean/output/pc-iii-text-clean.csv", header = TRUE, 
                      stringsAsFactors = FALSE))

#to calculate a topic model, we need to create a DOCUMENT-TERM MATRIX (dtm)
# the DTM counts terms for each document
# the dtm is a mathematical matrix that describes the frequency with which 
#  termsoccur across our 71 UN documents. 
#  in this matrix, each rows corresponds to one document in our collection
#  each column corresponds to one term, giving the number of times
#  that term appeared in the document

#there are many different ways to prepare a dtm for topic modeling
#  here, we want to remove terms that do not help distinguish very 
#  well between topics, such as terms that are very rare (which reduces
#  computation time), as well as terms that occur too often.
#  Below, we will drop terms that occur only once, or in more than
#  half of our documents
undocs <- df %>% 
  #let's remove numbers first
  mutate(text = tm::removeNumbers(text)) %>% 
  #drop words of character length 1 or 2
  mutate(text = gsub('\\b\\w{1,2}\\s','', text)) %>% 
  tidytext::unnest_tokens(word, text) %>%
  count(filename, word) %>%
  group_by(word) %>% 
  #calculate how many times a word occurs across all documents
  mutate(total_count = sum(n)) %>%
  #calculate in how many documents a given word occurs
  mutate(number_docs = n()) %>% 
  ungroup() %>%
  #before we create our DTM, we are going to drop certain terms that
  #  do not seem insightful when determining topics
  # we eliminate terms that occur less than 2 times across all documents
  filter(total_count > 1) %>%
  # and we eliminate terms that occur in more than half the documents
  filter(number_docs < length(unique(filename))/2) %>% 
  
  # now we are ready to cast our tidy data into a DTM,
  #  because the LDA function in the topicmodels library expects 
  #   an object of class "DocumentTermMatrix"
  #  to create the DTM, we are using information on 
  #   the filename (aka the document where a term was found), 
  #   the 'word' column that contains the "terms" we decided to keep, 
  #   and the number of times the term occured in a select document
  tidytext::cast_dtm(filename, word, n)

# before we continue, let's check out our DTM a bit:
#  it has 71 rows, which is equal to the number of documents
#  and 3100 columns, which is the number of terms we are working with
#  the dtm tracks how many times each term occurred in a select document
nrow(undocs)
ncol(undocs)
# the "Docs" dimension refers to our filenames 
undocs$dimnames$Docs[1:10]
# the "Terms" dimension identifies the various keywords we are working with
undocs$dimnames$Terms[1:10]
# this is what the first 
inspect(undocs[1:5, 1:5])


#perform topic modeling, we set a seed to reproduce the same result over and over
#  we manually choose to model n topics, by setting k = n
ntopics <- 2
un_lda <- LDA(undocs, k = ntopics, control=list(seed=4264))
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
  ggtitle(paste(ntopics, "topic-model for select UN Documents"))
outputname <- paste0("analyze/output/bg-LDA-", ntopics, "-topics.pdf")
ggsave(outputname, height = 8, width = 12)
dev.off()

# setting the number of topics (k = n) manually may not be the best idea
#  we might want to know which topic model fits our data best based on
#  a select comparison metric of model fit
#  see this vignette for details:
#    https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
library(parallel)
library(ldatuning)

#this command from the parallel library detects how many cores
#  are available on your machine for computation
ncores <- parallel::detectCores()

# depending on the available number of cores on your machine,
#  the following command may take a while to compute
#  it is very computationally intensive, especially as you 
#  increase the possible number of topics to be evaluated
result <- ldatuning::FindTopicsNumber(
  undocs,
  topics = seq(from = 2, to = 75, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 4264),
  mc.cores = ncores,
  verbose = TRUE
)

# we can plot the results
pdf("analyze/output/plot-LDA-tuning.pdf", height = 7, width = 14, 
    onefile=FALSE)
ldatuning::FindTopicsNumber_plot(result)
dev.off()

# using the vignette for the ldatuning package
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
#  based on our results, we would conclude that 
#  somewhere around 20-30 some topics would model these documents best
#  (Griffiths2004 high/looking for the max, 
#   CaoJuan2009 and Arun 2010 low/looking for the minimum)
#  (Deveaud2014 metric does not seem informative here)

#end of Rscript.