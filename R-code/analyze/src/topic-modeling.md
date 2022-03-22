Calculating a topic model
================

The final step in our text analysis is to calculate a topic model.
First, we will load a set of R libraries that will assist us in this
process. The `dplyr`, `tidytext`, and `tm` libraries will help us
prepare the data for analysis, the `topicmodels` library is for
computing a topic model and we will use `ggplot2` to visualize our
results.

``` r
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidytext)
library(tm)
```

    ## Loading required package: NLP

``` r
library(topicmodels)
library(ggplot2)
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:NLP':
    ## 
    ##     annotate

Let’s now read in our data that we cleaned earlier. We will continue in
a tidy approach.

``` r
setwd("~/git/UsingTextAsDataWorkshop/R-code/")
df <- tibble(read.csv("clean/output/pc-iii-text-clean.csv", header = TRUE, 
                      stringsAsFactors = FALSE))
head(df)
```

    ## # A tibble: 6 × 7
    ##   filename                         pubPlace date  symbol jobno text  n_words_doc
    ##   <chr>                            <chr>    <chr> <chr>  <chr> <chr>       <int>
    ## 1 2014/npt/conf_2015/pc_iii/1.xml  New York 2014… NPT/C… N142… 3 se…         778
    ## 2 2014/npt/conf_2015/pc_iii/10.xml New York 2014… NPT/C… N143… 3 se…        1012
    ## 3 2014/npt/conf_2015/pc_iii/11.xml New York 2014… NPT/C… N143… 3 se…        1572
    ## 4 2014/npt/conf_2015/pc_iii/12.xml New York 2014… NPT/C… N143… 3 se…        1165
    ## 5 2014/npt/conf_2015/pc_iii/13.xml New York 2014… NPT/C… N143… 3 se…        5639
    ## 6 2014/npt/conf_2015/pc_iii/14.xml New York 2014… NPT/C… N143… 3 se…        5560

## Creating a document-term matrix (DTM)

To calculate a topic model, we need to create a document-term matrix
(DTM). This is because the `topicmodels` package we will be using later
on expects an object of class “DocumentTermMatrix” to compute the Latent
Dirichlet Allocation (LDA) model.

A DTM is a mathematical matrix that describes the frequency with which
terms occur across our selection of documents. In this matrix, each rows
corresponds to one document in the collection and each column
corresponds to one term. The matrix then specifies the number of times a
term (keyword) appeared in a document. The topic model will then figure
out which clusters of text belong to a topic based on the combination of
keywords.

There are many different ways to prepare a DTM for topic modeling. Here,
we want to remove terms that do not help distinguish very well between
topics. This could either be terms that are very rare. Rare terms do not
help us identify topics because when a term does not occur often, we
will not see it cluster with other terms in the text. Removing these
rare terms will help us reduce computation time. At the same time, we
might also want to remove terms that occur too often. This is because
when a term occurs very often, it will cluster with many other keywords
without allowing us to learn anything new about our text.

In the below application, we will drop terms that occur only once, as
well as terms that occur in more than half of our documents. Before we
do that, we will also remove numbers from our text and words that are of
character length==2 or less. We will call the DTM we generate `undocs`.

``` r
undocs <- df %>% 
  #let's remove numbers first
  mutate(text = tm::removeNumbers(text)) %>% 
  #drop words of character length 1 or 2
  mutate(text = gsub('\\b\\w{1,2}\\s','', text)) %>% 
  #tokenize our text by word
  tidytext::unnest_tokens(word, text, token="words") %>%
  #count the occurrence of a word in each document
  count(filename, word) %>%
  group_by(word) %>% 
  #calculate how many times a word occurs across all documents
  mutate(total_count = sum(n)) %>%
  #calculate in how many documents a given word occurs
  mutate(number_docs = n()) %>% 
  ungroup() %>%
  #before we create our DTM, we are going to drop certain terms that
  #  do not seem insightful when determining topics
  #  we eliminate terms that occur only once across all documents
  filter(total_count > 1) %>%
  # and we eliminate terms that occur in more than half the documents
  filter(number_docs < length(unique(filename))/2) %>% 
  # now we are ready to cast our tidy data into a DTM:
  #  to create the DTM, we are using information on 
  #   the filename (aka the document where a term was found), 
  #   the 'word' column that contains the "terms" we decided to keep, 
  #   and the number of times the term occured in a select document
  tidytext::cast_dtm(filename, word, n)
```

## Inspecting the DTM

Before we continue, let’s check out our DTM a bit to get a better idea
of what it looks like:

``` r
dtmrows <- nrow(undocs)
dtmcols <- ncol(undocs)
print(paste("Number of rows:", dtmrows, "Number of columns:", dtmcols))
```

    ## [1] "Number of rows: 71 Number of columns: 3100"

``` r
# the "Docs" dimension refers to our filenames, here are the first 10 
undocs$dimnames$Docs[1:10]
```

    ##  [1] "2014/npt/conf_2015/pc_iii/1.xml"  "2014/npt/conf_2015/pc_iii/10.xml"
    ##  [3] "2014/npt/conf_2015/pc_iii/11.xml" "2014/npt/conf_2015/pc_iii/12.xml"
    ##  [5] "2014/npt/conf_2015/pc_iii/13.xml" "2014/npt/conf_2015/pc_iii/14.xml"
    ##  [7] "2014/npt/conf_2015/pc_iii/15.xml" "2014/npt/conf_2015/pc_iii/16.xml"
    ##  [9] "2014/npt/conf_2015/pc_iii/17.xml" "2014/npt/conf_2015/pc_iii/18.xml"

``` r
# the "Terms" dimension identifies the various keywords we are working with,
#  here the first 10
undocs$dimnames$Terms[1:10]
```

    ##  [1] "access"         "accordingly"    "account"        "accountability"
    ##  [5] "actual"         "addition"       "adequate"       "administrative"
    ##  [9] "advance"        "affair"

``` r
# this is what the frequencies for the first 5 documents and terms are:
inspect(undocs[1:5, 1:4])
```

    ## <<DocumentTermMatrix (documents: 5, terms: 4)>>
    ## Non-/sparse entries: 10/10
    ## Sparsity           : 50%
    ## Maximal term length: 14
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##                                   Terms
    ## Docs                               access accordingly account accountability
    ##   2014/npt/conf_2015/pc_iii/1.xml       1           2       3              1
    ##   2014/npt/conf_2015/pc_iii/10.xml      1           0       0              0
    ##   2014/npt/conf_2015/pc_iii/11.xml      0           1       2              0
    ##   2014/npt/conf_2015/pc_iii/12.xml      0           1       0              0
    ##   2014/npt/conf_2015/pc_iii/13.xml      0           1       1              0

As you can see, our DTM has 71 rows which is equal to the number of
documents we are working with. It has 3100 columns which represents the
total set of keywords we are working with here. In the above example,
looking at the first five documents and first four terms, you can see
that the DTM tracks how many times each term occurred in a select
document.

## Calculating a topic model

To calculate a topic model, we can set a seed so that we can reproduce
the same results over and over. This is useful once you are ready to
publish your analysis and results and want to enable others to replicate
them. We also manually choose how many `n` topics we want to model by
setting k = n, with `n` representing our desired number of topics.

``` r
ntopics <- 4
un_lda <- LDA(undocs, k = ntopics, control=list(seed=4264))
un_lda
```

    ## A LDA_VEM topic model with 4 topics.

We need to extract the per-topic-per-word probabilities, called β
(“beta”), from the model. This designates the probability for each
term that it belongs to every one of the n topics we generated.

``` r
un_topics <- tidytext::tidy(un_lda, matrix = "beta")
un_topics
```

    ## # A tibble: 12,400 × 3
    ##    topic term            beta
    ##    <int> <chr>          <dbl>
    ##  1     1 access      0.00107 
    ##  2     2 access      0.000481
    ##  3     3 access      0.00103 
    ##  4     4 access      0.000651
    ##  5     1 accordingly 0.000145
    ##  6     2 accordingly 0.000263
    ##  7     3 accordingly 0.000421
    ##  8     4 accordingly 0.00113 
    ##  9     1 account     0.00222 
    ## 10     2 account     0.00544 
    ## # … with 12,390 more rows

As you can see above, for example, the term `access` is more likely to
associate with topics 1 and 3, than with topics 2 and 4.

Let’s go ahead and find the `n` number of terms that are most common for
each topic, so we can visualize them. Let’s set the number of keywords
to visualize to `n=15` right now:

``` r
nwords <- 15
un_top_terms <- un_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = nwords) %>% 
  ungroup() %>%
  arrange(topic, -beta)

un_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme_bw(base_size = 10) +
  labs(y="Term", x="beta (Term-per-topic probability)") +
  ggtitle(paste(ntopics, "topic-model for UN Documents"))
```

![](topic-modeling_files/figure-gfm/retrieve%20and%20plot%2015%20most%20common%20terms-1.png)<!-- -->

You can play around with the topic modeling code above and calculate
different topic models by setting `ntopics` to a different number of
topics (e.g., 2, 3, 7, etc). Basically, it is up to you to decide how
many topics you want to generate from your data. Your choice may depend
on your research goals and next analytical steps. Once you have settled
on a topic model of your choice, you can also look at the keywords
associated with each topic and give each topic a name based on the most
common terms within it.

## Calculating the number of topics that best fit the data

Depending on your research question, manually setting the number of
topics (k = n) may not be what you want. Instead, you might want to know
which topic model fits the data best based on some metric of model fit.
We can use an R library called `ldatuning` to calculate what number of
topics fits a select text corpus best. You can check out [this
vignette](https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html)
for details on the `ldatuning` package.

To get started, we load the `ldatuning` and `parallel` R libraries. Both
are needed in this next step. We also determine how many cores are
available on our machine for computation.

``` r
library(parallel)
library(ldatuning)
ncores <- parallel::detectCores()
```

Now we can go ahead and calculate the number of topics that would best
represent our data. Depending on the available number of cores on your
machine, the following command may take a while to compute. It is very
computationally intensive, especially as you increase the possible
number of topics to be evaluated.

We will examine the range of two to 50 topics to determine what number
of topics best fits our data. We will again set a seed, so we can
reproduce the same results later on.

``` r
result <- ldatuning::FindTopicsNumber(
  undocs,
  topics = seq(from = 2, to = 50, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 4264),
  mc.cores = ncores,
  verbose = TRUE
)
```

    ## fit models... done.
    ## calculate metrics:
    ##   Griffiths2004... done.
    ##   CaoJuan2009... done.
    ##   Arun2010... done.
    ##   Deveaud2014... done.

Let’s go ahead and plot our results:

``` r
ldatuning::FindTopicsNumber_plot(result)
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

![](topic-modeling_files/figure-gfm/plot%20ldatuning-1.png)<!-- -->

How can we interpret this graph? Using [the
vignette](https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html)
for the `ldatuning` package, we are looking at four different metrics to
evaluate how a chosen number of topics fits the data. The CaoJuan2009
and Arun2010 metrics (top plot) indicate great fit when they are low, so
we are looking for their minimum values. The Griffiths2004 and
Deveaud2014 metrics (bottom plot) indicate great fit when they are high,
so we are looking for their maximum values. Based on our results, we
would conclude that somewhere around 20-30 some topics would model our
select 71 UN documents best. The Deveaud2014 metric does not seem
informative in our case.

This completes our topic modeling exercise. Below you can find some
resources for further reading.

If you have questions or suggestions, you are welcome to reach out to
ARC via [email](mailto:arc-consulting@umich.edu). If you need assistance
while you are learning text analysis or if you are trying to solve a
specific text analysis problem, you are welcome to visit
[CoderSpaces](https://datascience.isr.umich.edu/events/coderspaces/).

## Topic modeling resources for further reading

Below, you can find a few resources with further information on topic
modeling:

  - <https://www.tidytextmining.com/topicmodeling.html>
  - <https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25>
  - <https://towardsdatascience.com/evaluate-topic-model-in-python-latent-dirichlet-allocation-lda-7d57484bb5d0>
