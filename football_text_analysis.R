library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(ggplot2)
library(dplyr)
data<-read_csv("~/Desktop/gridPassingTokens1_mod.csv",col_types = list(
  segment = col_integer()))
final <- data %>% unite(word, passerSeasonGrid, receiverSeasonGrid, sep = "_", remove = FALSE) %>% 
  group_by(segment,word) %>% summarize(count=n()) %>% cast_dtm(segment, word, count)
tactical_patterns_lda <- LDA(final, k = 4, control = list(seed = 1234))
tactical_patterns <- tidy(tactical_patterns_lda, matrix = "beta")


tp_top_terms <- tactical_patterns %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

tp_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

patterns_gamma <- tidy(tactical_patterns_lda, matrix = "gamma")
patterns_gamma
