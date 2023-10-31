
########## TOPIC MODELLING ##########

library(topicmodels)

data("AssociatedPress")
AssociatedPress


# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
#> A LDA_VEM topic model with 2 topics.


# peluang topik
library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# 10 topik yang umum akan muncul

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() #problem: ada kata2 yang sama

# mengatasi agar tidak ada kata yang sama

library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide

# memunculkan grafik

# peluang masuk ke dokumen tertentu

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# peluang kata yg masuk pada topik tertentu (cek sebaran kata2)

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

######## Apbila menggunakan k=3 (3 topik) #########

library(topicmodels)

data("AssociatedPress")
AssociatedPress


# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 3, control = list(seed = 1234))
ap_lda
#> A LDA_VEM topic model with 3 topics.


# peluang topik
library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# 10 topik yang umum akan muncul

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() #problem: ada kata2 yang sama

# apabila dibuat wordcloud

library("ggwordcloud")
set.seed(123)
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#error
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_text_wordcloud(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 4) + #error
  theme_minimal()

?geom_text_wordcloud


# mengatasi agar tidak ada kata yang sama

library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic2 / topic1))#bingung untuk >2 topik

beta_wide
View(beta_wide)

# memunculkan grafik

# peluang masuk ke dokumen tertentu

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# peluang kata yg masuk pada topik tertentu (cek sebaran kata2)

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))