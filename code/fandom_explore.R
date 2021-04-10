# Read in saved/tagged works info for a fandom and start exploring.


library(tidyverse)


# Import data
(load("data/works_RWBY.Rda"))
#(load("data/tags_RWBY.Rda"))


## Exploratory looks

# Tag type frequency
wtagged %>% group_by(type) %>% 
  count()

# Word count histogram
wred %>% 
  ggplot(aes(x = word_count)) + 
  geom_histogram(bins = 40) +
  scale_y_log10() + 
  scale_x_log10() + 
  ggtitle("Word count frequency (RWBY)")

ggsave("plots/wordcount_RWBY.png")

# Language frequency
wred %>% 
  count(language) %>% 
  mutate(frac = n / nrow(wred))
  

## Work frequency over time

# Cumulative work count
works <- wred %>% 
  mutate(ctAll = row_number()) 
  
# For daily totals, cut to unique creat_date and max value of ct
ct_all <- works %>% 
  group_by(creat_date) %>% 
  summarize(ctAll = max(ctAll))

# Plot all works over time
ct_all %>% 
  ggplot(aes(x = creat_date, y = ctAll)) +
  geom_line() + 
  ggtitle("Cumulative works over time (RWBY)")

# How exponential is it?
ct_all %>% 
  ggplot(aes(x = creat_date, y = ctAll)) +
  geom_line() + 
  scale_y_log10() + 
  ggtitle("Cumulative works over time (RWBY)")


## Start checking out relationship tags

wtagged %>% 
  filter(type == "Relationship") %>% 
  group_by(name) %>% 
  count() %>% 
  arrange(desc(n)) 

wtagged %>% 
  filter(type == "Relationship", name == "Redacted")
