# Pull all rows for a specific fandom tag from works, convert to long data frame


library(tidyverse)
library(ggplot2)


# Import data
(load("data/works_RWBY.Rda"))
#(load("data/tags.Rda"))


# Exploratory looks
wtagged %>% group_by(type) %>% 
  count()

wred %>% 
  ggplot(aes(x = word_count)) + 
  geom_histogram(bins = 40) +
  scale_y_log10() + 
  scale_x_log10() + 
  ggtitle("Word count frequency (RWBY)")

wred %>% 
  count(language)
  
# Come back to add works / time plots


