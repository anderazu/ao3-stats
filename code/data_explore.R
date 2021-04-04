library(tidyverse)
library(ggplot2)


# Import data
(load("data/works.Rda"))

works_raw <- works


## Some really basic initial plots

# Histogram of works posted by date
works %>% ggplot(mapping = aes(x = creat_date)) +
  geom_histogram()


# Break out by completion status 
works %>% 
  count(complete) %>% 
  mutate(frac = n / nrow(works))

works %>% ggplot(mapping = aes(x = creat_date)) +
  geom_histogram() + 
  facet_grid(cols = vars(complete)) + 
  ggtitle("Works posted by date, by completion status")


# Break out by restricted status (use density or restricted gets squashed)
works %>% 
  count(restricted) %>% 
  mutate(frac = n / nrow(works))

works %>% ggplot(mapping = aes(x = creat_date)) +
  geom_histogram(aes(y = ..density..)) + 
  facet_grid(cols = vars(restricted)) + 
  ggtitle("Works posted by date, by restricted status")


# Plot count over time of all/complete/restricted

# First, accumulate counts of each work type 
works <- works_raw %>% 
  arrange(desc(row_number())) %>%   # invert rows first to count up
  mutate(ctAll = row_number()) %>%  # count all works
  group_by(complete) %>%     # count complete works
  mutate(ctComplete = row_number()) %>% 
  ungroup() %>% 
  group_by(restricted) %>%   # now do restricted
  mutate(ctRestrict = row_number()) %>% 
  ungroup()

# For daily totals, cut to unique creat_date and max value of ct
ct_all <- works %>% 
  group_by(creat_date) %>% 
  summarize(ctAll = max(ctAll))   

ct_complete <- works %>% 
  group_by(creat_date, complete) %>% 
  summarize(ctComplete = max(ctComplete))

ct_restrict <- works %>% 
  group_by(creat_date, restricted) %>% 
  summarize(ctRestrict = max(ctRestrict)) 


# All works over time
ct_all %>% ggplot(aes(x = creat_date, y = ctAll)) +
  geom_line() + 
  ggtitle("Cumulative works over time")

# How exponential is it?
ct_all %>% ggplot(aes(x = creat_date, y = ctAll)) +
  geom_line() + 
  scale_y_log10() + 
  ggtitle("Cumulative works over time")

# Restricted works over time
ct_restrict %>% 
  ggplot(mapping = aes(x = creat_date, y = ctRestrict)) + 
  geom_line(aes(color = restricted)) +
  ggtitle("Restricted work count over time")

# Complete works over time
ct_complete %>% 
  ggplot(mapping = aes(x = creat_date, y = ctComplete)) + 
  geom_line(aes(color = complete))  +
  ggtitle("Completed work count over time")


## Language frequency

# Not helpful, too much English
works %>% 
  count(language) %>% 
  ggplot(aes(x = language, y = n)) + geom_col()

# Axis labels hard to read without filtering for frequency
works %>% 
  count(language) %>% 
  filter(language != "en", n >= 50) %>% 
  ggplot(aes(x = n, y = language)) + 
  geom_col() +
  ggtitle("Frequency of works by language (omitting English)")


## Languages over time

# Top non-English languages
top_lang <- works %>% 
  #filter(language != "en") %>% 
  count(language) %>% 
  slice_max(order_by = n, n = 10) %>% 
  mutate(frac = n / nrow(works))

# Cumulative counts by language, per day
ct_lang <- works %>% 
  filter(language %in% top_lang$language) %>% 
  group_by(language) %>% 
  mutate(ctLang = row_number()) %>%  # count all works
  group_by(creat_date, language) %>% 
  summarize(ctLang = max(ctLang))

# Plots of cumulative works by language over time
ct_lang %>% 
  filter(language != "en") %>% 
  ggplot(aes(x = creat_date, y = ctLang)) +
  geom_line() + 
  facet_wrap(vars(language)) + 
  ggtitle("Works over time, top non-English languages")  

# Cumulative works per language, same plot
ct_lang %>% 
  filter(language != "en") %>% 
  ggplot(aes(x = creat_date, y = ctLang)) +
  geom_line(aes(color = language)) + 
  scale_y_log10() +
  ggtitle("Works over time, top non-English languages")  


## Word count

# Top and bottom of the distribution
works %>% 
  count(word_count) %>% 
  slice_head(n = 5)

works %>% 
  count(word_count) %>% 
  slice_tail(n = 5)


# Frequency plot: Semilog
works %>% 
  ggplot(aes(x = word_count)) + 
  geom_histogram(bins = 25) + 
  scale_y_log10() +
  ggtitle("Word count frequency")

# Frequency plot: Log-log
works %>% 
  ggplot(aes(x = word_count)) + 
  geom_histogram(bins = 40) + 
  scale_y_log10() + 
  scale_x_log10() + 
  ggtitle("Word count frequency")


## Tags

(load("data/tags.Rda"))

tags_raw <- tags

# What are the most common tags?
tags %>% arrange(desc(cached_count))

# How many tags are unique (no merger ID)?(76%)
tags %>% count(is.na(merger_id)) %>% 
  rename(unique = contains("na")) %>% 
  mutate(frac = n / nrow(tags))

# Are all canonical tags non-mergered? (nope)
tags %>% 
  filter(!is.na(merger_id)) %>% 
  count(canonical)

# Combine those two queries
tags %>% 
  count(is.na(merger_id), canonical) %>% 
  rename(unique = contains("na")) %>% 
  arrange(desc(unique))


# How many aren't used at all? (about 9%)
tags %>% 
  filter(cached_count == 0) %>% 
  tally() %>% 
  mutate(frac = n / nrow(tags))

# Frequency plot
tags %>% ggplot(aes(x = cached_count)) + 
  geom_histogram(bins = 50) +
  #geom_histogram(aes(y = ..density..), bins = 50) +   # displays funky
  scale_y_log10() + 
  scale_x_log10() +
  ggtitle("Frequency of tag use")


# How many Redacted tags?
#tags <- tags_raw %>% 
#  mutate(redacted = (name == "Redacted")) 

tags %>% 
  mutate(redacted = (name == "Redacted")) %>%   # not sure I want to keep this
  group_by(redacted) %>% 
  add_count() %>% 
  summarize(cached_count = sum(cached_count), n = n()) %>% 
  mutate(cachefrac = cached_count / sum(cached_count),
         nfrac = n / nrow(tags))


# How many of different ratings?
tags %>% filter(type == "Rating") %>% 
  select(name, cached_count) %>% 
  mutate(frac = cached_count / sum(cached_count)) 


# How many of different media types?
tags %>% 
  filter(type == "Media")  # this is way fewer than I expected...
