# Load and explore tag co-occurrence network


library(tidyverse)
library(igraph)


# Import data
load("networks/tags_project_RWBY.Rda")

summary(g_all)


# Extract edge and vertex data frames 
df <- g_all %>% 
  igraph::as_data_frame(what = "both") %>% 
  map(as_tibble)


# Counts of tag types, degree distributions
typeCt <- df$vertices %>% 
  count(type)

verts <- df$vertices %>% 
  bind_cols(deg = degree(g_all))

edges <- df$edges %>% 
  bind_cols(E(g_all)$weight) %>% 
  select(-...4)  # not sure where extra column comes from


# Scatterplot of degree vs. use_count
verts %>% 
  ggplot(aes(x = use_count, y = deg)) + 
  geom_point(aes(color = type)) + 
  scale_x_log10() + 
  scale_y_log10()

# Frequency distributions of tags
verts %>% ggplot(aes(x = type, y = deg)) + 
  geom_violin() + 
  scale_y_log10()


# Remove ArchiveWarning, Category, and Rating tags, plus current fandom
vred <- verts %>% 
  filter(!(type %in% c("ArchiveWarning", "Category", "Rating"))) %>%
  filter(!(name_long == "RWBY"))

ered <- edges %>% 
  mutate(fmatch = from %in% vred$name, 
         tmatch = to %in% vred$name) %>% 
  filter(fmatch & tmatch) %>% 
  select(-fmatch, -tmatch)

gred <- graph_from_data_frame(ered, vertices = vred)

# New degree frequency distributions
vred <- vred %>% bind_cols(degRed = degree(gred))

vred %>% filter(degRed == 0)

vred %>% ggplot(aes(x = type, y = degRed)) + 
  geom_violin() + 
  scale_y_log10()
