# Read in saved/tagged works info for a fandom and build a tag network.


library(tidyverse)
library(igraph)


# Import data
(load("data/works_RWBY.Rda"))
(load("data/tags_RWBY.Rda"))

wtagged <- wtagged_fandom %>% ungroup()

## Exploratory looks

# Tag type frequency
wtagged %>% group_by(type) %>% 
  count() %>% 
  mutate(frac = n / nrow(wtagged))

# Peek at the top
wtagged %>% 
  filter(type == "Relationship")

# See most-frequent relationship tags
wtagged %>% 
  filter(type == "Relationship") %>% 
  group_by(name) %>% 
  count() %>% 
  arrange(desc(n)) 

# Rarepairs or otherwise uncommon Relationship tags
wtagged %>% 
  filter(type == "Relationship", name == "Redacted")


## Bipartite network from works/tags

# Edge data frame from tagged works list
df_rel <- wtagged %>% 
  filter(type == "Relationship") %>% 
  select(wid, tag_list, name, word_count)


# Build igraph object
# 1. Modify wid column to avoid duplicate labels with tag numbers
df_rel2 <- df_rel %>% 
  mutate(wid = paste0("W", as.character(wid))) %>% 
  mutate(tag_list = as.character(tag_list))

# Before merging tags, gb_rel had 28843 nodes; after, it has 26384
gb_rel <- df_rel2 %>% 
  graph_from_data_frame()

summary(gb_rel)

# Set node type
vertex_attr(gb_rel, "type") <- V(gb_rel)$name %in% pull(df_rel2, var = wid)
table(V(gb_rel)$type)

# Bipartite projection
bipartite_projection_size(gb_rel)

# Before tag merger, g_rel had 7759 nodes; after, it has 5307
ptm <- proc.time()
g_rel <- bipartite_projection(gb_rel)[[1]]
(proc.time() - ptm)   # about 30 seconds

# May be easiest to match node attributes as data frame?
df_grel <- g_rel %>% 
  igraph::as_data_frame(what = "both") %>% 
  map(as_tibble)

str(df_grel, max.level = 2)

tagsumm <- df_rel2 %>% 
  select(-wid) %>% 
  group_by(tag_list) %>% 
  summarize(name = unique(name), 
            word_count = sum(word_count, na.rm = TRUE)) %>% 
  arrange(as.numeric(tag_list)) 

# Match in longer tag name and wordcount
df_grel$vertices <- df_grel$vertices %>% 
  left_join(tagsumm, by = c("name" = "tag_list")) %>% 
  rename(name_long = name.y)

g2 <- graph_from_data_frame(df_grel$edges, 
                            vertices = df_grel$vertices)
summary(g2)


## Check out the igraph object

# Degree distribution
degg2 <- tibble(name = V(g2)$name, deg = degree(g2)) 

degg2 %>% group_by(deg) %>% count()

degg2 %>% ggplot(aes(x = deg)) + 
  geom_histogram(bins = 40) + 
  scale_y_log10() #+
  #scale_x_log10()

# Look at isolates
degg2 %>% filter(deg == 0) %>% 
  left_join(df_grel$vertices)

# Remove isolates
g3 <- delete_vertices(g2, which(degree(g2) == 0)) 

# Plot that shit
ptm <- proc.time()
plot(g3, vertex.size = 7, vertex.label = NA)
(proc.time() - ptm)   # about 20 seconds


# Save a copy
vertex_attr(g3, "Label") <- V(g3)$name_long
write_graph(g3, file = "data/nw_relationship_RWBY.graphml", 
            format = "graphml")
write_graph(g3, file = "data/nw_relationship_RWBY.gml", 
            format = "gml")
