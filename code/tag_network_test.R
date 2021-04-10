# Read in saved/tagged works info for a fandom and build a tag network.


library(tidyverse)
library(igraph)


# Import data
(load("data/works_RWBY.Rda"))
#(load("data/tags_RWBY.Rda"))


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

# Rarepairs? (romantic or otherwise, I think)
wtagged %>% 
  filter(type == "Relationship", name == "Redacted")


## Bipartite network from works/tags

# Build edge data frame, then igraph object
# Last step necessary because some numbers duplicated in works/tags lists
df_rel <- wtagged %>% 
  filter(type == "Relationship") %>% 
  select(wid, tag_list, name, word_count) %>% 
  mutate(wid = paste0("W", as.character(wid))) 

gb_rel <- df_rel %>% 
  graph_from_data_frame()

# Set node type
vertex_attr(gb_rel, "type") <- V(gb_rel)$name %in% pull(df_rel, var = wid)
table(V(gb_rel)$type)

# Bipartite projection
bipartite_projection_size(gb_rel)

ptm <- proc.time()
g_rel <- bipartite_projection(gb_rel)[[1]]
(proc.time() - ptm)   # about 30 seconds

# Match in tag labels (not working yet, matches in whole vector)
vertex_attr(g_rel, "label") <- df_rel[match(V(g_rel)$name, df_rel$tag_list), 
                                      "name"]

ptm <- proc.time()
plot(g_rel, vertex.size = 7, vertex.label = V(g_rel)$label)
(proc.time() - ptm)   # 
