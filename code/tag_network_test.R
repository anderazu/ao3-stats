# Read in saved/tagged works info for a fandom and build a tag network.


library(tidyverse)
library(igraph)


# Import data
(load("data/works_RWBY.Rda"))
(load("data/tags_RWBY.Rda"))


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

# Before doing this, need to replace any duplicate tags with their merged info.

# Preliminary edge data frame from tagged works list
df_rel <- wtagged %>% 
  filter(type == "Relationship") %>% 
  select(wid, tag_list, name, word_count)

# Replace any tags with merger IDs:

# 1. Subset edge data frame by whether rows need merging
temp1 <- df_rel %>% 
  left_join(tred %>% select(id, merger_id), by = c("tag_list" = "id")) %>% 
  filter(is.na(merger_id)) %>%   # if merger_id = NA, no merge needed
  select(-merger_id)

temp2 <- df_rel %>% 
  left_join(tred %>% select(id, merger_id), by = c("tag_list" = "id")) %>% 
  filter(!is.na(merger_id))      # non-NA merger_id, need to merge

# 2. For frame of duplicated tags, replace with merged ID and name
temp2merge <- temp2 %>% 
  select(-tag_list) %>%   
  left_join(tred %>% select(id, name), 
            by = c("merger_id" = "id")) %>%  # pull in merged names and tags
  rename(tag_list = merger_id, name = name.y) %>%   # align names with df_rel
  select(wid, tag_list, name, word_count)    # cut to merged info columns

# 3. Recombine frames into new, merged edge list data
df_rel2 <- bind_rows(temp1, temp2rev) %>% 
  arrange(wid)


# Build igraph object
# 1. Modify wid column to avoid duplicate labels with tag numbers
df_rel3 <- df_rel2 %>% 
  mutate(wid = paste0("W", as.character(wid))) %>% 
  mutate(tag_list = as.character(tag_list))

# Before merging tags, gb_rel had 28843 nodes; after, it has 26384
gb_rel <- df_rel3 %>% 
  graph_from_data_frame()

# Set node type
vertex_attr(gb_rel, "type") <- V(gb_rel)$name %in% pull(df_rel3, var = wid)
table(V(gb_rel)$type)

# Bipartite projection
bipartite_projection_size(gb_rel)

# Before tag merger, g_rel had 7759 nodes; after, it has 5300
ptm <- proc.time()
g_rel <- bipartite_projection(gb_rel)[[1]]
(proc.time() - ptm)   # about 30 seconds

# May be easiest to match node attributes as data frame?
df_grel <- g_rel %>% igraph::as_data_frame(what = "both")

tagsumm <- df_rel3 %>% select(-wid) %>% 
  group_by(tag_list) %>% 
  summarize(name = unique(name), 
            word_count = sum(word_count, na.rm = TRUE)) %>% 
  arrange(as.numeric(tag_list)) 

# PICK UP HERE
# Not working yet
df_reltags$vertices %>% 
  slice_head(n = 5) %>% 
  left_join(tagsumm, by = c("name" = "tag_list"))


 
# Match in tag labels (not working yet, matches in whole vector)
vertex_attr(g_rel, "label") <- df_rel[match(V(g_rel)$name, df_rel$tag_list), 
                                      "name"]

ptm <- proc.time()
plot(g_rel, vertex.size = 7, vertex.label = V(g_rel)$label)
(proc.time() - ptm)   # 
