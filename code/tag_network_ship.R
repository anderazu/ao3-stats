# Read in tagged works info for a fandom and build a tag network for a ship
#  (or other focal tag).


library(tidyverse)
library(igraph)


# Import data
(load("data/works_RWBY.Rda"))
(load("data/tags_RWBY.Rda"))

wtagged <- wtagged_fandom
rm(wtagged_fandom)


## Reduce to ship-connected works and tags

# Find the tag
tid <- tred %>% 
  filter(name == "Qrow Branwen/Clover Ebi") %>% 
  pull(id)

wship <- wtagged %>% 
  filter(wid %in% (wtagged %>% 
                     filter(tag_list == tid) %>% 
                     pull(wid)))

tship <- tred %>% 
  filter(id %in% wship$tag_list)


## Exploratory looks

# Tag type frequency
wship %>% group_by(type) %>% 
  count() %>% 
  mutate(frac = n / nrow(wship))

# Looking by category
wship %>% filter(type == "Category")

wship %>%     # number of category tags per work (summarize)
  group_by(wid) %>% 
  transmute(ncat = sum(type == "Category")) %>% # number of category tags
  distinct() %>%   # cut to unique rows
  group_by(ncat) %>% 
  count() %>% 
  mutate(frac = n / nrow(wred))


# Peek at the top
wship %>% 
  filter(type == "Relationship")

# See most-frequent relationship tags
wship %>% 
  filter(type == "Relationship") %>% 
  count(name, sort = TRUE)

# Uncommon relationship tags
wship %>% 
  filter(type == "Relationship", name == "Redacted")


## Add tag count information

# Individual tag counts
tag_counts <- wship %>% 
  group_by(tag_list) %>% 
  count()

# Add tag counts (in this ship's works) to tag list
tship <- tship %>% 
  left_join(tag_counts, by = c("id" = "tag_list"))


## Bipartite network from works/tags

# Edge data frame from ship works list
df_all <- wship %>% 
  select(wid, tag_list:name, word_count) %>% 
  mutate(wid = paste0("W", as.character(wid)), 
         tag_list = as.character(tag_list))

# Build igraph object -- all tags
gb_all <- df_all %>% 
  graph_from_data_frame()
summary(gb_all)

# Set node type
vertex_attr(gb_all, "type") <- V(gb_all)$name %in% pull(df_all, var = wid)
table(V(gb_all)$type)

# Bipartite projection
bipartite_projection_size(gb_all)

# Make bipartite projections
# Getting just the first (AKA tags, type = FALSE) projection
ptm <- proc.time()
g_all <- bipartite_projection(gb_all, which = "false")
(proc.time() - ptm)   # < 1 second


# Match node attributes as data frame
df_gall <- g_all %>% 
  igraph::as_data_frame(what = "both") %>% 
  map(as_tibble)

df_gall$vertices
df_gall$edges

# Pull in tag info: type, name, associated wordcount
tagsumm <- df_all %>% 
  select(-wid) %>% 
  group_by(tag_list) %>% 
  summarize(type = unique(type), 
            name = unique(name), 
            word_count = sum(word_count, na.rm = TRUE)) %>% 
  arrange(as.numeric(tag_list)) 

# Match in longer tag name and wordcount, add use count from tred frame
df_gall$vertices <- df_gall$vertices %>% 
  left_join(tagsumm, by = c("name" = "tag_list")) %>% 
  rename(name_long = name.y) %>% 
  left_join(tship %>% select(id, n) %>% mutate(id = as.character(id)), 
            by = c("name" = "id")) %>% 
  rename(use_count = n)

g_all2 <- graph_from_data_frame(df_gall$edges, 
                                vertices = df_gall$vertices)
summary(g_all2)


# Reduced version: Remove Redacted nodes and their edges
df_gall$vertices %>% 
  mutate(redact = (name_long == "Redacted")) %>% 
  group_by(redact) %>% 
  count()

vall_reduced <- df_gall$vertices %>% 
  filter(name_long != "Redacted")

eall_reduced <- df_gall$edges %>% 
  filter(from %in% vall_reduced$name & to %in% vall_reduced$name)

g_all3 <- graph_from_data_frame(eall_reduced, 
                                directed = FALSE, 
                                vertices = vall_reduced)
summary(g_all3) 


# Save a copy of the igraph object
g_all <- g_all3
save(g_all, file = "networks/tags_ship_Fair-game.Rda")


# Save node and edge data frames
#write_csv(vall_reduced, file = "data/vertices_ship_Ironqrow.csv")
#write_csv(eall_reduced, file = "data/edges_ship_Ironqrow.csv")

