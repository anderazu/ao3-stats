# Read in saved/tagged works info for a fandom and build a tag network.


library(tidyverse)
library(igraph)


# Import data
(load("data/works_RWBY.Rda"))
(load("data/tags_RWBY.Rda"))
#(load("data/works_MTAP.Rda"))
#(load("data/tags_MTAP.Rda"))

wtagged <- wtagged_fandom
rm(wtagged_fandom)


## Exploratory looks

# Tag type frequency
wtagged %>% group_by(type) %>% 
  count() %>% 
  mutate(frac = n / nrow(wtagged))

# Looking by category
wtagged %>% filter(type == "Category")

wtagged %>% 
  filter(type == "Category") %>%   # number of category tags per work 
  group_by(wid) %>% count()

wtagged %>%     # number of category tags per work (summarize)
  group_by(wid) %>% 
  transmute(ncat = sum(type == "Category")) %>% # number of category tags
  distinct() %>%   # cut to unique rows
  group_by(ncat) %>% 
  count() %>% 
  mutate(frac = n / nrow(wred))


# Peek at the top
wtagged %>% 
  filter(type == "Relationship")

# See most-frequent relationship tags
wtagged %>% 
  filter(type == "Relationship") %>% 
  group_by(name) %>% 
  count(sort = TRUE)

# Uncommon relationship tags
wtagged %>% 
  filter(type == "Relationship", name == "Redacted")

# Individual tag counts
tag_counts <- wtagged %>% 
  group_by(tag_list) %>% 
  count()

# Add tag counts (in this fandom's works) to tag list
tred <- tred %>% 
  left_join(tag_counts, by = c("id" = "tag_list"))


## Bipartite network from works/tags

# Edge data frame from tagged works list
df_all <- wtagged %>% 
  select(wid, tag_list:name, word_count) %>% 
  mutate(wid = paste0("W", as.character(wid)), 
         tag_list = as.character(tag_list))

df_rel <- df_all %>% 
  filter(type == "Relationship") %>% 
  select(-type)


## Build igraph object -- relationships only

# Before merging tags, gb_rel had 28843 nodes; after, it has 26384
gb_rel <- df_rel %>% 
  graph_from_data_frame()

summary(gb_rel)

# Set node type
vertex_attr(gb_rel, "type") <- V(gb_rel)$name %in% pull(df_rel, var = wid)
table(V(gb_rel)$type)

# Bipartite projection
bipartite_projection_size(gb_rel)

# Before tag merger, g_rel had 7759 nodes; after, it has 5307
ptm <- proc.time()
g_rel <- bipartite_projection(gb_rel, which = FALSE)
(proc.time() - ptm)   # about 30 seconds


# Match node attributes as data frame
df_grel <- g_rel %>% 
  igraph::as_data_frame(what = "both") %>% 
  map(as_tibble)

df_grel$vertices
df_grel$edges

tagsumm <- df_rel %>% 
  select(-wid) %>% 
  group_by(tag_list) %>% 
  summarize(name = unique(name), 
            word_count = sum(word_count, na.rm = TRUE)) %>% 
  arrange(as.numeric(tag_list)) 

# Match in longer tag name and wordcount, add use count from tred frame
df_grel$vertices <- df_grel$vertices %>% 
  left_join(tagsumm, by = c("name" = "tag_list")) %>% 
  rename(name_long = name.y) %>% 
  left_join(tred %>% select(id, n) %>% mutate(id = as.character(id)), 
            by = c("name" = "id")) %>% 
  rename(use_count = n)

g_rel2 <- graph_from_data_frame(df_grel$edges, 
                                vertices = df_grel$vertices)
summary(g_rel2)

# Reduced version: Remove Redacted nodes and their edges
vrel_reduced <- df_grel$vertices %>% 
  filter(name_long != "Redacted")

erel_reduced <- df_grel$edges %>% 
  mutate(fmatch = from %in% vrel_reduced$name, 
         tmatch = to %in% vrel_reduced$name) %>% 
  filter(fmatch & tmatch) %>% 
  select(-fmatch, -tmatch)

g_rel3 <- graph_from_data_frame(erel_reduced, 
                                directed = FALSE, 
                                vertices = vrel_reduced)
summary(g_rel3)

# Save node and edge data frames
#write_csv(df_grel$vertices, file = "data/vertices_RWBY_rel.csv")
#write_csv(df_grel$edges, file = "data/edges_RWBY_rel.csv")
write_csv(vrel_reduced, file = "data/vertices_RWBY_rel.csv")
write_csv(erel_reduced, file = "data/edges_RWBY_rel.csv")


## Build igraph object -- all tags

gb_all <- df_all %>% 
  graph_from_data_frame()
summary(gb_all)

# Set node type
vertex_attr(gb_all, "type") <- V(gb_all)$name %in% pull(df_all, var = wid)
table(V(gb_all)$type)

# Bipartite projection
bipartite_projection_size(gb_all)

# Make bipartite projections

# Buckle up RAM, this will hurt
rm(gb_rel)
rm(wtagged)
# Get memory errors if we run this straight. Solution courtesy of: 
#  https://www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

# Try getting just the first (AKA tags, type = FALSE) projection
ptm <- proc.time()
g_all <- bipartite_projection(gb_all, which = "false")
(proc.time() - ptm)   # only 5 seconds for the tag projection, really?

save(g_all, file = "networks/tags_project_RWBY.Rda")


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
  left_join(tred %>% select(id, n) %>% mutate(id = as.character(id)), 
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
  mutate(fmatch = from %in% vall_reduced$name, 
         tmatch = to %in% vall_reduced$name) %>% 
  filter(fmatch & tmatch) %>% 
  select(-fmatch, -tmatch)

g_all3 <- graph_from_data_frame(eall_reduced, 
                                directed = FALSE, 
                                vertices = vall_reduced)
summary(g_all3) # cuts to 27K nodes


# Save node and edge data frames
write_csv(df_gall$vertices, file = "data/vertices_RWBY.csv")
write_csv(df_gall$edges, file = "data/edges_RWBY.csv")

write_csv(vall_reduced, file = "data/vertices_RWBY_reduced.csv")
write_csv(eall_reduced, file = "data/edges_RWBY_reduced.csv")


## CODE BELOW IS ONLY SEMI-UPDATED
## Check out the igraph object

# Degree distribution
deg_all <- tibble(name = V(g_all3)$name, deg = degree(g_all3)) 

deg_all %>% group_by(deg) %>% count()

deg_all %>% ggplot(aes(x = deg)) + 
  geom_histogram(bins = 40) + 
  scale_y_log10() +
  scale_x_log10()

# Look at isolates
deg_all %>% filter(deg == 0) %>% 
  left_join(df_gall$vertices)

# Remove isolates
g3 <- delete_vertices(g2, which(degree(g2) == 0)) 

# Plot that shit
ptm <- proc.time()
plot(g3, vertex.size = 7, vertex.label = NA)
(proc.time() - ptm)   # about 20 seconds


# Save a copy (wanted to use in Gephi, ditching for Cytoscape)
#vertex_attr(g3, "Label") <- V(g3)$name_long
#write_graph(g3, file = "data/nw_relationship_RWBY.graphml", 
#            format = "graphml")
#write_graph(g3, file = "data/nw_relationship_RWBY.gml", 
#            format = "gml")
