# Load and explore tag co-occurrence network


library(tidyverse)
library(igraph)


# Import data
#load("networks/tags_project_RWBY.Rda")
load("networks/tags_project_SDV.Rda")

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


## Remove tags I don't care about

# Remove ArchiveWarning, Category, and Rating tags, plus current fandom
vred <- verts %>% 
  filter(!(type %in% c("ArchiveWarning", "Category", "Rating"))) %>%
  filter(!(name_long == "Stardew Valley (Video Game)"))

# Find and remove other tags that don't interest me
vred %>% filter(grepl("to [bB]e [aA]dded", x = name_long) | 
                  grepl("[bB]eta [rR]ead", x = name_long) | 
                  grepl("[sS]poiler", x = name_long) | 
                  grepl("[pP]osted", x = name_long) | 
                  grepl("[tT]agging", x = name_long)) %>% 
  arrange(desc(use_count)) %>% 
  view()

vred <- vred %>% 
  filter(!grepl("to [bB]e [aA]dded", x = name_long) & 
           !grepl("[bB]eta [rR]ead", x = name_long) &
           !grepl("[sS]poiler", x = name_long) & 
           !grepl("[pP]osted", x = name_long) & 
           !grepl("[tT]agging", x = name_long)) 


ered <- edges %>% 
  filter(from %in% vred$name & to %in% vred$name)

gred <- graph_from_data_frame(ered, directed = FALSE, vertices = vred)


# New degree frequency distributions
vred <- vred %>% bind_cols(degRed = degree(gred))

vred %>% filter(degRed == 0)

vred %>% ggplot(aes(x = type, y = degRed)) + 
  geom_violin() + 
  scale_y_log10()


## Cut everything with low use_count

vhigh <- vred %>% 
  filter(use_count > 5)

ehigh <- ered %>% 
  filter(from %in% vhigh$name & to %in% vhigh$name)

ghigh <- graph_from_data_frame(ehigh, directed = FALSE, vertices = vhigh)


## Remove character and fandom tags

vfree <- vhigh %>% 
  filter(!(type == "Character" | type == "Fandom"))

efree <- ehigh %>% 
  filter(from %in% vfree$name & to %in% vfree$name)

gfree <- graph_from_data_frame(efree, directed = FALSE, vertices = vfree)


## Cut low-weight edges

ehwt <- efree %>% filter(weight >= 5)
vhwt <- vfree %>% 
  filter(name %in% ehwt$from | name %in% ehwt$to)
ghwt <- graph_from_data_frame(ehwt, directed = FALSE, vertices = vhwt)


## Collect some network statistics

#g <- g_all
source("code/functions_networks.R")

nwList <- list(g_all = g_all, gred = gred, 
               ghigh = ghigh, gfree = gfree, 
               ghwt = ghwt)

nwStats <- tibble(network = names(nwList), 
                  nwList %>% 
                    map(getNWstats) %>% 
                    bind_rows())
nwStats

save(nwList, file = "networks/nwlist_SDV.Rda")


## Compare degree distributions

degList <- map(nwList, degree)
names(degList) <- nwStats[[1]]

degFrame <- degList %>% 
  map(enframe, "id", "degree") %>%  # turn named degree vectors into tibbles
  enframe() %>%    # turn list into nested data frame, degree tibbles in "value"
  unnest(cols = c(value))   # unnest data frame into long form
degFrame$name <- factor(degFrame$name, levels = names(degList))

degFrame %>% ggplot(aes(x = degree, color = name)) + 
  geom_density() + 
  scale_x_log10() +
  ggtitle("Degree distributions for original and reduced networks")



## Compare edge weight distributions
ewList <- map(nwList, edge_attr, "weight")

ewFrame <- ewList %>% 
  map(enframe, "id", "weight") %>%  # turn named degree vectors into tibbles
  enframe() %>%    # turn list into nested data frame, degree tibbles in "value"
  unnest(cols = c(value))   # unnest data frame into long form
ewFrame$name <- factor(ewFrame$name, levels = names(ewList))

ewFrame %>% ggplot(aes(x = weight, color = name)) + 
  geom_density() + 
  scale_y_log10() +
  scale_x_log10() +
  ggtitle("Edge weight distributions for original and reduced networks")

ewFrame %>% count(name, weight) %>% 
  filter(!name == "gred") %>% 
  ggplot(aes(x = weight, color = name)) + 
  geom_point(aes(y = n)) + 
  scale_y_log10() +
  scale_x_log10() +
  ggtitle("Edge weight distributions for original and reduced networks")



# Community detection
g <- gfwt
cl_fg <- cluster_fast_greedy(g)
cl_im <- cluster_infomap(g)
cl_eb <- cluster_edge_betweenness(g)

vertex_attr(gfwt, "clusFG") <- membership(cl_fg)
vertex_attr(gfwt, "clusEB") <- membership(cl_eb)

sizes(cl_fg)
sizes(cl_eb)


saveGraph(gego, nwname = "gIronqrow", fandom = "RWBY")


## Save node and edge lists for Cytoscape

source("code/functions_networks.R")

saveGraph(gh10wt5, nwname = "gh10wt5", fandom = "RWBY")
