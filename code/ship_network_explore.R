# Load and explore tag co-occurrence network for a ship


library(tidyverse)
library(igraph)
library(disparityfilter)  # for network reduction


# Import data
#load("networks/tags_ship_Ironqrow.Rda")
load("networks/tags_ship_Fair-game.Rda")

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

edges <- df$edges

# Frequency distributions of tags
verts %>% ggplot(aes(x = type, y = deg)) + 
  geom_violin() + 
  scale_y_log10()


## Remove tags I don't care about

# ArchiveWarning, Category, and Rating tags, plus current fandom
vred <- verts %>% 
  filter(!(type %in% c("ArchiveWarning", "Category", "Rating"))) %>%
  filter(!(name_long == "RWBY"))

# Find and remove other tags that don't interest me
vred %>% filter(grepl("to Be Added", x = name_long) | 
                  grepl("Beta Read", x = name_long) | 
                  grepl("Spoilers", x = name_long)) 
vred <- vred %>% 
  filter(!grepl("to Be Added", x = name_long) & 
           !grepl("Beta Read", x = name_long) &
           !grepl("Spoilers", x = name_long)) 

ered <- edges %>% 
  filter(from %in% vred$name & to %in% vred$name)

gred <- graph_from_data_frame(ered, directed = FALSE, vertices = vred)


# New degree frequency distributions
vred <- vred %>% bind_cols(degRed = degree(gred))

vred %>% filter(degRed == 0)

vred %>% ggplot(aes(x = type, y = degRed)) + 
  geom_violin() + 
  scale_y_log10()


## Cut tags with low use_count

vhigh <- vred %>% 
  filter(use_count >= 5)
ehigh <- ered %>% 
  filter(from %in% vhigh$name & to %in% vhigh$name)
ghigh <- graph_from_data_frame(ehigh, directed = FALSE, vertices = vhigh)


vhigh10 <- vhigh %>% 
  filter(use_count >= 10)
ehigh10 <- ehigh %>% 
  filter(from %in% vhigh10$name & to %in% vhigh10$name)
ghigh10 <- graph_from_data_frame(ehigh10, directed = FALSE, vertices = vhigh10)


## Remove character and fandom tags

vfree <- vhigh %>% 
  filter(!(type == "Character" | type == "Fandom"))
efree <- ehigh %>% 
  filter(from %in% vfree$name & to %in% vfree$name)
gfree <- graph_from_data_frame(efree, directed = FALSE, vertices = vfree)

vfree10 <- vhigh10 %>% 
  filter(!(type == "Character" | type == "Fandom"))
efree10 <- ehigh10 %>% 
  filter(from %in% vfree10$name & to %in% vfree10$name)
gfree10 <- graph_from_data_frame(efree10, directed = FALSE, vertices = vfree10)

vfree15 <- vhigh %>% 
  filter(use_count >= 15) %>% 
  filter(!(type == "Character" | type == "Fandom"))
efree15 <- efree %>% 
  filter(from %in% vfree15$name & to %in% vfree15$name)
gfree15 <- graph_from_data_frame(efree15, directed = FALSE, vertices = vfree15)


# Save these, since I keep reusing them
save(gred, ghigh, gfree, file = "networks/tagNW_ship_Fair-game.Rda")


## Cut low-weight edges

ehwt2 <- ehigh %>% filter(weight >= 2)
vhwt2 <- vhigh %>% 
  filter(name %in% ehwt2$from | name %in% ehwt2$to)
ghwt2 <- graph_from_data_frame(ehwt2, directed = FALSE, vertices = vhwt2)

ehwt <- ehigh %>% filter(weight >= 4)
vhwt <- vhigh %>% 
  filter(name %in% ehwt$from | name %in% ehwt$to)
ghwt <- graph_from_data_frame(ehwt, directed = FALSE, vertices = vhwt)

efwt2 <- efree %>% filter(weight >= 2)
vfwt2 <- vfree %>% 
  filter(name %in% efwt2$from | name %in% efwt2$to)
gfwt2 <- graph_from_data_frame(efwt2, directed = FALSE, vertices = vfwt2)

efwt3 <- efree %>% filter(weight >= 3)
vfwt3 <- vfree %>% 
  filter(name %in% efwt3$from | name %in% efwt3$to)
gfwt3 <- graph_from_data_frame(efwt3, directed = FALSE, vertices = vfwt3)

efwt <- efree15 %>% filter(weight >= 8)
vfwt <- vfree15 %>% 
  filter(name %in% efwt$from | name %in% efwt$to)
gfwt <- graph_from_data_frame(efwt, directed = FALSE, vertices = vfwt)


## Collect some network statistics

#g <- g_all
source("code/functions_networks.R")

nwList <- list(g_all = g_all, gred = gred, 
               ghigh = ghigh, ghigh10 = ghigh10, 
               ghwt = ghwt, 
               gfree = gfree, gfree10 = gfree10, gfree15 = gfree15,
               gfwt = gfwt)

nwStats <- tibble(network = names(nwList), 
                  nwList %>% 
                    map(getNWstats) %>% 
                    bind_rows())

save(nwList, file = "networks/nwlist_ship_Fair-game.Rda")


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


#saveGraph(gfree, nwname = "gIronqrow_free", fandom = "RWBY")


## Save node and edge lists for Cytoscape

saveGraph(gfwt, nwname = "gFairgame_free_wt8", fandom = "RWBY")
