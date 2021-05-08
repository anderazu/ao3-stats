# Load and explore tag co-occurrence network


library(tidyverse)
library(igraph)
library(disparityfilter)  # for network reduction


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
  mutate(fmatch = from %in% vhigh$name, 
         tmatch = to %in% vhigh$name) %>% 
  filter(fmatch & tmatch) %>% 
  select(-fmatch, -tmatch)

ghigh <- graph_from_data_frame(ehigh, directed = FALSE, vertices = vhigh)


## Remove character and fandom tags

vfree <- vhigh %>% 
  filter(!(type == "Character" | type == "Fandom"))

efree <- ehigh %>% 
  mutate(fmatch = from %in% vfree$name, 
         tmatch = to %in% vfree$name) %>% 
  filter(fmatch & tmatch) %>% 
  select(-fmatch, -tmatch)

gfree <- graph_from_data_frame(efree, directed = FALSE, vertices = vfree)


## Apply disparity filter to get backbone

# Increase available memory
# (https://www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R)
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

ptm <- proc.time()
ebbfree_10 <- backbone(gfree, alpha = 0.1)
(proc.time() - ptm)   # about 5 hours 40 minutes?
save(ebbfree, file = "networks/ebbfree_10.Rda")

ptm <- proc.time()
ebbfree_50 <- backbone(gfree, alpha = 0.5)
(proc.time() - ptm)   # about 5 hours 40 minutes
save(ebbfree_50, file = "networks/ebbfree_50.Rda")

ptm <- proc.time()
ebbfree_25 <- backbone(gfree, alpha = 0.25)
(proc.time() - ptm)   # about 5 hours 40 minutes
save(ebbfree_25, file = "networks/ebbfree_25.Rda")

ptm <- proc.time()
ebbfree_05 <- backbone(gfree, alpha = 0.05)
(proc.time() - ptm)   # about 5 hours 40 minutes
save(ebbfree_05, file = "networks/ebbfree_05.Rda")

save(ebbfree_05, ebbfree_10, ebbfree_25, ebbfree_50, 
     file = "networks/ebbfree.Rda")


## Turn the backbone edge lists into networks

vbbfree_05 <- vfree %>% 
  filter(name %in% ebbfree_05$from | name %in% ebbfree_05$to)

vbbfree_10 <- vfree %>% 
  filter(name %in% ebbfree_10$from | name %in% ebbfree_10$to)

vbbfree_25 <- vfree %>% 
  filter(name %in% ebbfree_25$from | name %in% ebbfree_25$to)

vbbfree_50 <- vfree %>% 
  filter(name %in% ebbfree_50$from | name %in% ebbfree_50$to)

gbbfree_05 <- graph_from_data_frame(ebbfree_05, vertices = vbbfree_05)
gbbfree_10 <- graph_from_data_frame(ebbfree_10, vertices = vbbfree_10)
gbbfree_25 <- graph_from_data_frame(ebbfree_25, vertices = vbbfree_25)
gbbfree_50 <- graph_from_data_frame(ebbfree_50, vertices = vbbfree_50)


## Save community information for smaller networks 
cl_high <- cluster_fast_greedy(ghigh)
cl_free <- cluster_fast_greedy(gfree)
cl_im_free50 <- cluster_infomap(gbbfree_50)
cl_im_free25 <- cluster_infomap(gbbfree_25)
cl_im_free10 <- cluster_infomap(gbbfree_10)
cl_im_free05 <- cluster_infomap(gbbfree_05)
cl_wt_free50 <- cluster_walktrap(gbbfree_50)
cl_wt_free25 <- cluster_walktrap(gbbfree_25)
cl_wt_free10 <- cluster_walktrap(gbbfree_10)
cl_wt_free05 <- cluster_walktrap(gbbfree_05)

save(cl_im_free50, cl_im_free25, cl_im_free10, cl_im_free05, 
     cl_wt_free50, cl_wt_free25, cl_wt_free10, cl_wt_free05, 
     file = "networks/cluster_bbfree.Rda")


# Add community info to graphs and save

vertex_attr(gbbfree_50, "clusIM") <- membership(cl_im_free50)
vertex_attr(gbbfree_50, "clusWT") <- membership(cl_wt_free50)

vertex_attr(gbbfree_25, "clusIM") <- membership(cl_im_free25)
vertex_attr(gbbfree_25, "clusWT") <- membership(cl_wt_free25)

vertex_attr(gbbfree_10, "clusIM") <- membership(cl_im_free10)
vertex_attr(gbbfree_10, "clusWT") <- membership(cl_wt_free10)

vertex_attr(gbbfree_05, "clusIM") <- membership(cl_im_free05)
vertex_attr(gbbfree_05, "clusWT") <- membership(cl_wt_free05)

save(gbbfree_05, gbbfree_10, gbbfree_25, gbbfree_50, 
     file = "networks/gbbfree.Rda")


## Collect some network statistics

#g <- g_all
getNWstats <- function(g) {
  nNodes <- vcount(g)
  nIso <- sum(degree(g) == 0)
  nEff <- nNodes - nIso
  nEdges <- ecount(g)
  weight <- sum(E(g)$weight)
  #dens <- edge_density(g)
  avgDeg <- mean(degree(g))
  #ccGlobal <- transitivity(g, type = "global")
  #ccLocal <- transitivity(g, type = "weighted")
  assort <- assortativity_degree(g, directed = FALSE)
  df <- tibble(nEff, nIso, nNodes, nEdges, weight, #dens,
               avgDeg, #ccGlobal, ccLocal, 
               assort)
  return(df)
}

nwList <- list(g_all = g_all, gred = gred, 
               ghigh = ghigh, gfree = gfree, 
               gbbfree_50 = gbbfree_50, 
               gbbfree_25 = gbbfree_25, 
               gbbfree_10 = gbbfree_10, 
               gbbfree_05 = gbbfree_05)

nwStats <- tibble(network = names(nwList), 
                  nwList %>% 
                    map(getNWstats) %>% 
                    bind_rows())




## Compare degree distributions

degList <- map(nwList, degree)
names(degList) <- nwStats[[1]]

degFrame <- degList %>% 
  map(enframe, "id", "degree") %>%  # turn named degree vectors into tibbles
  enframe() %>%    # turn list into nested data frame, degree tibbles in "value"
  unnest(cols = c(value))   # unnest data frame into long form

degFrame %>% ggplot(aes(x = degree, color = name)) + 
  geom_density() + 
  scale_x_log10() +
  ggtitle("Degree distributions for original and reduced networks")

