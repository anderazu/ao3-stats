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
ebbhigh <- backbone(ghigh, alpha = 0.1)
(proc.time() - ptm)   # about 


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

nwList <- list(g_all = g_all, 
               gred = gred, 
               ghigh = ghigh, 
               gfree = gfree)

nwStats <- tibble(network = names(nwList), 
                  nwList %>% 
                    map(getNWstats) %>% 
                    bind_rows())



# Save community information for smaller networks 
clusHigh <- cluster_fast_greedy(ghigh)


## Compare degree distributions

degList <- map(list(g_all, gred, ghigh), degree)
names(degList) <- nwStats[[1]]

degFrame <- degList %>% 
  map(enframe, "id", "degree") %>%  # turn named degree vectors into tibbles
  enframe() %>%    # turn list into nested data frame, degree tibbles in "value"
  unnest(cols = c(value))   # unnest data frame into long form

degFrame %>% ggplot(aes(x = degree, color = name)) + 
  geom_density() + 
  scale_x_log10() +
  ggtitle("Degree distributions for original and reduced networks")

