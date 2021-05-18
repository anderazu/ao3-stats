# Load tag co-occurrence network and cut to various backbones


library(tidyverse)
library(igraph)
library(disparityfilter)  # for network reduction


# Import data
load("networks/tags_project_RWBY.Rda")

summary(g_all)


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

ptm <- proc.time()
ebbfree_75 <- backbone(gfree, alpha = 0.75)
(proc.time() - ptm)   # about 5 hours 40 minutes
save(ebbfree_75, file = "networks/ebbfree_75.Rda")

ptm <- proc.time()
ebbfree_90 <- backbone(gfree, alpha = 0.90)
(proc.time() - ptm)   # about 5 hours 40 minutes
save(ebbfree_90, file = "networks/ebbfree_90.Rda")

ptm <- proc.time()
ebbfree_95 <- backbone(gfree, alpha = 0.95)
(proc.time() - ptm)   # about 6 hours 30 minutes
save(ebbfree_95, file = "networks/ebbfree_95.Rda")

ptm <- proc.time()
ebbfree_99 <- backbone(gfree, alpha = 0.99)
(proc.time() - ptm)   # about 6 hours 30 minutes
save(ebbfree_99, file = "networks/ebbfree_99.Rda")

ptm <- proc.time()
ebbfree_999 <- backbone(gfree, alpha = 0.999)
(proc.time() - ptm)   # about 6 hours 30 minutes
save(ebbfree_999, file = "networks/ebbfree_999.Rda")

save(ebbfree_05, ebbfree_10, ebbfree_25, ebbfree_50, ebbfree_75, 
     ebbfree_90, ebbfree_95, ebbfree_99, ebbfree_999, 
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

vbbfree_75 <- vfree %>% 
  filter(name %in% ebbfree_75$from | name %in% ebbfree_75$to)

vbbfree_90 <- vfree %>% 
  filter(name %in% ebbfree_90$from | name %in% ebbfree_90$to)

vbbfree_95 <- vfree %>% 
  filter(name %in% ebbfree_95$from | name %in% ebbfree_95$to)

vbbfree_99 <- vfree %>% 
  filter(name %in% ebbfree_99$from | name %in% ebbfree_99$to)

vbbfree_999 <- vfree %>% 
  filter(name %in% ebbfree_999$from | name %in% ebbfree_999$to)

gbbfree_05 <- graph_from_data_frame(ebbfree_05, vertices = vbbfree_05)
gbbfree_10 <- graph_from_data_frame(ebbfree_10, vertices = vbbfree_10)
gbbfree_25 <- graph_from_data_frame(ebbfree_25, vertices = vbbfree_25)
gbbfree_50 <- graph_from_data_frame(ebbfree_50, vertices = vbbfree_50)
gbbfree_75 <- graph_from_data_frame(ebbfree_75, vertices = vbbfree_75)
gbbfree_90 <- graph_from_data_frame(ebbfree_90, vertices = vbbfree_90)
gbbfree_95 <- graph_from_data_frame(ebbfree_95, vertices = vbbfree_95)
gbbfree_99 <- graph_from_data_frame(ebbfree_99, vertices = vbbfree_99)
gbbfree_999 <- graph_from_data_frame(ebbfree_999, vertices = vbbfree_999)


## Save community information for smaller networks 
cl_high <- cluster_fast_greedy(ghigh)
cl_free <- cluster_fast_greedy(gfree)
cl_im_free999 <- cluster_infomap(gbbfree_999)
cl_im_free99 <- cluster_infomap(gbbfree_99)
cl_im_free95 <- cluster_infomap(gbbfree_95)
cl_im_free90 <- cluster_infomap(gbbfree_90)
cl_im_free75 <- cluster_infomap(gbbfree_75)
cl_im_free50 <- cluster_infomap(gbbfree_50)
cl_im_free25 <- cluster_infomap(gbbfree_25)
cl_im_free10 <- cluster_infomap(gbbfree_10)
cl_im_free05 <- cluster_infomap(gbbfree_05)

cl_wt_free75 <- cluster_walktrap(gbbfree_75)
cl_wt_free50 <- cluster_walktrap(gbbfree_50)
cl_wt_free25 <- cluster_walktrap(gbbfree_25)
cl_wt_free10 <- cluster_walktrap(gbbfree_10)
cl_wt_free05 <- cluster_walktrap(gbbfree_05)

save(cl_im_free75, cl_im_free50, cl_im_free25, cl_im_free10, cl_im_free05, 
     cl_wt_free75, cl_wt_free50, cl_wt_free25, cl_wt_free10, cl_wt_free05, 
     file = "networks/cluster_bbfree.Rda")


# Add community info to graphs and save

vertex_attr(gbbfree_75, "clusIM") <- membership(cl_im_free75)
vertex_attr(gbbfree_75, "clusWT") <- membership(cl_wt_free75)

vertex_attr(gbbfree_50, "clusIM") <- membership(cl_im_free50)
vertex_attr(gbbfree_50, "clusWT") <- membership(cl_wt_free50)

vertex_attr(gbbfree_25, "clusIM") <- membership(cl_im_free25)
vertex_attr(gbbfree_25, "clusWT") <- membership(cl_wt_free25)

vertex_attr(gbbfree_10, "clusIM") <- membership(cl_im_free10)
vertex_attr(gbbfree_10, "clusWT") <- membership(cl_wt_free10)

vertex_attr(gbbfree_05, "clusIM") <- membership(cl_im_free05)
vertex_attr(gbbfree_05, "clusWT") <- membership(cl_wt_free05)

save(gbbfree_05, gbbfree_10, gbbfree_25, gbbfree_50, gbbfree_75, 
     file = "networks/gbbfree.Rda")


## Calculate summary statistics

source("code/functions_networks.R")

nwList <- list(g_all = g_all, gred = gred, 
               ghigh = ghigh, gfree = gfree, 
               gbbfree_999 = gbbfree_999, 
               gbbfree_99 = gbbfree_99, 
               gbbfree_95 = gbbfree_95, 
               gbbfree_90 = gbbfree_90, 
               gbbfree_75 = gbbfree_75, 
               gbbfree_50 = gbbfree_50, 
               gbbfree_25 = gbbfree_25, 
               gbbfree_10 = gbbfree_10, 
               gbbfree_05 = gbbfree_05)

nwStats <- tibble(network = names(nwList), 
                  nwList %>% 
                    map(getNWstats) %>% 
                    bind_rows())


## Save node and edge lists for Cytoscape

saveGraph <- function(g, nwname = NULL, fandom = NULL, dir = "networks/") {
  df <- g %>% 
    igraph::as_data_frame(what = "both") %>% 
    map(as_tibble)
  
  vfile <- paste0(dir, "vertices_", nwname, "_", fandom, ".csv")
  efile <- paste0(dir, "edges_", nwname, "_", fandom, ".csv")
  
  write_csv(df$vertices, file = vfile)
  write_csv(df$edges, file = efile)
}

saveGraph(gbbfree_50, nwname = "gbbfree_50", fandom = "RWBY")
saveGraph(gbbfree_25, nwname = "gbbfree_25", fandom = "RWBY")
saveGraph(gbbfree_10, nwname = "gbbfree_10", fandom = "RWBY")
saveGraph(gbbfree_05, nwname = "gbbfree_05", fandom = "RWBY")


