# Calculate a data frame of statistics for an igraph object (use with map)
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

# Save node and edge lists for plotting
saveGraph <- function(g, nwname = NULL, fandom = NULL, dir = "networks/") {
  df <- g %>% 
    igraph::as_data_frame(what = "both") %>% 
    map(as_tibble)
  
  vfile <- paste0(dir, "vertices_", nwname, "_", fandom, ".csv")
  efile <- paste0(dir, "edges_", nwname, "_", fandom, ".csv")
  
  write_csv(df$vertices, file = vfile)
  write_csv(df$edges, file = efile)
}
