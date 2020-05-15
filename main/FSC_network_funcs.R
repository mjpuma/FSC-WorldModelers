## Functions to compute network statistics for food 

# https://www.r-bloggers.com/network-centrality-in-r-an-introduction/
all_indices <- function(g){
  res <- matrix(0,vcount(g),35)
  res[,1] <- igraph::degree(g)
  res[,2] <- igraph::betweenness(g)
  res[,3] <- igraph::closeness(g)
  res[,4] <- igraph::eigen_centrality(g)$vector
  res[,5] <- 1/igraph::eccentricity(g)
  res[,6] <- igraph::subgraph_centrality(g)
  
  A <- get.adjacency(g,sparse=F)
  res[,7] <- sna::flowbet(A)
  res[,8] <- sna::loadcent(A)
  res[,9] <- sna::gilschmidt(A)
  res[,10] <- sna::infocent(A)
  res[,11] <- sna::stresscent(A)
  
  res[,12] <- 1/centiserve::averagedis(g)
  res[,13] <- centiserve::barycenter(g)
  res[,14] <- centiserve::closeness.currentflow(g)
  res[,15] <- centiserve::closeness.latora(g)
  res[,16] <- centiserve::closeness.residual(g)
  res[,17] <- centiserve::communibet(g)
  res[,18] <- centiserve::crossclique(g)
  res[,19] <- centiserve::decay(g)
  res[,20] <- centiserve::diffusion.degree(g)     
  res[,21] <- 1/centiserve::entropy(g)
  res[,22] <- centiserve::geokpath(g)
  res[,23] <- centiserve::katzcent(g)             
  res[,24] <- centiserve::laplacian(g)
  res[,25] <- centiserve::leverage(g)             
  res[,26] <- centiserve::lincent(g)
  res[,27] <- centiserve::lobby(g)
  res[,28] <- centiserve::markovcent(g)           
  res[,29] <- centiserve::mnc(g)
  res[,30] <- centiserve::radiality(g)            
  res[,31] <- centiserve::semilocal(g)
  res[,32] <- 1/centiserve::topocoefficient(g) 
  
  res[,33] <- CINNA::dangalchev_closeness_centrality(g)
  res[,34] <- CINNA::harmonic_centrality(g)
  res[,35] <- 1/CINNA::local_bridging_centrality(g)
  apply(res,2,function(x) round(x,8))
}