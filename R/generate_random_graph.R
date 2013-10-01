generate_random_graph <-
function(ROI, length_, sparsity, str=0.6){
  # this function generates 1 segment of graphically structured data using Erdos-Renyi random graphs
  # Input:
  #   - ROI is number of regions of interest (ie number of nodes)
  #   - length is the length of each segment
  #   - sparsity is the level of sparsity
  #   - str is strength of correlation
  # Output:
  #   - data = VAR process of dimension ROI x length
  #   - true_cov = array with true graphical structure
  y = as.matrix(get.adjacency(erdos.renyi.game(ROI, sparsity), sparse=FALSE))
  y = fill_in_edges(y)
  y = y*str * (-1)
  diag(y)=1
  
  if (sum(eigen(y)$values>=0)<ROI){
    while(sum(eigen(y)$values>=0)<ROI){
      y = as.matrix(get.adjacency(erdos.renyi.game(ROI, sparsity)))
      y = fill_in_edges(y)
      y = y*str * (-1)
      diag(y)=1
    }
  }
  true_cov = array(y, c(1,ROI,ROI))
  VAR = ARMA(A=true_cov, B=diag(ROI))
  data = simulate(VAR, sampleT=length_)$output
  
  y2 = y * -1
  diag(y2)=1
  true_cov2 = array(y2, c(1,ROI,ROI))
  
  return(list(data=data, true_cov=true_cov2))  
}
