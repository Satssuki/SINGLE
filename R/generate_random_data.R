generate_random_data <-
function(ROI, length_, seg, sparsity, str=0.6){
  # This function generates random graphical structure 
  # Input:
  #   - ROI is number of regions of interest (ie number of nodes)
  #   - length_ is the length of each segment
  #   - seg is number of change segments (each with its own correlation structure)
  #   - sparsity is the level of sparsity
  # Output:
  #   - data = VAR process of dimension ROI x (length_*chgpts)
  #   - true_cov = array with true graphical structure
  
  # small hack (to avoid changing my previous code too much)
  chgpts = seg
  
  # empty true correlation structure (fill this in)
  true_cov = true_cov = array(0, c(ROI,ROI, length_*chgpts))
  data = matrix(0, length_*chgpts, ROI)
  for (i in 1:chgpts){
    X = generate_random_graph(ROI=ROI, length_=length_, sparsity=sparsity, str=str)
    data[((i-1)*length_+1):(i*length_),] = X$data
    true_cov[,,((i-1)*length_+1):(i*length_)] = X$true_cov
  }
  
  return(list(data=data, true_cov=true_cov))  
  
}
