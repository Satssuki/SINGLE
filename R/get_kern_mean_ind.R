get_kern_mean_ind <-
function(ID, data, radius=1){
  # calculate kernel mean for a given data point based on Gaussian kernel with specified radius
  x = seq(1:nrow(data))
  norm_ = my_exp_kern(ID,x,radius)
  
  mean_ = apply(data * norm_,2,FUN=function(x){sum(x)/sum(norm_)})
  
  return(mean_)
}
