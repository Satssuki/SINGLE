get_kern_cov_ind <-
function(data, ID, radius=1, centered=FALSE){
  # calculate kernel covariance for a given data point based on Gassiand kernel with specified radius
  
  # center data first (if needed):
  if (centered==FALSE){
    data = data - get_kern_mean(data, radius=radius)    
  }

  x = seq(1:nrow(data))
  norm_ = my_exp_kern(ID,x,radius)
  data = data * sqrt(norm_)
  
  cov_ = matrix(apply(apply(data, 1, FUN=function(x){x%*%t(x)}),1,sum), ncol=ncol(data))
 
  return(cov_/sum(norm_))
  
 # cov_ = matrix(apply(apply(data, 1, FUN=function(x){x%*%t(x)}), 1, FUN=function(x){sum(x)/sum(norm_)}), ncol=ncol(data)) 
}
