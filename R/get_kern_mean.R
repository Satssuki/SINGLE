get_kern_mean <-
function(data, radius=1){
  # calculate mean based on Gaussian kernel with some specified radius
  ID = matrix(seq(1:nrow(data)), ncol=1)
  kern_mean = apply(ID,1, FUN=get_kern_mean_ind, data=data, radius=radius)
  
  return(t(kern_mean))
}
