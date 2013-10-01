SINGLE <-
function(data, radius, l1, l2, k, tol=0.01){
  # This algorithm first estimates covariances incrementally using a fixed FF
  # It stores all covariances and then applies a Fused Lasso penalty to all of them.
  # this yields sparse estimates (glasso penalty) which are smooth (fused lasso penalty)
  # Input:
  #     - data is multivariate data
  #     - radius is the radius for the GAUSSIAN KERNEL
  #     - l1 is Glasso penalty coefficient
  #     - l2 is fused lasso penalty coefficient
  #     - k is the window size!
  
  # -- Kernel Estimation of Covariance Matricies -- #
  
  C = get_kern_cov(data=data, radius=radius)
  C_ = vector("list", nrow(data))
  for (i in 1:nrow(data)){ # this is incredibly inefficient but it only needs to be done once
    C_[[i]]=C[,,i]
  }
  
  # -- Estimate of Precision Matricies -- #
  
  result = jgl_fgl_offline(S=C_, lambda1=l1, lambda2=l2, window=k, rho=1, obs=rep(1, length(C_)), max_iter=500, tol=tol)
  precision = result$Z
  
  # make into an array:
  precision_ = array(unlist(precision), c(ncol(precision[[1]]), ncol(precision[[1]]), length(precision)))
  
  single_object = list(P=precision, P_=precision_, C=C_)
  
  class(single_object) = "SINGLE"
  
  return(single_object)
}
