SINGLE <-
function(data, C=NULL, h, l1, l2, tol=0.01){
  # This algorithm first estimates covariances incrementally using a fixed FF
  # It stores all covariances and then applies a Fused Lasso penalty to all of them.
  # this yields sparse estimates (glasso penalty) which are smooth (fused lasso penalty)
  # Input:
  #     - data is multivariate data
  #     - C: list of estimated covariance matrices. Must provide one of either data or C
  #     - h is the radius for the GAUSSIAN KERNEL
  #     - l1 is Glasso penalty coefficient
  #     - l2 is fused lasso penalty coefficient
  
  # -- Kernel Estimation of Covariance Matricies -- #
  
  if (missing(C)){
    C = get_kern_cov(data=data, h=h)
  } else {
  cat("Sample covariance matrices provided\n")
  }
  C_ = vector("list", nrow(data))
  for (i in 1:nrow(data)){ # this is incredibly inefficient but it only needs to be done once
    C_[[i]]=C[,,i]
  }
  
  # -- Estimate of Precision Matricies -- #
  
  result = jgl_fgl_offline(S=C_, lambda1=l1, lambda2=l2, rho=1, obs=rep(1, length(C_)), max_iter=500, tol=tol)
  precision = result$Z
  
  # make into an array:
  precision_ = array(unlist(precision), c(ncol(precision[[1]]), ncol(precision[[1]]), length(precision)))
  
  # estimate likelihood, AIC, BIC
  
  # estimate number of free parameters (k)
  # we estimate these as the number of UNIQUE estimates for partial correlations over time
  k = 0
  for (i in 1:(ncol(data)-1)){
    for (j in (i+1):ncol(data)){
      k = k + length(unique(precision_[i,j,]))
    }
  }
#   k = (sum(P_!=0) - dim(P_)[3] * dim(P_)[1])/2
  if (!missing(h)){
    data = data - get_kern_mean(data, h=h) # remove the mean - will make life easier
    # note that if sample covariance matrices provided we cannot remove the mean from data as we may not know h!!
   }
  
  L = sapply(1:nrow(data), FUN=function(i){log_lik(data[i,], precision_[,,i])})
  
  AIC = 2*k - 2*sum(L)
  BIC = k*log(nrow(data)) - 2 * sum(L)
  
  
  return(list(P=precision, P_=precision_, C=C_, AIC=AIC, BIC=BIC))
}
