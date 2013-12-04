choose_h <-
function(data, sample_size, kernel='gaussian', h_lower=5, h_upper=95, h_step=10){
  # estimate h using LOO likelihood
  h = seq(h_lower, h_upper, h_step)
  samples = sample(2:nrow(data), sample_size)
  results = unlist(lapply(h, FUN=function(x){est_log_like_LOO_samplesGiven(data, x, samples, kernel=kernel)}))
  
  return(h[which(results==max(results))])
}
