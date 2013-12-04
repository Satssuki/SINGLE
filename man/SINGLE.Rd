\name{SINGLE}
\alias{SINGLE}
\title{
Smooth Incremental Graphical Lasso Estimation (SINGLE)
}
\description{
Estimate sparse dynamic graphs using the Smooth Incremental Graphical Lasso Estimation (SINGLE) algorithm
}
\usage{
SINGLE(data, C = NULL, h, l1, l2, tol = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Matrix of observations where each row is a vector of observations and each column corresponds to a node.
}
  \item{C}{
List of estimating sample covariance matrices. If this is supplied then \code{data} is ignored. If \code{C} is null then sample covariances will be estimated from \code{data}.
This allows us to run the SINGLE algorithm for sample covariance matrices estimated in any manner.
}
  \item{h}{
Width of the Gaussian kernel. Will be ignored is \code{C} is provided.
}
  \item{l1}{
Value for the \eqn{\lambda_1} penalty (i.e., sparsity penalty) in the SINGLE algorithm.
}
  \item{l2}{
Value for the \eqn{\lambda_2} penalty (i.e., homogeneity penalty) in the SINGLE algorithm.
}
  \item{tol}{
Tolerance to determine when the algorithm has converged.
}
}
\details{
The SINGLE algorithm minimises the following objective function:
\deqn{
 f(\{ \Theta \}) =  \sum_{i=1}^T  [ -log det \hat \Theta_i + trace ( S_i \Theta_i) ] + \lambda_1 \sum_{i=1}^T || \Theta_i||_1 + \lambda_2 \sum_{i=2}^T || \Theta_i -  \Theta_{i-1}||_1
}
where \eqn{\{  \Theta \}} is the set of all precision matrices indexed over time.
}
\value{
\item{P}{List where the ith element is the ith estimated precision matrix}
\item{P_}{An array where \code{P_[,,i]} is the estimated precision matrix for the ith observation}
\item{C_}{List where the ith element is the ith estimated covariance matrix}
}

\references{
Monti R. P., Hellyer P., Sharp D., Leech R., Anagnostopoulos C. and Montana G. (2013) Estimating Dynamic Brain Connectivity Networks from Functional MRI Time Series
}
\author{
Ricardo Pio Monti
}\seealso{
\code{\link{plotSINGLE}, \link{generate_random_data}, \link{precision_recall}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.1, str=0.6)
\dontrun{
S = SINGLE(data=X$data, h=50, l1=0.7, l2=0.5)
result = precision_recall(true_cov=X$true_cov, estimated_cov=S$P_)
plot(result$F1, type='l', ylim=c(0,1), ylab='', 
   main='F1 Score', xlab='Time') # plot F1 score over time
}
}
\keyword{ SINGLE }