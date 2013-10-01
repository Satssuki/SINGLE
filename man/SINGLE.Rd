\name{SINGLE}
\alias{SINGLE}
\title{
Smooth Incremental Graphical Lasso Estimation (SINGLE)
}
\description{
Estimate sparse dynamic graphs using the Smooth Incremental Graphical Lasso Estimation (SINGLE) algorithm
}
\usage{
SINGLE(data, radius, l1, l2, k, tol = 0.01)
}
\arguments{
  \item{data}{
Matrix of observations where each row is a vector of observations and each column corresponds to a node.
}
  \item{radius}{
The radius of the Gaussian kernel used to estimate the covariance matrices. Unless quick changes are expected it is advised to set the radius to be appoximately equal to the number of nodes in the graph (i.e., ncol(data)). For details see references.
}
  \item{l1}{
Value for the \eqn{\lambda_1} penalty (i.e., sparsity penalty) in the SINGLE algorithm.
}
  \item{l2}{
Value for the \eqn{\lambda_2} penalty (i.e., smoothness penalty) in the SINGLE algorithm.
}
  \item{k}{
The number of adjacent estimated precision matrices to consider when optimising the SINGLE objective function. When using a small number of nodes (eg 5-20 nodes) it is advised to set k to approximately half the number of nodes (i.e., ncol(data)/2). However if a larger number of nodes are employed it is advised to reduce the value of k as this greatly increases the computational cost. For details see references.
}
  \item{tol}{
Tolerance to determine when the algorithm has converged.
}
}
\details{
The SINGLE algorithm minimises the following objective function:

\deqn{
 f({ \Theta }) =  \sum_{i=1}^T  [ -log det \hat \Theta_i + trace ( S_i \Theta_i) ] + \lambda_1 \sum_{i=1}^T || \Theta_i||_1 + \lambda_2 \sum_{|i-j|<k} || \Theta_i -  \Theta_j||_1
}

where \eqn{{  \Theta }} is the set of all precision matrices indexed over time.
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
}

\seealso{
\code{\link{plotSINGLE}, \link{generate_random_data}, \link{precision_recall}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.15, str=0.6)
\dontrun{
S = SINGLE(data=X$data, radius=5, l1=0.75, l2=0.3, k=3)
result = precision_recall(true_cov=X$true_cov, estimated_cov=S$P_)
plot(result$F1, type='l', ylim=c(0,1), ylab='', 
   main='F1 Score', xlab='Time') # plot F1 score over time
}
}
\keyword{ SINGLE }

