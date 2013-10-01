\name{generate_random_data}
\alias{generate_random_data}
\title{
Generate a random dynamic graph using Vector Autoregressive (VAR) processes
}
\description{
This function generates random data with a dynamic correlation structure. Given the number of changepoints, the correlation structure within each segment of data (i.e. between changepoints) is randomly simulated using Erdos-Renyi random graphs. 
}
\usage{
generate_random_data(ROI, length_, seg, sparsity, str = 0.6)
}
\arguments{
  \item{ROI}{
Number of regoins of interest (i.e., number of nodes in the simulated graph)
}
  \item{length_}{
The length of each segment of data between changepoints
}
  \item{seg}{
Number of data segments (each with its own randomly sampled correlation structure)
}
  \item{sparsity}{
Sparsity parameter when simulating Erdos-Renyi random graphs. This is the probability of an edge occuring between each pair of nodes. Should be between 0 and 1.
}
  \item{str}{
The strength of the correlation between nodes. This should be be between -1 and 1.
}
}
\value{
\item{data}{A matrix containing the simulated data}
\item{true_cov}{An array where \code{true_cov[,,i]} contains the adjacency matrix corresponding to the true correlation structure at the ith observation}
}
\author{
Ricardo Pio Monti
}


\seealso{
\code{\link{SINGLE}, \link{plotSINGLE}, \link{precision_recall}}
}
\examples{
X = generate_random_data(ROI=10, length_=100, seg=3, sparsity=0.1, str=0.6)
\dontrun{
S = SINGLE(data=X$data, l1=0.5, l2=0.25, k=5)
precision_recall(true_cov=X$true_cov, estimated_cov=S$P_)
}
}

