\name{choose_h}
\alias{choose_h}
\title{
Estimate kernel width for Gaussian kernel and Sliding window
}
\description{
Estimate the optimal width of kernel/sliding window using look-ahead log-likelihood
}
\usage{
choose_h(data, sample_size, kernel = "gaussian", h_lower, h_upper, h_step)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data used to estimate optimal width. Each row should be an observation.
}
  \item{sample_size}{
Number of samples to use when computing the look-ahead log-likelihood. This is done in order to reduce the computational cost. 
In order to use all samples simply set sample_size = nrow(data).
}
  \item{kernel}{
Type of kernel to use. Options are "gaussian" for Gaussian kernel and "window" for sliding window.
}
  \item{h_lower}{
Lowest potential value for width \eqn{h}.
}
  \item{h_upper}{
Highest potential value for width \eqn{h}.
}
  \item{h_step}{
Step size when searching for optimal \eqn{h}}
}
\details{
We estimate the optimal value of \eqn{h} by looking to maximise the look-ahead log-likelihood. This is defined by:
}
\value{
Optimal value of \eqn{h} is returned
}
\author{
Ricardo Pio Monti
}

\seealso{
\code{\link{SINGLE}, \link{generate_random_data}, \link{get_kern_cov}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.15, str=0.6)
h_est = choose_h(data=X$data, sample_size=30, kernel='window', h_lower=10, h_upper=100, h_step=10)
C = get_kern_cov(data=X$data, h=h_est, kernel="window")
}

\keyword{ kernel }
