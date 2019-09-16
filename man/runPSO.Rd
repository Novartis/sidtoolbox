\name{runPSO}
\alias{runPSO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
run PSO algorithm
}
\description{
This function runs the PSO method to find rules
the genomes is size 2*M where M is number of covariates
first M genes are used for sign, with range [0,1] where [0,.5 is mapped to <= and [.5, 1] maps to >
second M genes are the cutoffs on each covariate, in range of [0, 1] which represent the quantiles
of the data (if numeric) or probability (if factor)
abstol is used to stop the algorithm early, minimum enhancement required to terminate
}
\usage{
runPSO(subgroup.data, nmin = 0.1, threshold = 0.05, desirable_response = "increasing", iterations = 100, abstol = -Inf, reltol = 0.01, seed = 42, swarm_size = 40)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subgroup.data}{a subgroup.data structure, see loadDataset, a list of data, covariates, outcomes, contrast, control [list]}
  \item{y_idx}{index of outcome to optimize [numeric or character]}
  \item{y_type}{type of outcome c("numeric", "binary", "count") default=get from subgroup.data$outcomeTypes}
  \item{nmin}{minimum size of samples in subgroup [numeric] default=0.1
}
 \item{depth}{maximum depth of rules [integer] default=length of covariates}
  \item{desirable_response}{
whether to increase or decrease the difference c("increasing", "decreasing") [character] default="increasing"
}
  \item{iterations}{
number of iteration to run [numeric] default=100
}
  \item{abstol}{
absolute tolerance [numeric] default=-Inf
}
  \item{reltol}{
restart tolerance [numeric] default=0.01
}
  \item{swarm_size}{
number of particles in swarm [numeric] default=40
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a list of rule structure [list]
}
\references{
 https://www.rdocumentation.org/packages/pso/versions/1.0.3/topics/psoptim
 }
\author{
%%  ~~who you are~~
}
\note{
requires pso
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
subgroup.data <- simulateSubgroupData()
runPSO(subgroup.data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
