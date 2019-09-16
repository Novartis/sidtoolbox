\name{runTSDT}
\alias{runTSDT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
run TSDT algorithm
}
\description{
TSDT finds rules of maxdepth and supports binary, survival, and continuous outcomes (Battioui et al. 2014)
Algorithm:
1. Draw B bootstrap or subsamples, for each sample
1.2. Construct a tree on one treatment arm, leaves form subgroups
1.3. Calculate the treatment difference in subgroups using the other arm
2. Rank subgroups according to internal and external consistency
3. Calculate the median as the suggested cutoff
4. Apply multiplicity adjustment through P permutations (of Y)
5. Calculate adjusted p values
}
\usage{
runTSDT(subgroup.data, y_idx, y_type = NULL, maxdepth = 4, min_subgroup_n_control = 0.05, min_subgroup_n_trt = 0.05, desirable_response = "increasing", nsamples = 1, npermutations = 1, n_cpu = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subgroup.data}{a subgroup.data structure, see loadDataset, a list of data, covariates, outcomes, outcomeTypes, contrast, control [list]}
  \item{y_idx}{index of outcome to run on [numeric or character])}
  \item{y_type}{type of outcome c("numeric", "binary", "survival") [character] default will be read from subgroup.data$outcomeTypes}
  \item{maxdepth}{maximum depth of rules [numeric] default=4}
  \item{min_subgroup_n_control}{minimum size of control samples in subgroup [numeric] default=0.05}
  \item{min_subgroup_n_trt}{  minimum size of treatment samples in subgroup [numeric] default=0.05}
  \item{desirable_response}{whether to increase or decrease the difference c("increasing", "decreasing") [character] default="increasing"}
  \item{nsamples}{number of bootstrapping subsamples, suggested 100 [numeric] default=100}
  \item{npermutations}{ number of permutations, suggested 500 [numeric] default=0}
  \item{n_cpu}{number of cpus to use [numeric] default=8}
}
\details{

}
\value{
returns a data.table, see TSDT documentation for details
}
\references{
Battioui et al. 2014
https://rdrr.io/cran/TSDT/man/TSDT.html
}
\author{

}
\note{
 cannot run for 1 covariate
 solution, I add a dummy variable
 added to all TSDT runs, add a dummy, then had the problem of finding dummy rules
 added check to remove dummy rules, find is.dummy --> removed
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
parseTSDTResults
}
\examples{
subgroup.data <- simulateSubgroupData()
for (y_idx in subgroup.data$outcomes) {
  runTSDT(subgroup.data, y_idx)
}

}

