\name{runVirtualTwins}
\alias{runVirtualTwins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
run VirtualTwins algorithm
}
\description{
This function runs the Virtual Twins method
https://rdrr.io/cran/aVirtualTwins/
Borrows counterfactual model idea in causal inference
Step 1: Impute unobserved outcome and calculate individualized treatment effects Z, using random forests
Step 2: Apply recursive partitioning on X with Z as response to identify subgroups with large Z’s
Implemented in aVirtualTwins R package
Only handles binary outcomes
requires aVirtualTwins, randomForest
}
\usage{
runVirtualTwins(subgroup.data, y_idx, desirable_response = "increasing", forest.type = "double", tree.type = "reg", folds = 10, sampsize = 1, maxdepth = 4, method = "logit", interactions = TRUE, ntree = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subgroup.data}{a subgroup.data structure, see loadDataset, a list of data, covariates, outcomes, outcomeTypes, contrast, control [list]}
  \item{y_idx}{index of outcome to run on [numeric or character])}
  \item{maxdepth}{maximum depth of rules [numeric] default=4}
  \item{desirable_response}{whether to increase or decrease the difference c("increasing", "decreasing") [character] default="increasing"}
  \item{forest.type}{c("one", "double", "fold") [character] default="double"}
  \item{tree.type}{ c("class", "reg") [character] default="reg"}
  \item{folds}{if forest.type=fold [numric] default=10}
  \item{sampsize}{ratio to balance outcome class [numeric] default=1}
  \item{method}{c("absolute", "relative", "logit") [character] default="logit"}
  \item{interactions}{a logical value, whether to calculate interactions [logical] default=TRUE}
  \item{ntree}{number of trees in random forest}
}
\details{
}
\value{
a data.table of rules [data.table]
}
\references{
Foster et. al (2011) Stat Med 30:2867-2880
}
\author{

}
\note{
Fold is not working.
Only runs for binary outcomes
}


\seealso{
https://rdrr.io/cran/aVirtualTwins/
}
\examples{
subgroup.data <- simulateSubgroupData()
binary_outcomes <- which(subgroup.data$outcomeTypes == "binary")
for (y_idx in binary_outcomes) {
  runVirtualTwins(subgroup.data, y_idx)
}

}
