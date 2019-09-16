\name{makeRandomRule}
\alias{makeRandomRule}

\title{
make random rule
}
\description{
This function creates a random rule on covariates which is used in simulating data.
}
\usage{
function(covariates, depth = 2, subgroup_ratio = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{covariates}{
    covariates a data.table of covariates (you can use subgroup.data$covariates)
    depth the depth (number of covariates in rule) default=4
    subgroup_ratio the desired ratio samples in subgroup created by rule default=100
  }
}
\details{
This function creates random subgroup rules on covariates. It considers the subgroup size and tries to apply that. However for binary covariates this will not hold. For practicality, this function will return two rules each time, one is used for TP and another as FP. They will be independant.
}
\value{
a list with two rules: random_TP and random_TP
each a data.frame of 2 columns and rows=depth.
first column is the rule, and the second is the covariate name
}
\references{
}
\author{
}
\note{
The cutoffs will be rounded to 2 digits.
}

\seealso{
simulateSubgroupData
}
\examples{
subgroup.data <- simulateSubgroupData()
X <- subgroup.data$covariates
rules <- makeRandomRule(covariates = X)
rules

}

