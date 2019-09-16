\name{parseRule}
\alias{parseRule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
parse rule
}
\description{
parse rule and return a logical vector indicating participation in subgroup created by rule
}
\usage{
parseRule(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{covariates}{a data.table of covariates (you can use subgroup.data$covariates)}
  \item{rule}{list of statement strings in rule format}
}
\details{
This function parses a rule.
A rule is a vector of strings, each containing a statement in format of "covariate sign value" i.e. "X1 = 1" or "blood >= 2.01"
A logical vector is returned indicating membership in subgroup according to the rule.
}
\value{
lofical vector
}
\references{
}
\author{
Marzie Rasekh
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
subgroup.data <- simulateSubgroupData()
subgroup_TP <- parseRule(covariates = subgroup.data$covariates,
                        rule = subgroup.data$rules$random_TP)
sum(subgroup_TP)
}

