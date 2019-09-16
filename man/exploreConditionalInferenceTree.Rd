\name{exploreConditionalInferenceTree}
\alias{exploreConditionalInferenceTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
explore conditional inferene tree
}
\description{
This method draw an inference tree trying to get interactions predicting outcomes. Supports binary and numeric outcomes.Uses ctree from the party package.
}
\usage{
exploreConditionalInferenceTree(subgroup.data, stump, teststat, testtype, nresample, significance, nmin, maxdepth)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subgroup.data}{
    a subgroups.data structured list (list)
  }
  \item{stump}{
    whether to draw a shallow tree with maximum 3 objects (logical) default=TRUE
  }
  \item{teststat}{
    "a character specifying the type of the test statistic to be applied" c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic") default="Bonferroni"
  }
  \item{testtype}{
    "a character specifying how to compute the distribution of the test statistic." c("quad", "max") default="quad"
  }
  \item{nresample}{
    number of resampling for MonteCarlo method (integer) default=100
  }
  \item{significance}{
    threshold for p-value (numeric [0-1]) default=0.025
  }
  \item{nmin}{
    minimum number of samples in each leaf (subgroup size), if <1 considered as ratio (numeric) default=0.1
  }
  \item{maxdepth}{
    maximum depth (statements in rule) (integer) default=4
  }
}
\details{
This function allows you to plot the conditional inference in your data as a tree for information on hyper parameters see 'ctree_control' from library party. For more information see https://www.rdocumentation.org/packages/partykit/versions/1.2-4/topics/ctree
}
\value{
None
}
\references{
Hothorn, Torsten, Kurt Hornik, and Achim Zeileis. "ctree: Conditional inference trees." The Comprehensive R Archive Network (2015): 1-34.
https://cran.r-project.org/web/packages/party/index.html
}
\author{
}
\note{
}

\seealso{
}
\examples{
subgroup.data <- simulateSubgroupData()
exploreConditionalInferenceTree(subgroup.data)
exploreConditionalInferenceTree(subgroup.data, stump = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~explore }
\keyword{ ~tree }
