\name{listAssociationRules}
\alias{listAssociationRules}
\title{
list association rules
}
\description{
This function finds association rules, filters them, and returns them in a table format.
}
\usage{
listAssociationRules(subgroup.data, support, confidence, top, filterby)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subgroup.data}{
    A list containing the dataset, list of covariates, list of outcomes, the contrast, and control group (list)
  }
  \item{support}{
    the support for each rule, see arules (double) default is 0.1
  }
  \item{confidence}{
    the rule confidence, see arules (double) default is 0.5
  }
  \item{top}{
    the number of top rules to keep (integer) default is 10
  }
  \item{filterby}{
    the measure to filter the top by c("support", "confidence", "lift")
  }
  \item{onlyCovariates}{whether to exclude outcomes (logical) default=FALSE}
}
\details{
This can reveal higher coordinate relationships that the correlation matrix could not find. This function uses the 'inspect' from 'arules' package. See https://www.rdocumentation.org/packages/arules/versions/1.6-3/topics/inspect
}
\value{
a table with LHS (left hand set), RHS (right hand set), support, confidence and lift columns and 'top' rows
}
\references{
Hornik, Kurt, Bettina Grün, and Michael Hahsler. "arules-A computational environment for mining association rules and frequent item sets." Journal of Statistical Software 14.15 (2005): 1-25.
https://cran.r-project.org/web/packages/arules/index.html

dependencies: arules
}
\author{
}
\note{
%%  ~~further notes~~
}

\seealso{
exploreAssociationRules
plotAssociationRules
plotAssocationRulesInteractive
}
\examples{
data_sim <- sim_data(N = 2000, case = 1)
listAssociationRules(subgroup.data = data_sim)
}
\keyword{ ~explore }
\keyword{ ~associationrules }
