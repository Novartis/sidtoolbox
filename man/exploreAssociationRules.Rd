\name{exploreAssociationRules}
\alias{exploreAssociationRules}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Explore association rules
}
\description{
  Use this function to explore association rules in the data.
}
\usage{
  exploreAssociationRules(subgroup.data, support, confidence)
}
\arguments{
  \item{subgroup.data}{
    A list containing the dataset, list of covariates, list of outcomes, the contrast, and control group (list)
  }
  \item{support}{
    the support for each rule, see arules (numeric) default is 0.1
  }
  \item{confidence}{
    the rule confidence, see arules (numeric) default is 0.5
  }
}
\details{
Association rules can reveal higher coordinate relationships that the correlation matrix could not find. Use listAssociationRules to print in table format. Use plotAssiciationRules to plot them. This function uses the 'apriori' from 'arules' package. See https://www.rdocumentation.org/packages/arules/versions/1.6-3/topics/apriori

dependencies: arules
}
\value{
Returns an object of class '>rules or '>itemsets
see: apriori in R
}
\references{
Hornik, Kurt, Bettina Grün, and Michael Hahsler. "arules-A computational environment for mining association rules and frequent item sets." Journal of Statistical Software 14.15 (2005): 1-25.
https://cran.r-project.org/web/packages/arules/index.html
}
\author{

}
\note{

}

\seealso{
listAssociationRules
plotAssociationRules
plotAssocationRulesInteractive
}
\examples{
data_sim <- sim_data(N = 2000, case = 1)
rules <- exploreAssociationRules(subgroup.data = data_sim)
}

\keyword{ ~explore }
