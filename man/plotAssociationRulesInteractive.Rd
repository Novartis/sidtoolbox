\name{plotAssociationRulesInteractive}
\alias{plotAssociationRulesInteractive}
\title{
  plot association rules in an interactive mode
}

\description{
  You can display association rules in an interactive format. No useful for markdown.
}

\usage{
  plotAssociationRules(subgroup.data, support, confidence)
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
  This function allows you to plot the rules found using the aruleViz package.
  uses function: exploreAssociationRule
  dependencies: arules, aruleViz
}

\value{an interactive plot}

\references{
Hornik, Kurt, Bettina Grün, and Michael Hahsler. "arules-A computational environment for mining association rules and frequent item sets." Journal of Statistical Software 14.15 (2005): 1-25.
https://cran.r-project.org/web/packages/arules/index.html

Hahsler, Michael, and Sudheer Chelluboina. "Visualizing association rules: Introduction to the R-extension package arulesViz." R project module (2011): 223-238.
}

\author{}

\note{}

\seealso{
exploreAssociationRules
plotAssociatinRules
}

\examples{
data_sim <- sim_data(N = 2000, case = 1)
plotAssociationRulesInteractive(subgroup.data = data_sim)
plotAssociationRulesInteractive(subgroup.data = data_sim,
                     support = 0.1,
                     confidence = 0.9)
}

\keyword{ ~explore }
