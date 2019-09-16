\name{interactionEffectSize}
\alias{interactionEffectSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interaction effect size for a rule
}
\description{
This function allows you to calculate the interaction effect for a given rule.
}
\usage{
interactionEffectSize(subgroup.data, y_idx, subgroup_idx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{subgroup.dataseta list of data, covariates, outcomes and the trt, see loadDataset (list)}
  \item{y_idx}{index of outcome to calculate measure on (integer)}
  \item{subgroup_idx}{a logical vector indicating participation in subgroup}
}
\details{
This function allows you to calculate the interaction effect size for a given rule. This measure can be used as the fitness of a rule. Fitness is what we try to minimize/maximize. Fitness in this version is the effect size given by a model.
For numeric outcome, a linear model is calculate, for binary a glm with family=binomial, and for negative binomial a glm.nb model.
The model formula is: Y ~ subgroup + treatment_contrast + subgroup*treatment_contrast
where cofficient on the interaction term is returned.
To fit to your project, overwrite this function to another that returns 2 scalars:
c(effect_size, p-value)
p-value is check to be significant while effect size is used for optimization.
Ideas to investigate: cohen's d as standardized effect size
TODO: add pareto
}
\value{
two scalar values
  \item{effect_size}{a numeric value to be optimizes}
  \item{p-value}{the p-value}
}
\references{
Zeileis, Achim, Christian Kleiber, and Simon Jackman. "Regression models for count data in R." Journal of statistical software 27.8 (2008): 1-25.
}
\author{
Marzie Rasekh
}
\note{
TODO: add pareto and cohen's d
}

\seealso{
https://www.rdocumentation.org/packages/MASS/versions/7.3-51.4/topics/glm.nb

https://www.rdocumentation.org/packages/stats/versions/3.6.0/topics/lm

https://www.rdocumentation.org/packages/stats/versions/3.6.0/topics/glm
}
\examples{
subgroup.data <- simulateSubgroupData(overal_treatment_effect = 0, SD_NOISE_RATIO = 0)
rule <- subgroup.data$rules$random_TP[, "rule"]
y_idx <- subgroup.data$outcomes[1]
sbg_idx <- parseRule(covariates = subgroup.data$data[, subgroup.data$covariates, with = FALSE],
                     rule = rule)
fitnessOfRule(subgroup.data,
              y_idx,
              sbg_idx)
}
