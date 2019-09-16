\name{simulateSubgroupData}
\alias{simulateSubgroupData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make a simulation for subgroup.data
}
\description{
This is a complicated function to create sophisticated simulation for subgroup analysis benchmarking
}
\usage{
simulateSubgroupData(N, case, overal_treatment_effect, subgroup_enhanced_effect, subgroup_shift_effect, subgroup_ratio, covariates_normal, covariates_binomial, covariates_binary, treatment_p, rule_depth, correlation, SD_NOISE_RATIO)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{number of samples (integer) default=2000}
  \item{case}{c("prognostic", "predictive") default="prognostic"}
  \item{overal_treatment_effect}{the base effect of treatment (double) default=1}
  \item{subgroup_enhanced_effect}{the enhancement of treatment in true positive subgroup (double) default=1}
  \item{subgroup_shift_effect}{the amount of shift for false positive subgroup (double) default=1}
  \item{subgroup_ratio}{desired ratio of subgroups, not guaranteed (double (0, 1))}
  \item{covariates_normal}{list of c(mean, sd) default=list(c(0,1), c(1,2))}
  \item{covariates_uniform}{list of c(min, max) default=list(c(0,1))}
  \item{covariates_binomial}{list of c(size, probabilty) for count data default=list(c(10, 0.5))}
  \item{covariates_binary}{vector of probabilities default=c(0.5)}
  \item{treatment_p}{ratio of treatment to control (double) default=0.5}
  \item{rule_depth}{vector of depth of true positive and false positive rules (integer) default=2}
  \item{correlation}{vector of correlation, length determines number of correlated covariates (vector) default=c(0.2)}
  \item{SD_NOISE_RATIO}{a small value for adding noise in all data simulation (double) default=0.1}
  \item{has_FP}{whether to insert a false positive subgroup (logical) default=TRUE}
}
\details{
This method is complicated and has many inputs. It can be used to benchmark different tools and test the package.
It supports four different distributions for covariates: binary, normal, uniform, and binomial.
It will create three outcomes: one numeric, one binary, and one count data.
Two random subgroups with desired size will be created.
One is a true positive where the outcome for treatment arm is enhanced.
Another one is a false positive, and the outcome is shifted for both treatment arms.
Note that the subgroup sizes are not guaranteed.
This function is under development and has not been tested.
Make logical decisions when setting parameters.
For example if you set rule_depth to 4 make sure you have 8 covariates.
prerequisite: makeRandomRule, parseRule, loadDataset
dependencies: data.table
}
\value{
  an instance of subgroup.data (list)
  \item{data}{a datatable containing all data}
  \item{covariates}{list of covariate indices in data}
  \item{outcomes}{list of outcome indices in data}
  \item{contrast}{the contrast index (trt)}
  \item{control}{the control value}
  \item{rules}{list of rules}
}
\references{
}
\author{
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  makeRandomRule
  parseRule
  loadDataset
}
\examples{
subgroup.data <- simulateSubgroupData()
subgroup.data$rules
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
