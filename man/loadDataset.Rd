\name{loadDataset}
\alias{loadDataset}
\title{
load dataset
}
\description{
This function loads a datatable into a subgroup.data structure.
This is simply a list with 5 attributes, the dataset itself, list of covariate indices, list of outcomes indices, list of outcome type, the contrast column index, and the control value in contrast column. This structure will be be used for further exploration and subgroup discovery and analysis.
}
\usage{
loadDataset(dataset, covariates, outcomes, outcomesTypes, trt, ctrl, rules = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{the dataset in data table format}
  \item{covariates}{a vector containing colnames or indices of covariates from the dataset}
  \item{outcomes}{a vector containing colnames or indices of outcomes from the dataset. currently binary, numeric, and count data is supported.}
  \item{outcomeTypes}{A vector of the outcome types, in c("numeric", "binary", "count", "survival") (vector)}
  \item{trt}{a scalar containing colname or index of test arm / contrast in the dataset. Must have at least 2 levels.}
  \item{control}{the treatment control (a level in trt column)}
  \item{rules}{a list of rules to be added to subgroup.data (list) default=list()}
}
\details{
  This function is used to setup an instance of subgroup.data which is used for generalizability across other functionalities.
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
  Marzie Rasekh
}
\note{
  This function returns errors if the covariates, outcomes, and trt overlap.   Will try to convert the trt to factor if it is not. Might cause error.
  Will return error if ctrl is not a level in trt or if trt does not have at least 2 levels.
  Will return error if indices are out of range.
  Supported outcomes are c("numeric", "binary", "count", "survival", "timetoevent")
}

\seealso{

}
\examples{
  # example
  data_sim <- simulateSubgroupData()$data
  # you can loadDataset by column names
  subgroup.data <- loadDataset(dataset = data_sim,
                             covariates = paste0("X", c(1:5)),
                             outcomes = c("Y_numeric", "Y_binary", "Y_count"), 
                             outcomeTypes = c("numeric", "binary", "count"), 
                             trt = "TRT", 
                             ctrl = 0)
  # you can loadDataset by id
  subgroup.data <- loadDataset(dataset = data_sim,
                             covariates = c(1:5),
                             outcomes = c(6:8), 
                             outcomeTypes = c("numeric", "binary", "count"), 
                             trt = 9, 
                             ctrl = 0)
}

