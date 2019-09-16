#' explore the correlations between the features
#'
#' This function allows you to plot a detail plot of correlation between features
#' with PerformanceAnalytics. However all fields will be converted to numeric
#' prerequisite:
#' dependencies: PerformanceAnalytics
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @return plots the correlation matrix using corrplot
#' @export
exploreDetailedChartCorrelation <- function(subgroup.data) {
  library(PerformanceAnalytics)
  library(Hmisc)
  library(dplyr)

  data <- subgroup.data$data %>%
    select_if(is.numeric)
  M <- ncol(data)
  
  # draw the complete correlation chart from the PerformanceAnalytics package
  chart.Correlation(data,
                    histogram=TRUE,
                    pch=19)
}
