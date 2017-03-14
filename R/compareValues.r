#' compareValues function
#'
#' This is a helper function to aid comparison of values reported in articles to values obtained in reproducibility checks.
#' (1) calculates the percentage error (PE) between a reported value and an obtained value.
#' (2) identifies the error type (major numerical, minor numerical, no error)
#' (3) if p-values are being compared, user must set isP to TRUE. Function will identify if there is an additional decision error.
#' Errors types are defined as follows:
#' >> 'minor numerical': i.e., >= 2% PE < 10%
#' >> 'major numerical' (i.e., PE >= 10%)
#' If p values are being compared, also returns an additional error type:
#' >> 'decision error' (i.e., reported p and obtained p fall on different sides of the .05 threshold)
#' @param reportedValue Enter the value reported in the article
#' @param Decision_Errors Enter the corresponding value obtained in your reproducibility check
#' @return Returns a short text report noting the error type and the PE.
#' @export
#' @examples
#' compareValues(reportedValue = 3.45, obtainedValue = 1.34)
#' compareValues(reportedValue = .054, obtainedValue = .049, isP = T)
#' compareValues(reportedValue = 15.63, obtainedValue = 15.63)

compareValues <- function(reportedValue, obtainedValue, isP = F) {

  pe <- ((abs(obtainedValue - reportedValue))/abs(reportedValue))*100 # calculate percentage error

  # identify error type
  if(pe >= 10){
    errorType <- "MAJOR NUMERICAL ERROR"
  }else if(pe > 0 & pe < 10){
    errorType <- "MINOR NUMERICAL ERROR"
  }else{
    errorType <- "MATCH"
  }

  decisionError <- "" # initially make decision error blank (only needed if p value)

  if(isP){ # if we are comparing p values
    if((reportedValue >= .05 && obtainedValue <.05) || (reportedValue < .05 && obtainedValue >= .05)){
      decisionError <- "DECISION ERROR and "
    }
  }

  reportText <- paste0(decisionError, errorType, ". The reported value (", reportedValue,") and the obtained value (", obtainedValue,") differed by ", round(pe, 2), "%")

  return(reportText)
}
