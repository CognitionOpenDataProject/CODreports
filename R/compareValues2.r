#' compareValues2 function
#'
#' This is a helper function to aid comparison of values reported in articles to values obtained in reproducibility checks.
#' (1) calculates the percentage error (PE) between a reported value and an obtained value.
#' (2) identifies the error type (decision error, major numerical, minor numerical, no error)
#' (3) user must specificy the value type from the defaults list (or use 'other' if not listed)
#' Errors types are defined as follows:
#' >> 'minor numerical': i.e., >= 2% PE < 10%
#' >> 'major numerical' (i.e., PE >= 10%)
#' If p values are being compared, also returns an additional error type:
#' >> 'decision error' (i.e., reported p and obtained p fall on different sides of the .05 threshold)
#' The default value types are as follows: "p" (p-value), "mean" (mean), "sd" (standard deviation), "se" (standard error), "df" (degrees of freedom), "F" (F test statistic), "t" (t test statistic), "bf" (bayes factor), "ci" (confidence interval), "median" (median), "es" (effect size), "irr" (inter-rater reliability), "r" (Pearson correlation coefficient), "z" (Wilcoxon z), "coeff" (regression coefficients), "other")
#' @param reportedValue Enter the value reported in the article
#' @param obtainedValue Enter the corresponding value obtained in your reproducibility check
#' @return Returns a short text report noting the error type and the PE.
#' @export
#' @examples
#' compareValues2(reportedValue = '3.45', obtainedValue = 1.34, valueType = 'mean')
#' compareValues2(reportedValue = '.054', obtainedValue = .049, valueType = 'p')
#' compareValues2(reportedValue = '15.63', obtainedValue = 15.63, valueType = 'sd')

compareValues2 <- function(reportedValue, obtainedValue, valueType = c("p", "mean", "sd", "se", "df", "F", "t", "bf", "ci", "median", "es", "irr", "r", "z", "coeff", "other"), updatedReportObject = reportObject) {

  "inc<-" <- function(x, value) { # a custom function (from Hmisc) to increment a variable
    x + value
  }

  # check that obtained value is length one
  if(length(reportedValue) != 1){
    stop('WHOOPS! - THE REPORTED VALUE NEEDS TO HAVE LENGTH ONE')
  }

  # check that value type was specified
  if(missing(valueType)){
    stop('WHOOPS! - YOU NEED TO ENTER THE VALUE TYPE')
  }

  # check that value type was an accepted default
  if(!valueType %in% c("p", "mean", "sd", "se", "df", "F", "t", "bf", "ci", "median", "es", "irr", "r", "z", "coeff", "other")){
    stop('WHOOPS! - YOU NEED TO ENTER THE VALUE TYPE FROM THE SPECIFIED LIST (you can also specify "other")')
  }

  # check that reported value was entered as a string
  if(!is.character(reportedValue)){
    stop('WHOOPS! - YOU NEED TO ENTER THE REPORTED VALUE AS A CHARACTER STRING, NOT A NUMBER')
  }

  # identify if its a p value
  if(valueType == 'p'){
    isP <- TRUE
  }else{
    isP <- FALSE
  }

  options(scipen = 999) # turn off scientific notation

  decisionError <- "" # initially make decision error blank (only needed if p value)

  if(reportedValue == 'eyeballMATCH'){ # reported value eyeballed and its a match
    errorType <- "MATCH"
    inc(updatedReportObject[['eyeballs']]) <- 1 # total values eyeballed
    reportText <- paste0("MATCH for ", valueType, ". Eyeball comparison only.")
  }else if(reportedValue == 'eyeballMAJOR'){ # reported value eyeballed and its a MAJOR ERROR
    inc(updatedReportObject[['eyeballs']]) <- 1 # total values eyeballed
    errorType <- "MAJOR NUMERICAL ERROR"
    inc(updatedReportObject[["Major_Numerical_Errors"]]) <- 1
    inc(updatedReportObject[[paste0("Major_", valueType)]]) <- 1
    reportText <- paste0(errorType, " for ", valueType, ". Eyeball comparison only.")
  }else if(reportedValue == 'eyeballDECISION'){ # reported value eyeballed and its a decision error
    inc(updatedReportObject[['eyeballs']]) <- 1 # total values eyeballed
    errorType <- "MAJOR NUMERICAL ERROR"
    inc(updatedReportObject[["Major_Numerical_Errors"]]) <- 1
    inc(updatedReportObject[[paste0("Major_", valueType)]]) <- 1
    decisionError <- "DECISION ERROR and "
    inc(updatedReportObject[["Decision_Errors"]]) <- 1
    reportText <- paste0(decisionError, errorType, " for ", valueType, ". Eyeball comparison only.")
  }else{ # its a regular reported value - let's check it out

    # first make sure reported value and obtained value have the same number of decimal places
    # this function will return the number of decimal places
    decimalPlaces <- function(x) {
      nchar(stringr::str_split_fixed(x, "\\.", n = 2))[,2]
    }

    dp <- decimalPlaces(reportedValue) # get number of decimal places for reported value
    obtainedValue <- round(as.numeric(obtainedValue), dp) # round obtained value to the same number of decimal places
    reportedValue <- as.numeric(reportedValue) # ensure reported value is numeric

    pe <- ((abs(obtainedValue - reportedValue))/abs(reportedValue))*100 # calculate percentage error

    # identify error type
    if(pe >= 10){
      errorType <- "MAJOR NUMERICAL ERROR"
      inc(updatedReportObject[["Major_Numerical_Errors"]]) <- 1
      inc(updatedReportObject[[paste0("Major_", valueType)]]) <- 1
    }else if(pe > 0 & pe < 10){
      errorType <- "MINOR NUMERICAL ERROR"
      inc(updatedReportObject[["Minor_Numerical_Errors"]]) <- 1
    }else{
      errorType <- "MATCH"
    }

    if(isP){ # if we are comparing p values
      if((reportedValue >= .05 && obtainedValue <.05) || (reportedValue < .05 && obtainedValue >= .05)){
        decisionError <- "DECISION ERROR and "
        inc(updatedReportObject[["Decision_Errors"]]) <- 1
      }
    }

    reportText <- paste0(decisionError, errorType, " for ", valueType, ". The reported value (", reportedValue,") and the obtained value (", obtainedValue,") differed by ", round(pe, 2), "%. NB obtained value was rounded to ", dp, " decimal places.")
  }

  # update the reportObject
  inc(updatedReportObject[['valuesChecked']]) <- 1 # total values checked
  inc(updatedReportObject[[paste0("Total_", valueType)]]) <- 1 # this value type checked

  # print outcome
  print(reportText)

  return(updatedReportObject)
}


