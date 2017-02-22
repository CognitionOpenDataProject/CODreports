#' COD_Report function
#'
#' This is a helper function for building standardised final outcoems for COD reproducibility reports.
#' @param Article_ID Enter the article's unique ID code
#' @param Decision_Errors Enter the number of decision errors
#' @param Major_Numerical_Errors Enter the number of major numerical errors
#' @param Minor_Numerical_Errors Enter the number of minor numerical errors
#' @return Returns a formatted table (via kable) reporting error tallys. Also writes out a csv object containing the error tallys.
#' @export
#' @examples
#' COD_Report(Article_ID = "ABhgyo", Decision_Errors = 1, Major_Numerical_Errors = 4, Minor_Numerical_Errors = 12)

COD_Report <- function(Article_ID, Decision_Errors, Major_Numerical_Errors, Minor_Numerical_Errors){
  if(Decision_Errors > 0 | Major_Numerical_Errors > 0){
    finalOutcome <- "Failure"
  }else{
    finalOutcome <- "Success"
  }

  reportObject <- data.frame("Decision_Errors" = Decision_Errors,
                             "Major_Numerical_Errors" = Major_Numerical_Errors,
                             "Minor_Numerical_Errors" = Minor_Numerical_Errors)
  filename <- paste("reportObject_", Article_ID, ".csv")
  write.csv(reportObject, filename, row.names = F)
  return(kable(reportObject))
}
