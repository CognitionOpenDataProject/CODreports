#' codReport2 function
#'
#' This is a helper function for building standardised final outcomes for COD reproducibility reports.
#' @param Report_Type Enter 'pilot' or 'joint'
#' @param Article_ID Enter the article's unique ID code
#' @param Insufficient_Information_Errors Enter the number of Insufficient Information Errors
#' @param Decision_Errors Enter the number of decision errors
#' @param Major_Numerical_Errors Enter the number of major numerical errors
#' @param Minor_Numerical_Errors Enter the number of minor numerical errors
#' @param Author_Assistance Enter whether author assistance was required (T/F)
#' @return Returns a formatted table (via kable) reporting error tallys. Also writes out a csv object containing the error tallys.
#' @export
#' @examples
#' codReport2(Report_Type = "final", Article_ID = "ABhgyo", Insufficient_Information_Errors = 0, Decision_Errors = 1, Major_Numerical_Errors = 4, Minor_Numerical_Errors = 12, Author_Assistance = T)

codReport2 <- function(Report_Type, Article_ID, valuesChecked, Total_df = 0, Total_p = 0, Total_mean = 0, Total_sd = 0, Total_se = 0, Total_ci = 0, Total_bf = 0, Total_t = 0, Total_F = 0, Total_es = 0, Total_median = 0, Total_other = 0,
                       Insufficient_Information_Errors, Decision_Errors, Major_Numerical_Errors, Minor_Numerical_Errors,
                       Major_df = 0, Major_p = 0, Major_mean = 0, Major_sd = 0, Major_se = 0, Major_ci = 0, Major_bf = 0,
                       Major_t = 0, Major_F = 0, Major_es = 0, Major_median = 0, Major_other = 0,
                       affectsConclusion = c("Yes", "No", "Unclear"),
                       errorLocus_typo = 0, errorLocus_specification = 0, errorLocus_analysis = 0, errorLocus_data, errorLocus_unidentified = 0,
                       Author_Assistance, Nature_of_Assistance = NA,  correctionSuggested = NA, correctionPublished = NA,
                       Resolved_Insufficient_Information_Errors = NA, Resolved_Decision_Errors = NA, Resolved_Major_Numerical_Errors = NA){

  # input check
  if(!(Report_Type %in% c('pilot', 'joint', 'final'))) stop("Error! Report_Type must be either 'pilot', 'joint', or 'final'.")

  if(Decision_Errors > 0 | Major_Numerical_Errors > 0 | Insufficient_Information_Errors > 0){
    finalOutcome <- "Failure"
    if(Author_Assistance == T){
      finalOutcome <- "Failure despite author assistance"
    }
  }else{
    finalOutcome <- "Success"
    if(Author_Assistance == T){
      finalOutcome <- "Success with author assistance"
    }
  }

  reportObject <- data.frame("Insufficient_Information_Errors" = Insufficient_Information_Errors,
                             "Decision_Errors" = Decision_Errors,
                             "Major_Numerical_Errors" = Major_Numerical_Errors,
                             "Minor_Numerical_Errors" = Minor_Numerical_Errors,
                             "Final_Outcome" = finalOutcome,
                             "Affects_Conclusion" = affectsConclusion,
                             "errorLocus_typo" = errorLocus_typo,
                             "errorLocus_specification" = errorLocus_specification,
                             "errorLocus_analysis" = errorLocus_analysis,
                             "errorLocus_data" = errorLocus_data,
                             "errorLocus_unidentified" = errorLocus_unidentified,
                             "Nature_of_Assistance" = Nature_of_Assistance,
                             "correctionSuggested" = correctionSuggested,
                             "correctionPublished" = correctionPublished,
                             "Resolved_Insufficient_Information_Errors" = Resolved_Insufficient_Information_Errors,
                             "Resolved_Decision_Errors" = Resolved_Decision_Errors,
                             "Resolved_Major_Numerical_Errors" = Resolved_Major_Numerical_Errors)

  filename <- paste0("reportObject_",Report_Type,"_",Article_ID,".csv")
  write.csv(reportObject, filename, row.names = F)
  return(kable(reportObject))
}
