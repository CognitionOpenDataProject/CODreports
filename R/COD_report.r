# Helper function to build a standardised reproducibility report.
# User specifies number of each error type.
# Function writes out a report in .csv format and returns short text report
COD_Report <- function(articleID, Decision_Errors, Major_Numerical_Errors, Minor_Numerical_Errors){
  reportObject <- data.frame("Decision_Errors" = Decision_Errors, 
                             "Major_Numerical_Errors" = Major_Numerical_Errors, 
                             "Minor_Numerical_Errors" = Minor_Numerical_Errors)
  filename <- paste("reportObject_", articleID)
  write.csv(reportObject, filename, row.names = F)
  kable(reportObject)
}

data.frame(1,2,3)
reportOutcomes('APJHA', 1,2,3)