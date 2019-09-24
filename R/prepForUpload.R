#' Validate a data frame for upload to TGR
#'
#' @param dataToValidate A data frame containing the data you want to validate for upload.
#' Note:  For a valid dataset to be uploaded to the TGR REDCap, it must have the `molecular_id` column first.
#' @return A list of results including: `assessment` which is the results from REDCapR's `validate_for_write` function;
#' `invalid` which is a data frame including `molecular_id` that includes columns that are not included in the TGR data dictionary and cannot be uploaded;
#' and `valid` which is a data frame suitable for uploading via the Data Import Tool in REDCap after writing it to a csv.
#' @author Amy Paguirigan
#' @details
#' Requires REDCap credentials to be set in the environment.  Does not allow for changes in DAG or marking if an instrument is complete.
#' @export
validateUpload <- function(dataToValidate) {
  if ("" %in% Sys.getenv(c("REDURI", "TGR"))) {
    stop("You have missing environment variables.  Please use setCreds().")}
  else message("Credentials set successfully.")
    if (is.data.frame(dataToValidate) == T) {
      if (nrow(dataToValidate) > 0 ){
        if ("molecular_id" %in% colnames(dataToValidate) == T) {
          dataToValidate[dataToValidate == "NA"] <- ""
          dataToValidate[is.na(dataToValidate) == T] <- ""
      assessment <- REDCapR::validate_for_write(dataToValidate) # return whatever this gives you, possibly useless
      allcolumns <- suppressMessages(getDictionary())
      nonRequiredCols <- allcolumns[!allcolumns %in% c("molecular_id")]
      invalid <- dataToValidate[!colnames(dataToValidate) %in% nonRequiredCols] # return columns that are named wrong
      if (colnames(invalid) == "molecular_id") {invalid$molecular_id <- NULL}
      valid <- dataToValidate[colnames(dataToValidate) %in% allcolumns] # return columns that are named correctly
      results <- list(assessment = assessment, invalid = invalid, valid = valid)
        } else {stop("Please provide a data frame with the column `molecular_id` to validate.")}
      }  else {stop("Please provide a data frame with more than 0 rows to validate.")}
    } else {stop("Please provide a data frame to validate.")}
return(results)
}

