#' Copy annotations from another record
#' @description Copies the study, specimen and assay material metadata from another record in the Repository
#' @param copyFrom The molecular_id of the record to copy data from
#' @param copyTo The molecular_id of the record to edit using copied data
#' @param type A character array of the types of annotations you want to copy: "submission", "study", "subject", "biospecimen", "assaymaterial", "genomics". Default: c("submission", "study")
#' @param overwriteExisting Default to false.  If data exists in the record to edit, do you want to overwrite existing data?
#' @return Results of the copy attempt including the data of the new record as well as the REDCap response.
#' @author Amy Paguirigan
#' @details Requires valid credentials to be set in the environment with setCreds() and to have write priviledges to the data
#' @export
copyAnnotations <- function(copyFrom = NULL, copyTo = NULL, type = c("submission", "study"), overwriteExisting = FALSE) {
  if ("" %in% Sys.getenv(c("TGR", "REDURI"))) {
    stop("You have missing environment variables.  Please set creds in env vars.")}
  else message("Credentials set successfully.")

  if (is.null(copyFrom) == TRUE | is.null(copyTo) == TRUE) {
    stop("Please specify both the molecular_id of the record to copy from (copyFrom) and that for the record to edit (copyTo).")
  } else if (sum(!type %in% c("submission", "study", "subject", "biospecimen", "assaymaterial", "genomics")) > 0) {
    message('Please choose one or more of these values: "submission", "study", "subject", "biospecimen", "assaymaterial", "genomics".')
  } else {
    dictionary <- suppressMessages(
      REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("TGR"))$data)
    type <- c("id", type)
    onlyThese <- dictionary[dictionary$field_annotation %in% type,]
    sourceRecord <- suppressMessages(REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("TGR"), records = copyFrom,
      fields = paste0(onlyThese$field_name, collapse = "',"),
      export_data_access_groups = TRUE, guess_type = F)$data)
    ## Test to make sure these molecular_ids are valid and the user has access to them
    if (nrow(sourceRecord) != 1) {
      stop("Please enter a valid molecular_id to copy from.")
    } else {
      destinationRecord <- suppressMessages(REDCapR::redcap_read_oneshot(
        Sys.getenv("REDURI"), Sys.getenv("TGR"), records = copyTo,
        fields = paste0(onlyThese$field_name, collapse = "',"),
        export_data_access_groups = TRUE, guess_type = F)$data)
      # only keep fields with values in them in destination record
      destinationRecord <- destinationRecord[,is.na(destinationRecord)==F]
      if (nrow(destinationRecord) != 1) {
        stop("Please enter a valid molecular_id to copy to.")
      }
    }
    if (overwriteExisting == FALSE) {
      # only source columns that don't already have a value in the destination REcord AND aren't NA
      onlyNewData <- sourceRecord[,!colnames(sourceRecord) %in% colnames(destinationRecord) & is.na(sourceRecord)==F]
      updatedRecord <- cbind(destinationRecord, onlyNewData)
    } else if (overwriteExisting == TRUE) {
      updatedRecord <- sourceRecord
      updatedRecord$molecular_id <- destinationRecord$molecular_id
      updatedRecord$redcap_data_access_group <- destinationRecord$redcap_data_access_group
    }
    uploadResponse <- REDCapR::redcap_write_oneshot(updatedRecord, redcap_uri = Sys.getenv("REDURI"), token = Sys.getenv("TGR"))
  }
  return(list(data = updatedRecord, redcap_result = uploadResponse))
  }


