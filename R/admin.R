#' Test if proposed identifiers have already been used in the TGR.
#' @description Test if proposed identifiers have already been used in the TGR, or in your data access group specifically.
#' You must provide an array of identifiers you'd like to use, specify what type of identifier it is, and also whether you want
#' to test if the identifiers are unique in the entire repository (such as for molecular_id),
#' or just unique to your data access group (such as subject_id, biospecimen_id, assay_material_id).
#' The function will return identifiers in your list that are valid for use.
#' FYI:  Querying over the entire repository will require admin credentials.
#' @param x Character vector of proposed identifiers.
#' @param type Identifier type to query, choose one of: `subject_id`,`biospecimen_id`, `assay_material_id`, or `molecular_id`.
#' @param DAG Data access group to query for uniqueness.  Do not supply DAG to test for uniqueness across the entire Repository. If you do not have admin credentials you MUST provide a DAG.
#' @return Returns any identifiers supplied that are NOT used or are valid for use.
#' @author Amy Paguirigan
#' @details
#' Some conditions may require **admin** REDCap credentials to be set in the environment.
#' @export
validIdentifiers <- function(x, type, DAG = NULL) {
  if ("" %in% Sys.getenv(c("REDURI", "S3A", "S3SA", "TGR"))) {
    stop("You have missing environment variables.  Please set creds in env vars.")}
  else message("Credentials set successfully.")

  if (is.null(DAG) == TRUE) {
    usedIDs <- suppressMessages(REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("TGR"),
      records = x, fields = type,
      export_data_access_groups = TRUE, guess_type = F)$data)
    if(ncol(usedIDs)>0) {validIDs <- as.character(x[!x %in% usedIDs[,1]])} else validIDs<-as.character(x)
  }
  if (is.null(DAG) == FALSE) {
    usedIDs <- suppressMessages(REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("TGR"),
      records = x, fields = type,
      export_data_access_groups = TRUE, guess_type = F)$data)
    thisDAG <- usedIDs %>% filter(redcap_data_access_group == DAG)
    if(ncol(thisDAG)>0) {validIDs <- as.character(x[!x %in% thisDAG[,1]])} else validIDs<-as.character(x)
  }

  return(validIDs)
}

