#' Test if identifiers have already been used in the TGR.
#'
#' @param x Character vector of proposed identifiers.
#' @param type Identifier type: `biospecimen_id`, `assay_material_id`, or `molecular_id`.
#' @return Returns the list of idnetifiers that have already been used.
#' @author Amy Paguirigan
#' @details
#' Requires **admin** REDCap credentials to be set in the environment.
#' @export
usedIdentifiers <- function(x, type) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    stop("You have missing environment variables.  Please set creds in env vars.")}
  else print("Credentials set successfully.")

  if (type == "biospecimen_id") {
    IDs <- suppressMessages(REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("INT"),
      records = x, fields = type)$data)
  }
  if (type == "assay_material_id") {
    IDs <- suppressMessages(REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("FCT"),
      records = x, fields = type)$data)
  }
  if (type == "molecular_id") {
    IDs <- suppressMessages(REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("MHT"),
      records = x, fields = type)$data)
  }
  return(IDs)
}
#' Create a snapshot of the Annotation Dictionary (not needed anymore)
#'
#' Pulls sample data down from REDCap in order to generate example column lists for use in Shiny UI's.
#'
#' @param commonKnowledge The commonKnowledge data frame containing current annotations via pullAnnotations().
#' @return A character vector containing the `categorical` annotations in the TGR.
#' @author Amy Paguirigan
#' @details
#' Requires **admin** REDCap credentials to be set in the environment.
annotationDictionary <- function(commonKnowledge) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")
  print("annotationDictionary(); setup for Shiny UI")
  # only display annotations in the Shiny app if they have been defined in commonKnowledge
  categorical <- unique(commonKnowledge[commonKnowledge$Type == "categorical",]$Annotation)
  return(categorical)
}


