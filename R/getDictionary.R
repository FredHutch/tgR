#' Retrieve an example data dictionary from the TGR REDCap projects
#'
#'
#' @param project A string for which REDCap project to return data dictionary for, such as "specimen", "assay", or "molecular".
#' @return An array of field names for the project specified.
#' @author Amy Paguirigan
#' @details
#' Requires REDCap credentials to be set in the environment.
#' @export
getDictionary <- function(project) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    stop("You have missing environment variables.  Please set creds in env vars.")}
  else print("Credentials set successfully.")
  if (project == "specimen") {
    result <- jsonlite::fromJSON(RCurl::postForm(
      uri=Sys.getenv("REDURI"),
      token=Sys.getenv("INT"),
      content='metadata',
      format='json',
      returnFormat='json'
    ))
  }
  if (project == "assay") {
    result <- jsonlite::fromJSON(RCurl::postForm(
      uri=Sys.getenv("REDURI"),
      token=Sys.getenv("FCT"),
      content='metadata',
      format='json',
      returnFormat='json'
    ))
  }
  if (project == "molecular") {
    result <- jsonlite::fromJSON(RCurl::postForm(
      uri=Sys.getenv("REDURI"),
      token=Sys.getenv("MHT"),
      content='metadata',
      format='json',
      returnFormat='json'
    ))
  }
  fields <- result$field_name
  return(fields)

}
