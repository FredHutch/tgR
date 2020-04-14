#' Retrieve an example data dictionary from the TGR REDCap project
#'
#'
#' @return An array of field names for the project specified.
#' @author Amy Paguirigan
#' @details
#' Requires setCreds() to have been run successfully.
#' @export
getDictionary <- function() {
  if ("" %in% Sys.getenv(c( "TGR", "REDURI"))){
    stop("You have missing environment variables.  Please set creds in env vars.")}
  else message("Credentials set successfully.")

  fields <- REDCapR::redcap_variables(redcap_uri = Sys.getenv("REDURI"),
                                      token=Sys.getenv("TGR"),
                                      verbose = FALSE)$data$original_field_name
  fields <- c(fields, "redcap_data_access_group")
  return(fields)
}
