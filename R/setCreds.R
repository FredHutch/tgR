#' Sets tgR environment variables
#'
#' @param tokenSet Which token set to apply, default is "file", and then a path to a credentials file is required.  Other options include "admin" or "app".
#' @param path The path to the file to source that contains appropriately defined environment variables.
#' @return Nothing, sets environment variables for the remaining functions to work.
#' @author Amy Paguirigan
#' @details
#' Requires the desired REDCap and S3 credentials to be set in the environment or be available in an R file specified.
#' @export
setCreds <- function(tokenSet="file", path) {
	if (tokenSet == "file"){
		source(path);
  }
  if (tokenSet == "admin"){
    Sys.setenv(INT=Sys.getenv("INTOKEN"))
    Sys.setenv(FCT=Sys.getenv("FCTOKEN"))
    Sys.setenv(MHT=Sys.getenv("MHTOKEN"))
    Sys.setenv(S3A=Sys.getenv("PAGAACCESS"))
    Sys.setenv(S3SA=Sys.getenv("PAGASECRET"))
  }
  if (tokenSet == "app"){
    Sys.setenv(INT=Sys.getenv("INAPPTOKEN"))
    Sys.setenv(FCT=Sys.getenv("FCAPPTOKEN"))
    Sys.setenv(MHT=Sys.getenv("MHAPPTOKEN"))
    Sys.setenv(S3A=Sys.getenv("PAGAACCESS2"))
    Sys.setenv(S3SA=Sys.getenv("PAGASECRET2"))
  }
  Sys.setenv(REDURI="https://cdsweb07.fhcrc.org/redcap/api/")
  Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
             AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
             AWS_DEFAULT_REGION = "us-west-2")
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    stop("You have missing environment variables.  Please set env vars.")} else print("Credentials set successfully.")
}
