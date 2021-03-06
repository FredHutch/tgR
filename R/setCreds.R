#' Sets tgR environment variables
#'
#' @param path The path to the file to source that contains appropriately defined environment variables (see requiredCredentials.R for an example).
#' @param tokenSet Which token set to apply, default is "file", and then a path to a credentials file is required.  Other options include "admin" or "app".
#' @return Nothing, sets environment variables for the remaining functions to work.
#' @author Amy Paguirigan
#' @details
#' Requires the desired REDCap and S3 credentials to be set in the environment or be available in an R file specified.
#' @export
setCreds <- function(path, tokenSet="file") {
  Sys.setenv(AWS_DEFAULT_REGION = "us-west-2")
	if (tokenSet == "file"){
		source(path);
  }
  if (tokenSet == "app"){
    Sys.setenv(TGR=Sys.getenv("TGRAPPTOKEN"))
    Sys.setenv(S3A=Sys.getenv("PAGAACCESS2"))
    Sys.setenv(S3SA=Sys.getenv("PAGASECRET2"))
    Sys.setenv(AWS_ACCESS_KEY_ID=Sys.getenv("PAGAACCESS2"))
    Sys.setenv(AWS_SECRET_ACCESS_KEY=Sys.getenv("PAGASECRET2"))
  }

  if ("" %in% Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"))) {
    # if the person is still using the old set of creds, repurpose them
    Sys.setenv(AWS_ACCESS_KEY_ID= Sys.getenv("S3A"),
             AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"))
  }
  Sys.setenv(REDURI="https://redcap.fredhutch.org/API/")
  if ("" %in% Sys.getenv(c("REDURI", "TGR", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION"))) {
    stop("You have missing environment variables.  Please set env vars.")} else message("Credentials set successfully.")
}
