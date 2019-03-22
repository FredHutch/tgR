#' Sets tgR environment variables
#'
#' @param tokenSet Which token set to apply, currently either "admin" or "app".
#' @return Nothing, sets environment variables for the remaining functions to work.
#' @author Amy Paguirigan
#' @details
#' Requires the desired REDCap and S3 credentials to be set in the environment.
#' @export
setCreds <- function(tokenSet="admin") {
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
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")
}
