#' Pull the list(s) of objects and tags in S3
#'
#' Pulls Information from the results of s3tagcrawler for TGR that are in an S3 bucket, including the object list and their tags as well as size metadata.
#'
#' @param bucket A character vector containing the full names of the S3 bucket(s) containing the data to return.
#' @return Returns a long form data frame of objects in the indicated S3 bucket(s).
#' @author Amy Paguirigan
#' @details
#' Requires valid S3 credentials to be set in the environment by setCreds.
#' @export
listS3Objects <- function(bucket) {
  if ("" %in% Sys.getenv(c("S3A", "S3SA"))) {
    print("You have missing environment variables.  Please setCreds().")
    } else print("Credentials set successfully.")

    Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
               AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
               AWS_DEFAULT_REGION = "us-west-2")

    print("Pulling S3 tag list(s).")
    if (bucket == "fh-pi-paguirigan-a-genomicsrepo") {
      keys <- aws.s3::get_bucket_df(bucket = bucket,
                                    prefix = "apptags/meta/")$Key
    } else {
      keys <- paste0("tg/apptags/", x, "-meta.csv") }

    s3tags <- purrr::map_dfr(keys, function(x) {
      aws.s3::s3read_using(utils::read.csv, stringsAsFactors = F,
                           object = x,
                           bucket = bucket)})
    return(s3tags)
}
#' Pull a summary of objects in S3
#'
#' Pulls summary information from the apptags prefix in S3 for a given S3 bucket(s).
#'
#' @param bucket A character vector containing the full names of the S3 bucket(s) containing the data to return.
#' @return Returns a data frame containing a summary of what objects are in the indicated S3 bucket(s).
#' @author Amy Paguirigan
#' @details
#' Requires valid S3 credentials to be set in the environment by setCreds.
#' @export
summarizeS3Objects <- function(bucket) {
  if ("" %in% Sys.getenv(c("S3A", "S3SA"))) {
    print("You have missing environment variables.  Please setCreds().")
    } else print("Credentials set successfully.")

  Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
             AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
             AWS_DEFAULT_REGION = "us-west-2")

  print("Pulling S3 object summary.")

  if (bucket == "fh-pi-paguirigan-a-genomicsrepo") {
    keys <- aws.s3::get_bucket_df(bucket = bucket, prefix = "apptags/summary/")$Key
  } else {
    keys <- paste0("tg/apptags/", x, "-summary.csv") }

  s3summary <- purrr::map_dfr(keys, function(x) {
    aws.s3::s3read_using(utils::read.csv, stringsAsFactors = F,
                         object = x,
                         bucket = bucket)})
  return(s3summary)
}

