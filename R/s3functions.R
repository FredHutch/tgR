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
    stop("You have missing environment variables.  Please setCreds().")
    } else message("Credentials set successfully.")
  if(Sys.getenv("AWS_ACCESS_KEY_ID") == "") {
    Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
               AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
               AWS_DEFAULT_REGION = "us-west-2")} # only set them if they are unset

    message("Pulling S3 tag list(s).")
    if (bucket == "fh-pi-paguirigan-a-genomicsrepo") {
      keys <- aws.s3::get_bucket_df(bucket = bucket,
                                    prefix = "apptags/meta/")$Key
    } else {
      keys <- paste0("tg/apptags/", bucket, "-meta.csv") }

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
    stop("You have missing environment variables.  Please setCreds().")
    } else message("Credentials set successfully.")
  if(Sys.getenv("AWS_ACCESS_KEY_ID") == "") {
  Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
             AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
             AWS_DEFAULT_REGION = "us-west-2")}

  message("Pulling S3 object summary.")

  if (bucket == "fh-pi-paguirigan-a-genomicsrepo") {
    keys <- aws.s3::get_bucket_df(bucket = bucket, prefix = "apptags/summary/")$Key
  } else {
    keys <- paste0("tg/apptags/", bucket, "-summary.csv") }

  s3summary <- purrr::map_dfr(keys, function(x) {
    aws.s3::s3read_using(utils::read.csv, stringsAsFactors = F,
                         object = x,
                         bucket = bucket)})
  return(s3summary)
}
#' (admin) Copy and tag objects between S3 buckets
#'
#' Copies and tags objects in an S3 bucket from another S3 bucket.
#'
#' @param fromBucket A character vector containing the full name of the source bucket
#' @param fromPrefix A character vector containing the prefix of the object to copy in the source bucket
#' @param toBucket A character vector containing the full name of the destination bucket
#' @param toPrefix A character vector containing the prefix to give to the object in the destination bucket
#' @param tagList A named list of tags to apply to the destination object
#' @return TBD
#' @author Amy Paguirigan
#' @details
#' Requires valid admin S3 credentials to be set in the environment by setCreds and the likely the output from cromwellOutput()
#' @export
s3_copy_and_tag <- function(fromBucket, fromPrefix, toBucket, toPrefix, tagList) {
  if ("" %in% Sys.getenv(c("S3A", "S3SA"))) {
    stop("You have missing environment variables.  Please setCreds().")
  }
  if (is.list(tagList) == F) {stop("tagList needs to be a named list for this to work like you want it to.")} else {
    if(Sys.getenv("AWS_ACCESS_KEY_ID") == "") {
    Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
             AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
             AWS_DEFAULT_REGION = "us-west-2")}

  if ("molecular_id" %in% names(tagList)){
    a <- aws.s3::copy_object(from_object = fromPrefix,
                             from_bucket = fromBucket,
                             to_object = toPrefix,
                             to_bucket = toBucket)

    b <- aws.s3::put_object_tagging(bucket = toBucket,
                                    object = toPrefix,
                                    tags = tagList)
    return(list(object = paste0("s3://", toBucket, "/", toPrefix), upload = a, tagged = b))
  } else {stop("The tag molecular_id is required because that's the ENTIRE point.")}
  }
}

#' (admin) Prep a data frame for s3_copy_and_tag()
#'
#' Creates a list of tibbles for each row of the provided data frame such that the data can then be used to pass arguments to s3_copy_and_tag
#'
#' @param thisDataFrameofMine A data frame containing the columns: "molecular_id", "s3Prefix", "s3Bucket", "s3DestinationPrefix", "s3DestinationBucket" as well as any tags desired as additional columns.
#' @return A list of lists ready for s3_copy_and_tag
#' @author Amy Paguirigan
#' @details
#' Requires valid admin S3 credentials to be set in the environment by setCreds and the likely the output from cromwellOutput()
#' @export
prep_s3_copy_and_tag <- function(thisDataFrameofMine){
  if (is.data.frame(thisDataFrameofMine)==T){
    if (ncol(thisDataFrameofMine[,c("molecular_id", "s3Prefix", "s3Bucket", "s3DestinationPrefix", "s3DestinationBucket")]) == 5) {
      objectNTagList <- list(rep(list(), nrow(thisDataFrameofMine)))
      for (i in seq(from = 1, to = nrow(thisDataFrameofMine), by = 1)) {
        objectNTagList[[i]] <- list(s3Bucket = thisDataFrameofMine$s3Bucket[i],
                                s3Prefix = thisDataFrameofMine$s3Prefix[i],
                                s3DestinationBucket = thisDataFrameofMine$s3DestinationBucket[i],
                                s3DestinationPrefix = thisDataFrameofMine$s3DestinationPrefix[i],
                                tagSet = as.list(
                                  thisDataFrameofMine[i,!names(thisDataFrameofMine) %in%
                                                        c("s3Prefix", "s3Bucket", "s3DestinationPrefix", "s3DestinationBucket")]))
      }
      return(objectNTagList)
    } else {stop("There are missing or incorrectly named columns: molecular_id, s3Prefix, s3Bucket, s3DestinationPrefix, s3DestinationBucket")}
  } else {stop("Please provide a data frame as input.")}
}
