#' Pull the list(s) of objects and tags in S3
#'
#' Pulls metadata about all files in S3 that are accessible, such as tags, name, and sizes.
#'
#' @param bucket The name of the S3 bucket containing the data to return.
#' @param prefix The prefix of the data being indexed by the TGR (default: "tg").
#' @return Returns a long form data frame of objects in the indicated S3 bucket.
#' @author Amy Paguirigan
#' @details
#' Requires valid S3 credentials to be set in the environment by setCreds (now "AWS_ACCESS_KEY_ID" and "AWS_SECRET_ACCESS_KEY").
#' @export
listS3Objects <- function(bucket, prefix = "tg") {
  if ("" %in% Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"))) {
    stop("You have missing environment variables.  Please setCreds().")
    } else message("Credentials set successfully.")

    message("Pulling S3 tag list.")
    if (bucket == "fh-pi-paguirigan-a-eco" & prefix == "genomicsrepo") {
      keys <- aws.s3::get_bucket_df(bucket = bucket,
                                    prefix = "tg/apptags/")$Key
      keys <- keys[grepl("*-listing.csv", keys)]
    } else {
      keys <- paste0("tg/apptags/", bucket,"-", prefix, "-listing.csv") }

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
  if ("" %in% Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"))) {
    stop("You have missing environment variables.  Please setCreds().")
  } else message("Credentials set successfully.")

  message("Pulling S3 object summary.")

  if (bucket == "fh-pi-paguirigan-a-eco") {
    keys <- aws.s3::get_bucket_df(bucket = bucket, prefix = "tg/apptags/summary/")$Key
  } else {
    keys <- paste0("tg/apptags/", bucket,"-", prefix, "-summary.csv") }

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
  if ("" %in% Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"))) {
    stop("You have missing environment variables.  Please setCreds().")
  } else message("Credentials set successfully.")
  if (is.list(tagList) == F) {stop("tagList needs to be a named list for this to work like you want it to.")
    } else {
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
