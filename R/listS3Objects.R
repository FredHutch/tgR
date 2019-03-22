#' Pull the list of objects and tags in S3
#'
#' Pulls Information from the results of s3tagcrawler for TGR that are in an S3 bucket, including the object list and their tags as well as size metadata.
#'
#' @param bucket The name of the S3 bucket containing the data.
#' @return Returns a long form data frame of annotated objects in the S3 bucket.
#' @author Amy Paguirigan
#' @details
#' Requires S3 credentials to be set in the environment by setCreds.
#' @export
listS3Objects <- function(bucket) {
    Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
               AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
               AWS_DEFAULT_REGION = "us-west-2")
    print("Pulling all S3 tag lists.")
    s3tags <- aws.s3::s3read_using(utils::read.csv, stringsAsFactors = F,
                           object = "tg/apptags/s3tags.csv",
                           bucket = bucket)
    s3tags <- s3tags %>% dplyr::select("key", "molecular_id", "omics_sample_name", "stage", "workflowID")
    s3tags$pi_bucket <- bucket
    s3tags <- s3tags[s3tags$molecular_id != "" & s3tags$omics_sample_name != "" & s3tags$stage != "", ]
    print("Pulling all S3 object size lists.")
    s3sizes <- aws.s3::s3read_using(utils::read.table, stringsAsFactors = F,
                          col.names = c("dateCreated", "timeCreated", "sizeBytes", "key"),
                          object = "tg/apptags/s3sizes.tsv",
                          bucket = bucket)
    s3sizes$pi_bucket <- bucket
    print("Joining tags and sizes.")
    allObjects <- dplyr::inner_join(s3tags, s3sizes)
    return(allObjects)
}
