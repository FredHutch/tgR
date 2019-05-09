#' Finds undefined REDCap variables
#'
#' Pulls sample data down from REDCap and compares them with the defined annotation list in GitHub and identifies variables in REDCap that need defining in GitHub.  Only variables for which there is a value in at least one record are returned.
#'
#' @param commonKnowledge The commonKnowledge data frame containing current annotations via pullAnnotations().
#' @return A data frame of makeMeaning.  This is a template to add to the commonKnowledge dataframe with new definitions or corrections.
#' @author Amy Paguirigan
#' @details
#' Requires **admin** REDCap credentials to be set in the environment.
#' @export
undefinedAnnotations <- function(commonKnowledge) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")

  print("Get all data from REDCap for variables intended to be harmonized.")
  sciMeta <- redcapPull(harmonizedOnly = T, DAG = "all", evenEmptyCols = F)
  # Remove project memberships
  defineMe <- sciMeta %>% dplyr::select(-dplyr::starts_with("data_is_"))
  categorical <- commonKnowledge %>% dplyr::filter(Type == "categorical")
  noLevelAnnots <- commonKnowledge %>% dplyr::filter(Type != "categorical")

  usedAnnots <- purrr::map_dfr(colnames(defineMe), function(x){
    Y <- unique(dplyr::select(defineMe, x))
    colnames(Y) <- "Value"
    Y$Value <- as.character(Y$Value)
    Y$Annotation <- x
    Y
  })

  usedCat <- usedAnnots %>% dplyr::filter(Annotation %in% categorical$Annotation & is.na(Value) !=T)
  missingCat <- dplyr::anti_join(usedCat, categorical)

  usedOther <- usedAnnots %>% dplyr::filter(!Annotation %in% categorical$Annotation) %>% dplyr::select(Annotation) %>% unique()
  missingOther <- dplyr::anti_join(usedOther, noLevelAnnots)

  makeMeaning <- dplyr::full_join(missingCat, missingOther)

  suppressWarnings(makeMeaning <- dplyr::bind_rows(commonKnowledge[0,], makeMeaning)) # make a sample data frame to fill in

  return(makeMeaning)
}

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
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")

  if (type == "biospecimen_id") {
    IDs <- REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("INT"),
      records = x, fields = type)$data
  }
  if (type == "assay_material_id") {
    IDs <- REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("FCT"),
      records = x, fields = type)$data
  }
  if (type == "molecular_id") {
    IDs <- REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("MHT"),
      records = x, fields = type)$data
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

#' Pull the list of objects and tags in the Repository overall
#'
#' Pulls Information from the results of s3tagcrawler for TGR that are in an S3 bucket, including the object list and their tags as well as size metadata.
#'
#' @param bucket The name of the S3 bucket containing the data.
#' @return Returns a long form data frame of annotated objects in the S3 buckets associated with the Repository.
#' @author Amy Paguirigan
#' @details
#' Requires **admin** or **app** S3 credentials to be set in the environment by setCreds.
#' @export
listS3RepoObjects <- function(bucket = "fh-pi-paguirigan-a-genomicsrepo") {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")

  Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
             AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
             AWS_DEFAULT_REGION = "us-west-2")
  a <- aws.s3::get_bucket_df(bucket = bucket, prefix = "apptags/meta/")
  b <- purrr::map_dfr(a$Key, function(x) {
    aws.s3::s3read_using(utils::read.csv, stringsAsFactors = F,
                         object = x,
                         bucket = bucket)
  })
  return(b)
}

#' Pull the summary of objects and tags in the Repository overall
#'
#' Pulls just the processed summary of object metadata for all buckets with data in the Repository.
#'
#' @param bucket The name of the S3 bucket containing the data.
#' @return Returns a long form data frame of annotated objects in the S3 bucket.
#' @author Amy Paguirigan
#' @details
#' Requires **admin** or **app** S3 credentials to be set in the environment by setCreds.
#' @export
listS3RepoSummaries <- function(bucket = "fh-pi-paguirigan-a-genomicsrepo") {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")

  Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("S3A"),
             AWS_SECRET_ACCESS_KEY = Sys.getenv("S3SA"),
             AWS_DEFAULT_REGION = "us-west-2")
  c <- aws.s3::get_bucket_df(bucket = bucket, prefix = "apptags/summary/")
  d <- purrr::map_dfr(c$Key, function(x) {
    aws.s3::s3read_using(utils::read.csv, stringsAsFactors = F,
                         object = x,
                         bucket = bucket)
  })
  return(d)
}

