#' Submit a workflow job to Cromwell
#'
#' Supports the submission of a fully defined workflow job to a Cromwell instance.
#'
#' @param WDL Local path to the wdl file describing the workflow. (Required)
#' @param Params Local path to the json containing the parameters to use with the workflow. (Optional)
#' @param Batch Local path to the json containing a reference to any batch file desired if the workflow is a batch. (Required)
#' @param Options Local path to the json containing workflow options to apply.(Optional)
#' @param Labels A data frame containing the labels for this workflow.(Optional)
#' @param Dependencies A zip'd file of subworkflow dependencies. (Optional)
#' @return Returns the response from the API post which includes the workflow ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellSubmitBatch <-
  function(WDL, Params=NULL, Batch, Options=NULL, Labels=NULL, Dependencies = NULL) {
    if("" %in% Sys.getenv("CROMWELLURL")) {
      stop("CROMWELLURL is not set.")
    } else
      print("Submitting a batch workflow to Cromwell.")

    bodyList <- list(
      wdlSource = httr::upload_file(WDL),
      workflowInputs = httr::upload_file(Batch))

    if(is.null(Dependencies) == F) bodyList <- c(bodyList, workflowDependencies = list(httr::upload_file(Dependencies)))
    if(is.null(Options) == F) bodyList <- c(bodyList, workflowOptions = list(httr::upload_file(Options)))
    if(is.null(Labels) == F) bodyList <- c(bodyList, labels = list(jsonlite::toJSON(as.list(Labels), auto_unbox = TRUE)))
    if(is.null(Params) == F) bodyList <- c(bodyList, workflowInputs_2 = list(httr::upload_file(Params)))

    cromDat <-
      httr::POST(
        url = paste0(Sys.getenv("CROMWELLURL"), "/api/workflows/v1"),
        body = bodyList,
        encode = "multipart"
      )
    cromResponse <-
      data.frame(httr::content(cromDat), stringsAsFactors = F)
    return(cromResponse)
  }

#' Abort a workflow job on Cromwell
#'
#' Aborts any given workflow job on Cromwell.
#'
#' @param workflow_id Unique workflow id of the job you wish to kill.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellAbort <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else
    print("Aborting job in Cromwell.")
  cromAbort <-
    httr::POST(url = paste0(
      Sys.getenv("CROMWELLURL"),
      "/api/workflows/v1/",
      workflow_id,
      "/abort"
    ))
  cromResponse <-
    data.frame(httr::content(cromAbort), stringsAsFactors = F)
  return(cromResponse)
}
#' Gets outputs for a workflow in Cromwell
#'
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellOutputs <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for outputs list for workflow id: ", workflow_id))
  }
  cromOut <-
    httr::GET(url = paste0(
      Sys.getenv("CROMWELLURL"),
      "/api/workflows/v1/",
      workflow_id,
      "/outputs"
    ))
  cromResponse <- httr::content(cromOut, as = "parsed")
  if (length(cromResponse$outputs) > 0) {
    outputsDf <- purrr::map_dfr(cromResponse$outputs, function(x) {
      Z <- data.frame("s3URL" = unlist(x), stringsAsFactors = F)
      dplyr::mutate(Z, shardIndex = seq(
        from = 0,
        to = nrow(Z) - 1,
        by = 1
      ))
    }, .id = "workflowOutputType")
    outputsDf$s3Prefix <- gsub("s3://[^/]*/", "", outputsDf$s3URL)
    outputsDf$s3Bucket <-
      gsub("/.*$", "", gsub("s3://", "", outputsDf$s3URL))
    outputsDf$workflow_id <- workflow_id
    outputsDf$workflowName <-
      gsub("/.*$",
           "",
           gsub("cromwell-output/", "", outputsDf$s3Prefix))
    outputsDf$shardIndex <-  gsub("/.*$", "",
                                       gsub("^.*shard-", "", outputsDf$s3Prefix))
  } else {
    print("No outputs are available for this workflow.")
  }
  return(outputsDf)
}
#' Gets logs for a workflow in Cromwell
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellLogs <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  }  else
    print("Getting list of logs from Cromwell.")
  cromDat <-
    httr::GET(url = paste0(
      Sys.getenv("CROMWELLURL"),
      "/api/workflows/v1/",
      workflow_id,
      "/logs"
    ))
  cromResponse <- httr::content(cromDat, as = "parsed")
  calls <- purrr::pluck(cromResponse, "calls")
  callsFlat <- purrr::map_dfr(calls, function(x) {
    justcalls <- purrr::map_dfr(x, function(s) {
      shard <-
        data.frame(rbind(unlist(s)), stringsAsFactors = F) # flatten them and make them a data frame
    })
  }, .id = "callName")
  callsFlat$workflow_id <- workflow_id
  return(callsFlat)
}
