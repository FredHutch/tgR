#' Pull metadata for recent Cromwell workflow jobs
#'
#' Requests metadata about Cromwell workflow jobs during a time period specified.
#'
#' @param days The number of days of history to return, defaults to 1 day.
#' @return Returns a long form data frame of metadata on workflow jobs submitted to a specific Cromwell instance.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' ## Set credentials from a file with the specified format, called `secrets.R` in path `~/myCreds/`.
#' setCreds(tokenSet = "file", path = "~/myCreds/secrets.R")
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' @export
cromwellJobs <- function(days = 1) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")
    } else print("cromwellJobs(); Querying cromwell for jobs list.")
  beforeNow <- Sys.Date() - round(days, 0)
  cromDat <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/query?submission=",
        beforeNow,
        "T00%3A00Z"
      )
    ))$results
  cromTable <- purrr::map_dfr(cromDat, dplyr::bind_rows)
  if(nrow(cromTable)>0){
  cromTable <- dplyr::rename(cromTable, "workflow_id" = "id")
  if("end" %in% colnames(cromTable)==T& "start" %in% colnames(cromTable)==T) {
    cromTable$start <- as.POSIXct(cromTable$start, "UTC", "%Y-%m-%dT%H:%M:%S")
    cromTable$end <- as.POSIXct(cromTable$end, "UTC", "%Y-%m-%dT%H:%M:%S")
    cromTable$submission <- as.character(as.POSIXct(cromTable$submission, "UTC", "%Y-%m-%dT%H:%M:%S"))
    cromTable$jobDuration <- round(difftime(cromTable$end, cromTable$start, units = "mins"), 3)
  } else (cromTable$jobDuration <- "NA")
  } else (cromTable = setNames(data.frame("No jobs in that time period", stringsAsFactors = F), "workflow_id"))
  return(cromTable)
}

#' Pull metadata for a specific Cromwell workflow job
#'
#' Retrieve and process all labels, submission and workflow level metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return metadata for.
#' @return Returns a long form data frame of metadata on a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' ## Set credentials from a file with the specified format, called `secrets.R` in path `~/myCreds/`.
#' setCreds(tokenSet = "file", path = "~/myCreds/secrets.R")
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' workflowMeta <- cromwellWorkflow(workflow_id = thisWorkflowID)
#' @export
cromwellWorkflow <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")
  } else print("cromwellWorkflow(); Querying cromwell for workflow metadata.")
  crommetadata <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?expandSubWorkflows=false"
      )))
  if (crommetadata$status == "fail") {
    stop(crommetadata$message)
  } else {
      if ("id" %in% names(crommetadata)) {
        #Get Labels
        if (is.list(crommetadata$labels) == T) {
          drag <- purrr::pluck(crommetadata, "labels")
          drag <- as.data.frame(purrr::flatten(drag), stringsAsFactors = F)
          drag$workflow_id <- gsub("cromwell-", "", drag$cromwell.workflow.id)
          drag$cromwell.workflow.id <- NULL
        } else {
          drag <- setNames(data.frame(workflow_id), c("workflow_id"))
        }
        #Get submission data
        submit <- as.data.frame(purrr::flatten(purrr::pluck(crommetadata, "submittedFiles")))
        submit$labels <- NULL
        submit$workflow_id <- workflow_id
        #Get remaining workflow level data
        remainder <- as.data.frame(purrr::discard(crommetadata, is.list))
        remainder <- dplyr::rename(remainder, "workflow_id" = "id")
        # Get workflow failure data if it exists
        if (crommetadata$status == "Failed"){
          failures <- data.frame(rbind(unlist(purrr::pluck(crommetadata, "failures"))))
          failures$workflow_id <- workflow_id
          suppressWarnings(
            resultdf <- purrr::reduce(list(remainder, drag, submit, failures), dplyr::full_join, by = "workflow_id")
          ) # fix this warning suppression later
        } else {
          suppressWarnings(
            resultdf <- purrr::reduce(list(remainder, drag, submit), dplyr::full_join, by = "workflow_id")
          ) # fix this warning suppression later
        }


        resultdf <- dplyr::mutate_all(resultdf, as.character)
        resultdf$submission <- as.character(as.POSIXct(resultdf$submission, "UTC", "%Y-%m-%dT%H:%M:%S"))

        if ("start" %in% colnames(resultdf) == T) {
          if (is.na(resultdf$start) == F) {
            resultdf$start <- as.POSIXct(resultdf$start, "UTC", "%Y-%m-%dT%H:%M:%S")
          } else {
            resultdf$start <- NA
          }
          if ("end" %in% colnames(resultdf) == T) {
            if (is.na(resultdf$end) == F) {
              resultdf$end <- as.POSIXct(resultdf$end, "UTC", "%Y-%m-%dT%H:%M:%S")
              resultdf <- dplyr::mutate(resultdf, workflowDuration = round(difftime(end, start, units = "mins"), 3))
            } else {
              resultdf$workflowDuration <- NA
              resultdf$end <- NA
            }
          } else {
            resultdf <- dplyr::mutate(resultdf,
                                      end = NA ,
                                      workflowDuration = NA)
          }
        } else {resultdf$start <- NA}
      } else
        {resultdf = setNames(data.frame(paste0(
            "There are no available metadata associated with the workflow_id: ",
            workflow_id), stringsAsFactors = F), "workflow_id")}
    }
    return(resultdf)
  }

#' Pull metadata for the calls made in a Cromwell workflow job
#'
#' Retrieve and process call metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return call metadata on.
#' @return Returns a long form data frame of metadata on calls.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' ## Set credentials from a file with the specified format, called `secrets.R` in path `~/myCreds/`.
#' setCreds(tokenSet = "file", path = "~/myCreds/secrets.R")
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' callsMeta <- cromwellCall(workflow_id = thisWorkflowID)
#' @export
cromwellCall <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")
  } else print("cromwellCalls(); Querying cromwell for job calls list.")
  crommetadata <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?expandSubWorkflows=false"
      )))
  callNames <- names(crommetadata$calls)
  if (is.list(crommetadata$calls) == T) {
    bob <- purrr::pluck(crommetadata, "calls")
    if (length(bob) > 0) {
      # this is redudant but a better error catch isn't quite clear yet.
      suppressWarnings(
        justCalls <- purrr::map(bob, function(callData) {
          purrr::map_dfr(callData, function(shardData) {
            y <- purrr::discard(shardData, is.list)
            Z <- as.data.frame(rbind(unlist(y)))
            runTime <- purrr::pluck(shardData, "runtimeAttributes")
            Z1 <- as.data.frame(rbind(unlist(runTime)))
            cbind(Z, Z1)
          })
        }) %>% purrr::map_dfr(., function(x) {x}, .id = "callName")
      ) # Fix the warnings later.
      justCalls$workflow_id <- workflow_id
      if("start" %in% colnames(justCalls)==T) {
        justCalls$start <- as.POSIXct(justCalls$start, "UTC", "%Y-%m-%dT%H:%M:%S")
        if("end" %in% colnames(justCalls)==T) {
          justCalls$end <- as.POSIXct(justCalls$end, "UTC", "%Y-%m-%dT%H:%M:%S")
          justCalls$jobDuration <- round(difftime(justCalls$end, justCalls$start, units = "mins"), 3)
        }
      } else {
        justCalls$jobDuration <- NA
        justCalls$end <- NA}
      #justCalls <- justCalls[,colnames(justCalls) %in% c("workflow_id","callName","shardIndex", "jobId","attempt", "start","end",
      #                       "executionStatus", "returnCode", "stdout", "compressedDockerSize", "backend", "stderr",
      #                       "callRoot", "backendStatus", "commandLine", "dockerImageUsed", "retryableFailure", "jobDuration")]
    } else (justCalls = setNames(
      data.frame(cbind(paste0("There are no calls associated with this workflow id. "), workflow_id), stringsAsFactors = F),
      c("callName", "workflow_id")))
  }
  return(justCalls)
}

#' Pull metadata for the failed calls made in a Cromwell workflow job
#'
#' Gets info about failed calls for a specific workflow
#'
#' @param workflow_id The workflow ID to return call failure metadata for.
#' @return Returns a long form data frame of metadata on failed calls in a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' ## Set credentials from a file with the specified format, called `secrets.R` in path `~/myCreds/`.
#' setCreds(tokenSet = "file", path = "~/myCreds/secrets.R")
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' failsMeta <- cromwellFailures(workflow_id = thisWorkflowID)
#' @export
cromwellFailures <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")
  } else print("cromwellFailures(); Querying cromwell for failed call list.")
  cromfail <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?includeKey=failures&includeKey=jobId"
      )))
  if (is.list(cromfail$calls) == T) {
    bobfail <- purrr::pluck(cromfail, "calls")
    if (length(bobfail) > 0) {
      suppressWarnings(
        faildf <- purrr::map(bobfail, function(callData) {
          purrr::map_dfr(callData, function(shardData) {
            Z <- as.data.frame(rbind(unlist(shardData)))
          })
        }) %>% purrr::map_dfr(., function(x) {x}, .id = "callName")
      ) # Fix the warnings later.
      faildf$workflow_id <- workflow_id
      if ("failures.message" %in% colnames(faildf)) {
        faildf <- dplyr::filter(faildf, is.na(failures.message) == F)
      } else {faildf <- faildf[0, ]}
        #faildf = setNames(data.frame(cbind(paste0(
        #  "There are no failure metadata associated with this workflow id"), workflow_id), stringsAsFactors = F), c("failures.message", "workflow_id"))}
    }
  } else
    faildf = setNames(data.frame(cbind(paste0(
      "There are no failure metadata associated with this workflow id"), workflow_id), stringsAsFactors = F), c("failures.message", "workflow_id"))
  return(faildf)
}

#' Pull Cromwell Call Caching Data
#'
#' Gets info about call caching status for the calls of a workflow
#'
#' @param workflow_id The workflow ID to return call caching metadata for.
#' @return Returns a long form data frame of metadata on call caching in a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' ## Set credentials from a file with the specified format, called `secrets.R` in path `~/myCreds/`.
#' setCreds(tokenSet = "file", path = "~/R/requiredCredentials.R")
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' cacheMeta <- cromwellCache(workflow_id = thisWorkflowID)
#' @export
cromwellCache <- function(workflow_id){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")
    } else print("cromwellCache(); Querying cromwell for call cacheing metadata.")
  crommetadata <- httr::content(httr::GET(paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/",
                                                 workflow_id,"/metadata?expandSubWorkflows=false")), as = "parsed")

  if(is.list(crommetadata$calls)==T){ # if there are calls to be queried, continue
    bobCalls <- purrr::pluck(crommetadata, "calls") # we only want the calls data from the metadata for this workflow
    suppressWarnings(
      bobCallMeta <- purrr::map(bobCalls, function(x){ # for each of the calls in the workflow...
      purrr::map_dfr(x , function(y){ # and for each of the shards in that workflow...
        if ("inputs" %in% names(y) == T) {
        a <- purrr::keep(y, names(y) %in% c("callCaching", "inputs", "outputs")) # select only these lists
        b <- as.data.frame(rbind(unlist(a))) # flatten them and make them a data frame
        b$shardIndex <- y$shardIndex # add the shard Index associated
        } else b <- setNames(data.frame(y$shardIndex), "shardIndex")

        b$executionStatus <- y$executionStatus # and this
        b$returnCode <- y$returnCode # and this
        b$jobId <- y$jobId # and especially this
        b<- b %>% dplyr::select(-dplyr::starts_with("callCaching.hitFailures")) # then remove any data from the messy hitFailures lists
        return(b)
      })
    })
    ) # Fix suppression later
    suppressWarnings(
      cacheHits <- purrr::map_dfr(bobCallMeta, function(x){
        if("callCaching.hit"%in% colnames(x)){
          y <- x %>% dplyr::filter(callCaching.hit == T) %>% Filter(function(x)!all(is.na(x)), .)
        } else {y <- x}# setNames(data.frame(paste0("No Calls were cached for the workflow_id: ", x$workflow_id)), c("callCaching.hit"))}
        return(y)
        }, .id = "callName") %>% dplyr::mutate_if(is.factor, as.character) %>% dplyr::mutate_if(is.logical, as.character)
    ) # Fix suppression later
    cacheHits$workflow_id <- workflow_id

    suppressWarnings(
      cacheMisses <- purrr::map_dfr(bobCallMeta, function(x){
        if("callCaching.hit" %in% colnames(x)){
          y<- x %>% dplyr::filter(callCaching.hit == F) %>% Filter(function(x)!all(is.na(x)), .)
        } else {y <- x}#setNames(data.frame(paste0("No Calls were cached for the workflow_id: ", x$workflow_id)), c("callCaching.hit"))}
        return(y)
    }, .id = "callName") %>% dplyr::mutate_if(is.factor, as.character) %>% dplyr::mutate_if(is.logical, as.character)
    )  # Fix suppression later
    cacheMisses$workflow_id <- workflow_id

    suppressWarnings(
      hitFailures <- purrr::map(bobCalls, function(eachCall){
      listofShardFrames <- purrr::map_dfr(eachCall, function(eachShard){
        c <- purrr::pluck(eachShard, "callCaching")
        if(c$effectiveCallCachingMode == F){
          d <- c[colnames(c) %in% c("shardIndex", "callCaching.allowResultReuse", "callCaching.effectiveCallCachingMode")]}
        else {
        d <- as.data.frame(unlist(purrr::pluck(c, "hitFailures")))
        if(nrow(d)>0){
          colnames(d) <- c("hitFailureMessage")
          d$breadCrumbs <- rownames(d)
          d$shardIndex <- eachShard$shardIndex
          d <- d %>% dplyr::mutate_if(is.factor, as.character)
          d <- d %>% dplyr::mutate_if(is.logical, as.character)
          return(d)
        }
      }})
    }) %>% purrr::map_dfr(., function(x){ x }, .id = "callName") #WTF?? Why does this work but reduce or flatten don't?
    )  # Fix suppression later

    if("shardIndex" %in% colnames(cacheMisses) & "shardIndex" %in% colnames(hitFailures)){
      cacheMisses <- dplyr::full_join(cacheMisses, hitFailures, by = c("callName", "shardIndex"))
    } else {cacheMisses <- dplyr::full_join(cacheMisses, hitFailures, by = c("callName"))}
    geocache <- dplyr::bind_rows(cacheHits, cacheMisses)
    } else {geocache = setNames(data.frame(paste0("There are no calls associated with the workflow_id: ", workflow_id), stringsAsFactors = F), c("workflow_id"))}
  return(geocache)
}
#' Submit a workflow job to Cromwell
#'
#' Supports the submission of a fully defined workflow job to a Cromwell instance.
#'
#' @param WDL Local path to the wdl file describing the workflow.
#' @param Params Local path to the json containing the parameters to use with the workflow.
#' @param Batch Local path to the json containing a reference to any batch file desired if the workflow is a batch.
#' @param Options Local path to the json containing workflow options to apply.
#' @param Labels A data frame containing the labels for this workflow.
#' @return Returns the response from the API post which includes the workflow ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellSubmitBatch <- function(WDL, Params, Batch, Options, Labels){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")
    } else print("Submitting a batch workflow to Cromwell.")
  cromDat <- httr::POST(url = paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1"),
                        body = list(wdlSource = httr::upload_file(WDL),
                                    workflowInputs = httr::upload_file(Params),
                                    workflowInputs_2 = httr::upload_file(Batch),
                                    labels = jsonlite::toJSON(as.list(Labels), auto_unbox = TRUE),
                                    workflowOptions = httr::upload_file(Options)),
                        encode = "multipart")
  cromResponse <- data.frame(httr::content(cromDat), stringsAsFactors = F)
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
cromwellAbort <- function(workflow_id){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")
    } else print("Aborting job in Cromwell.")
  cromDat <- httr::POST(url = paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/", workflow_id, "/abort"))
  cromResponse <- data.frame(httr::content(cromDat))
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
cromwellOutputs <- function(workflow_id){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    print("The cromwell URL is not set.  Please setCromwellURL().")
    } else print("Getting list of outputs from Cromwell.")
  cromDat <- httr::GET(url = paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/", workflow_id, "/outputs"))
  cromResponse <- httr::content(cromDat, as = "parsed")
  if(length(cromResponse$outputs)>0){
    outputsDf <- purrr::map_dfr(purrr::map(cromResponse$outputs, unlist), cbind)
    outputsDf$workflow_id <- workflow_id
  } else {print("No outputs are available for this workflow.")}
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
cromwellLogs <- function(workflow_id){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    print("The cromwell URL is not set.  Please setCromwellURL().")
    }  else print("Getting list of logs from Cromwell.")
  cromDat <- httr::GET(url = paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/", workflow_id, "/logs"))
  cromResponse <- httr::content(cromDat, as = "parsed")
  calls <- purrr::pluck(cromResponse, "calls")
  callsFlat <- purrr::map_dfr(calls, function(x){
    justcalls <- purrr::map_dfr(x, function(s){
      shard <- data.frame(rbind(unlist(s)), stringsAsFactors = F) # flatten them and make them a data frame
    })
  }, .id = "callName")
  callsFlat$workflow_id <- workflow_id
  return(callsFlat)
}

# ## Mongodb query for AWS Batch
# batchQuery <- function(callDat) {
#   require(mongolite); require(dplyr)
#   a <- mongo(collection = "events", url = mongoBatchURL, verbose = TRUE)
#   awsBatchMeta <- a$find(query = '{"jobQueue":"arn:aws:batch:us-west-2:064561331775:job-queue/cromwell-1999"}',
#                           fields = '{"jobName":1, "jobId":1,"status":1, "timestamp":1, "statusReason":1}')
#   awsBatchMeta <- awsBatchMeta %>% group_by(jobId) %>% arrange(desc(timestamp))
#   batchDat <- left_join(awsBatchMeta, callDat)
#   #mostRecentBatch <<- awsBatchMeta %>% select(-"_id") %>% group_by(jobName, jobId) %>%
#   #  filter(timestamp == max(timestamp)) %>% arrange(desc(timestamp))
#   return(batchDat)
# }

