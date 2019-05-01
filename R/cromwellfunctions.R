#' Pull Cromwell Workflow Jobs
#'
#' Requests metadata about Cromwell Workflow Jobs during a time period specified.
#'
#' @param days The number of days of history to return, defaults to 1 day.
#' @return Returns a long form data frame of metadata on workflow jobs submitted
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @export
cromwellJobs <- function(days = 1){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")} else print("Cromwell URL set successfully.")
  print("cromwellJobs(); Querying cromwell for jobs list.")
  beforeNow <- Sys.Date()-round(days,0)
  cromDat <- httr::content(httr::GET(paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/query?submission=", beforeNow, "T00%3A00Z")))$results
  cromTable <- purrr::map_dfr(cromDat, dplyr::bind_rows)
  if(nrow(cromTable)>0){
  cromTable <- dplyr::rename(cromTable, "workflow_id" = "id")
  if("end" %in% colnames(cromTable)==T& "start" %in% colnames(cromTable)==T) {
    cromTable$jobDuration <- as.character(difftime(cromTable$end, cromTable$start, units = "mins"))
  } else (cromTable$jobDuration <- "NA")
  } else (cromTable = as.data.frame("No jobs in that time period"))
  return(cromTable)
}

#' Pull Cromwell Workflow Calls
#'
#' Retrieve and process call data for a workflow
#'
#' @param workflow_id The workflow ID to return call metadata on.
#' @return Returns a long form data frame of metadata on calls.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @export
cromwellCall <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")} else print("Cromwell URL set successfully.")
  crommetadata <- httr::content(httr::GET(paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/",
                                                 workflow_id,"/metadata?expandSubWorkflows=false")))
  jobdf <- as.data.frame(c())
  callNames <- names(crommetadata$calls)
  if(is.list(crommetadata$calls)==T){
    bob <- unlist(crommetadata$calls, recursive = FALSE, use.names = TRUE)
    if (length(bob) > 0) {
      stanley <- purrr::map(bob, function(x) {purrr::flatten(x)})
      simpleStan <- purrr::map(names(stanley), function(i) {
        x <- stanley[[i]]
        y <- purrr::discard(x, is.list)
        Y <- purrr::map_dfc(y, cbind)
        Y$workflow_id <- workflow_id
        Y$callName <- i
        Y <- dplyr::mutate_all(Y, as.character)
        if("end" %in% colnames(Y)==T & "start" %in% colnames(Y)==T) {
          Y$jobDuration <- as.character(difftime(Y$end, Y$start, units = "mins"))
        } else {Y$jobDuration <- "NA"}
        as.data.frame(Y)})
      names(simpleStan) <- names(stanley)
      jobdf <- purrr::reduce(simpleStan, dplyr::bind_rows)
      #If a callNames value is in the string in callName, then replace that value with THAT callNames value
      jobdf$callName <- gsub("[0-9]*$", "", jobdf$callName)
    }
    } else(jobdf = as.data.frame(paste0("There are no calls associated with the workflow_id: ", workflow_id)))
  return(jobdf)
}

#' Pull Cromwell Workflow Metadata
#'
#' Retrieve and process all labels, submission and workflow level metadata for a workflow
#'
#' @param workflow_id The workflow ID to return metadata for.
#' @return Returns a long form data frame of metadata on a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @export
cromwellWorkflow <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")} else print("Cromwell URL set successfully.")
  crommetadata <- httr::content(httr::GET(paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/",
                                                 workflow_id,"/metadata?expandSubWorkflows=false")))
  resultdf <- as.data.frame(c())
  #Get Labels
  if(is.list(crommetadata$labels)==T){
    drag <- as.data.frame(purrr::flatten(crommetadata$labels), stringsAsFactors = F)
    drag$workflow_id <- gsub("cromwell-", "", drag$cromwell.workflow.id)
    drag$cromwell.workflow.id <- NULL
    #Get submission data
    submit <- as.data.frame(purrr::flatten(crommetadata$submittedFiles))
    submit$labels <- NULL
    submit$workflow_id <- workflow_id
    #Get workflow level data
    remainder <- as.data.frame(purrr::discard(crommetadata, is.list))
    remainder <- dplyr::rename(remainder, "workflow_id"="id")
    resultdf <- purrr::reduce(list(remainder, drag, submit), dplyr::full_join, by = "workflow_id")
    if ("end" %in% colnames(resultdf) == T & "start" %in% colnames(resultdf)==T ) {
      resultdf <- dplyr::mutate(resultdf, workflowDuration = as.character(difftime(end, start, units = "mins")))
    } else {resultdf <- dplyr::mutate(resultdf, end = "NA" , workflowDuration = "NA")}
    } else(resultdf = as.data.frame(paste0("There are no available metadata associated with the workflow_id: ", workflow_id)))
  return(resultdf)
}
#' Pull Cromwell Call Failure Data
#'
#' Gets info about failed jobs for a workflow
#'
#' @param workflow_id The workflow ID to return call failure metadata for.
#' @return Returns a long form data frame of metadata on failed calls in a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid cromwell URL to be set in the environment.
#' @export
#
cromwellFailures <- function(workflow_id){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("The cromwell URL is not set.  Please setCromwellURL().")} else print("Cromwell URL set successfully.")
  cromfail <- httr::content(httr::GET(paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/",
                                             workflow_id,"/metadata?includeKey=failures&includeKey=jobId")))
  faildf <- as.data.frame(c())

  if(is.list(cromfail$calls)==T){
    callNames <- names(cromfail$calls)
    bobfail <- purrr::pluck(cromfail, "calls")
    if (length(bobfail) > 0) {
      faildf <- purrr::map(bobfail, function(callData) {
        purrr::map_dfr(callData, function(shardData){
          Z <- as.data.frame(rbind(unlist(shardData)))
        })
      }) %>% purrr::map_dfr(., function(x){x}, .id = "callName")
      faildf$workflow_id <- workflow_id
      if ("failures.message" %in% colnames(faildf)) {
        faildf <- dplyr::filter(faildf,is.na(failures.message) == F)} else {
          faildf <- faildf[0,]
        }
    }
  } else(faildf = as.data.frame(paste0("There are no failure metadata associated with the workflow_id: ", workflow_id)))
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
#' @export
#
cromwellCache <- function(workflow_id){
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    print("The cromwell URL is not set.  Please setCromwellURL().")} else print("Cromwell URL set successfully.")
  crommetadata <- httr::content(httr::GET(paste0(Sys.getenv("CROMWELLURL"),"/api/workflows/v1/",
                                                 workflow_id,"/metadata?expandSubWorkflows=false")), as = "parsed")
  geocache <- as.data.frame(c())
  if(is.list(crommetadata$calls)==T){ # if there are calls to be queried, continue
    bobCalls <- purrr::pluck(crommetadata, "calls") # we only want the calls data from the metadata for this workflow
    bobCallMeta <- purrr::map(bobCalls, function(x){ # for each of the calls in the workflow...
      purrr::map_dfr(x , function(y){ # and for each of the shards in that workflow...
        a <- purrr::keep(y, names(y) %in% c("callCaching", "inputs", "outputs")) # select only these lists
        b <- as.data.frame(rbind(unlist(a))) # flatten them and make them a data frame
        b$shardIndex <- y$shardIndex # add the shard Index associated
        b$executionStatus <- y$executionStatus # and this
        b$returnCode <- y$returnCode # and this
        b$jobId <- y$jobId # and especially this
        b<- b %>% dplyr::select(-dplyr::starts_with("callCaching.hitFailures")) # then remove any data from the messy hitFailures lists
        return(b)
      })
    })
    cacheHits <- purrr::map_dfr(bobCallMeta, function(x){
      x %>% dplyr::filter(callCaching.hit == T) %>% Filter(function(x)!all(is.na(x)), .)
    }, .id = "callName")
    cacheHits$workflow_id <- workflow_id
    cacheMisses <- purrr::map_dfr(bobCallMeta, function(x){
      x %>% dplyr::filter(callCaching.hit == F) %>% Filter(function(x)!all(is.na(x)), .)
    }, .id = "callName")
    cacheMisses$workflow_id <- workflow_id

    hitFailures <- purrr::map(bobCalls, function(eachCall){
      listofShardFrames <- purrr::map_dfr(eachCall, function(eachShard){
        c <- purrr::pluck(eachShard, "callCaching")
        d <- as.data.frame(unlist(purrr::pluck(c, "hitFailures")))
        if(nrow(d)>0){
          colnames(d) <- c("hitFailureMessage")
          d$breadCrumbs <- rownames(d)
          d$shardIndex <- eachShard$shardIndex
          return(d)
        }
      })
    }) %>% purrr::map_dfr(., function(x){ x }, .id = "callName") #WTF?? Why does this work but reduce or flatten don't?

    cacheMisses <- dplyr::full_join(cacheMisses, hitFailures, by = c("callName", "shardIndex"))
    geocache <- dplyr::bind_rows(cacheHits, cacheMisses)
    } else(geocache = as.data.frame(paste0("There are no calls associated with the workflow_id: ", workflow_id)))
  return(geocache)
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

