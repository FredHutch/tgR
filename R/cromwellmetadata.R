#' Pull information about recent Cromwell workflow jobs
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
    stop("CROMWELLURL is not set.")
  } else
    print(paste0("Querying cromwell for jobs in the last ", days, " days."))
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
  if (nrow(cromTable) > 0) {
    cromTable <- dplyr::rename(cromTable, "workflow_id" = "id")
    if ("end" %in% colnames(cromTable) == T &
        "start" %in% colnames(cromTable) == T) {
      cromTable$start <-
        as.POSIXct(cromTable$start, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
      cromTable$end <-
        as.POSIXct(cromTable$end, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
      cromTable$submission <-
        as.character(as.POSIXct(cromTable$submission, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 )# because PST/hack)
      cromTable$workflowDuration <-
        round(difftime(cromTable$end, cromTable$start, units = "mins"),
              3)
      cromTable$workflowDuration <-
        as.numeric(cromTable$workflowDuration)
    } else {
      cromTable$workflowDuration <- "NA"
    }
  } else {
    cromTable <- data.frame("workflow_id" = NA,
                            stringsAsFactors = F)
  }
  return(cromTable)
}

#' Pull metadata for a specific Cromwell workflow job
#'
#' Retrieve and process all labels, submission and workflow level metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return metadata for.
#' @param expandSubWorkflows Boolean, whether to expand subworkflows in the results or not, default is F.
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
cromwellWorkflow <- function(workflow_id, expandSubWorkflows = F) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for metadata for workflow id: ", workflow_id))
  }
  if (expandSubWorkflows == F) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=false"
        )
      ), as = "parsed")
  }
  if (expandSubWorkflows == T) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=true"
        )
      ), as = "parsed")
  }
  if (crommetadata$status == "fail") {
    #this is when a workflow itself fails to start
    return(data.frame(
      "workflow_id" = crommetadata$message,
      stringsAsFactors = F
    ))
  } else {
    if ("id" %in% names(crommetadata)) {
      # if a workflow starts then the id will be there
      # if a workflow has a list of labels
      if (is.list(crommetadata$labels) == T) {
        drag <- purrr::pluck(crommetadata, "labels")
        drag <-
          data.frame(purrr::flatten(drag), stringsAsFactors = F)
        drag$workflow_id <-
          gsub("cromwell-", "", drag$cromwell.workflow.id)
        drag$cromwell.workflow.id <- NULL
      } else {
        drag <- data.frame("workflow_id" = workflow_id)
      }
      # Get submission data
      submit <-
        data.frame(purrr::flatten(purrr::pluck(crommetadata, "submittedFiles")), stringsAsFactors = F)
      submit$labels <-
        NULL # why do they have labels here TOO!!?!>!>
      submit$workflow_id <- workflow_id
      # Get remaining workflow level data
      remainder <-
        data.frame(purrr::discard(crommetadata, is.list),
                   stringsAsFactors = F)
      remainder <- dplyr::rename(remainder, "workflow_id" = "id")
      # Get workflow failure data if it exists
      if (crommetadata$status == "Failed") {
        failureData <-
          unlist(purrr::pluck(
            purrr::pluck(crommetadata, "failures", .default = NA),
            "causedBy",
            "message",
            .default = NA
          ))
        if (is.na(failureData) == F) {
          failures <-
            data.frame(failureData[failureData != ""], stringsAsFactors = F)
          failures$workflow_id <- workflow_id
          resultdf <-
            purrr::reduce(list(remainder, drag, submit, failures),
                          dplyr::full_join,
                          by = "workflow_id")
        } # if failures is na, then keep going
        resultdf <-
          purrr::reduce(list(remainder, drag, submit), dplyr::full_join, by = "workflow_id")
      } else {
        resultdf <-
          purrr::reduce(list(remainder, drag, submit), dplyr::full_join, by = "workflow_id")
      }
      #resultdf <- dplyr::mutate_all(resultdf, as.character)
      resultdf$submission <-
        as.character(as.POSIXct(resultdf$submission, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 )# because PST/hack)
      if ("start" %in% colnames(resultdf) == T) {
        # if the workflow has started
        if (is.na(resultdf$start) == F) {
          # and if the value of start is not NA
          resultdf$start <-
            as.POSIXct(resultdf$start, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
        } else {
          # if start is NA, then make sure it's set to NA????  Stupid.
          resultdf$start <- NA
        }
        if ("end" %in% colnames(resultdf) == T) {
          # and if end is present
          if (is.na(resultdf$end) == F) {
            # and it is not NA
            resultdf$end <-
              as.POSIXct(resultdf$end, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
            resultdf <-
              dplyr::mutate(resultdf, workflowDuration = round(difftime(end, start, units = "mins"), 3))
          }
        } else {
          # if end doesn't exist or it is already NA (???), make it and workflowDuration but set to NA
          resultdf$end <- NA
          resultdf$workflowDuration <- NA
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        resultdf$start <- NA
      }
      resultdf <- dplyr::mutate_all(resultdf, as.character)
      resultdf$workflowDuration <-
        as.numeric(resultdf$workflowDuration)
    } else {
      # if id is not in the names, then
      resultdf <-
        data.frame("workflow_id" = "No metadata available.", stringsAsFactors = F)
    }
    return(resultdf)
  }
}

#' Pull metadata for the calls made in a Cromwell workflow job
#'
#' Retrieve and process call metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return call metadata on.
#' @param expandSubWorkflows Boolean, whether to expand subworkflows in the results or not, default is F.
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
cromwellCall <- function(workflow_id, expandSubWorkflows = F) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for call metadata for workflow id: ", workflow_id))
  }
  if (expandSubWorkflows == F) {
  crommetadata <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?expandSubWorkflows=false"
      )
    ), as = "parsed")
  }
  if (expandSubWorkflows == T) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=true"
        )
      ), as = "parsed")
  }
  if(is.character(crommetadata) == T) stop(crommetadata)
  if (is.list(crommetadata$calls) == T) {
    # if the workflow has made calls, nab them
    bob <- purrr::pluck(crommetadata, "calls")
    if (length(bob) > 0) {
      # this is redudant but a better error catch isn't quite clear yet.
      justCalls <- purrr::map(bob, function(callData) {
        purrr::map_dfr(callData, function(shardData) {
          y <-
            purrr::discard(shardData, is.list) # only keep data that isn't a list itself!
          Z <- data.frame(rbind(unlist(y)), stringsAsFactors = F)

          if (shardData$executionStatus == "Failed") {
            failureData <-
              unlist(purrr::pluck(
                purrr::pluck(shardData, "failures", .default = NA),
                "causedBy",
                "message",
                .default = NA
              ))
            if (is.na(failureData) == F) {
              Zf <-
                data.frame(failureData[failureData != ""], stringsAsFactors = F)
              Z <- cbind(Z, Zf)
            }
          }

          if (is.null(shardData$runtimeAttributes) == F) {
            # if there are runtimeAttributes then..
            runTime <-
              purrr::pluck(shardData, "runtimeAttributes") # pull them out
            Z1 <-
              data.frame(rbind(unlist(runTime)), stringsAsFactors = F) # and make into a data frame
            Z <- cbind(Z, Z1) # put those together into just Z
          }
          return(Z)
        })
      }) %>% purrr::map_dfr(., function(x) {
        x
      }, .id = "callName") # melt it all down by callName

      justCalls$workflow_id <-
        workflow_id # This lets you take this output in a map_dfr and just do rbind as the function. ;)
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(justCalls) == T) {
        # if the workflow has started
        justCalls$start <-
          as.POSIXct(justCalls$start, tz = "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
        if ("end" %in% colnames(justCalls) == T) {
          # and if end is present
          justCalls$end <-
            as.POSIXct(justCalls$end, tz = "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
          justCalls <-
            dplyr::mutate(justCalls, callDuration = round(difftime(end, start, units = "mins"), 3))
        } else {
          # if end doesn't exist or it is already NA (???), make it and workflowDuration but set to NA
          justCalls$end <- NA
          justCalls$callDuration <- NA
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        justCalls$start <- NA
      }
      justCalls <- dplyr::mutate_all(justCalls, as.character)
      justCalls$callDuration <- as.numeric(justCalls$callDuration)

      ############# Subworkflows ################
    #   if (expandSubWorkflows == T) {
    #     subCalldata <- purrr::keep(bob, is.list)
    #     subWay <- purrr::map(subCalldata, function(x) {
    #       a <- purrr::flatten(x)
    #       b <- purrr::pluck(a, "subWorkflowMetadata")
    #       return(b)
    #     })
    #
    #     subworkflowCallData <- purrr::map_dfr(subWay, function(indSubCalls){
    #
    #     if (is.list(indSubCalls$calls) == T) {
    #       # if the workflow has made calls, nab them
    #       sub <- purrr::pluck(indSubCalls, "calls")
    #       if (length(sub) > 0) {
    #         # this is redudant but a better error catch isn't quite clear yet.
    #         justCallsSub <- purrr::map(sub, function(callData) {
    #           purrr::map_dfr(callData, function(shardData) {
    #             y <- purrr::discard(shardData, is.list) # only keep data that isn't a list itself!
    #             Z <- data.frame(rbind(unlist(y)), stringsAsFactors = F)
    #
    #             if (shardData$executionStatus == "Failed") {
    #               failureData <-
    #                 unlist(purrr::pluck(
    #                   purrr::pluck(shardData, "failures", .default = NA),
    #                   "causedBy",
    #                   "message",
    #                   .default = NA
    #                 ))
    #               if (is.na(failureData) == F) {
    #                 Zf <-
    #                   data.frame(failureData[failureData != ""], stringsAsFactors = F)
    #                 Z <- cbind(Z, Zf)
    #               }
    #             }
    #
    #             if (is.null(shardData$runtimeAttributes) == F) {
    #               # if there are runtimeAttributes then..
    #               runTime <-
    #                 purrr::pluck(shardData, "runtimeAttributes") # pull them out
    #               Z1 <-
    #                 data.frame(rbind(unlist(runTime)), stringsAsFactors = F) # and make into a data frame
    #               Z <- cbind(Z, Z1) # put those together into just Z
    #             }
    #             return(Z)
    #           })
    #
    #         }) %>% purrr::map_dfr(., function(x) {x}, .id = "callName") # melt it all down by subCallName
    #         justCallsSub$subWorkflow_id <- indSubCalls$id
    #         justCallsSub$subWorkflowCallName <- indSubCalls$workflowName
    #         justCallsSub$workflow_id <- workflow_id # This lets you take this output in a map_dfr and just do rbind as the function. ;)
    #         ## Big Chunk of dealing with start and end times.
    #         if ("start" %in% colnames(justCallsSub) == T) {
    #           # if the workflow has started
    #           justCallsSub$start <-
    #             as.POSIXct(justCallsSub$start, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
    #           if ("end" %in% colnames(justCallsSub) == T) {
    #             # and if end is present
    #             justCallsSub$end <-
    #               as.POSIXct(justCallsSub$end, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
    #             justCallsSub <-
    #               dplyr::mutate(justCallsSub, callDuration = round(difftime(end, start, units = "mins"), 3))
    #           } else {
    #             # if end doesn't exist or it is already NA (???), make it and workflowDuration but set to NA
    #             justCallsSub$end <- NA
    #             justCallsSub$callDuration <- NA
    #           }
    #         } else {
    #           # if start doesn't exist, then create it and set it to NA
    #           justCallsSub$start <- NA
    #         }
    #         justCallsSub <- dplyr::mutate_all(justCallsSub, as.character)
    #         justCallsSub$callDuration <- as.numeric(justCallsSub$callDuration)
    #         }
    #     }
    #       justCallsSub$subWorkflowName <- indSubCalls$workflowName
    #       return(justCallsSub)
    #   })
    #
    # justCalls <- dplyr::full_join(justCalls, justCallsSub)
    #
    #
    # }
    #  }
  } else {
      justCalls <-
        data.frame("workflow_id" = "No call metadata available.", stringsAsFactors = F)
    }
  } else {
    justCalls <-
      data.frame("workflow_id" = "No call metadata available.", stringsAsFactors = F)
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
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for failure metadata for workflow id: ", workflow_id))
  }
  cromfail <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?includeKey=failures&includeKey=jobId"
      )
    ), as = "parsed")
  if (is.list(cromfail$calls) == T) {
    bobfail <- purrr::pluck(cromfail, "calls")
    if (length(bobfail) > 0) {
      faildf <- purrr::map(bobfail, function(callData) {
        purrr::map_dfr(callData, function(shardData) {
          Z <- data.frame(rbind(unlist(shardData)), stringsAsFactors = F)
        })
      }) %>% purrr::map_dfr(., function(x) {
        x
      }, .id = "callName")
      faildf$workflow_id <- workflow_id
      # this section creates a URL for where the stderr log for the failed jobs likely are.
      # This is hardcoded for ALP right now b/c it's not part of what Cromwell returns.
      temp1 <- data.frame(do.call('rbind',strsplit(faildf$callName, split = "[.]")))
      colnames(temp1)<- c("workflowName", "call")
      faildf <- cbind(faildf, temp1)
      faildf$stderrPrefix <- paste0("cromwell-output/",
                                faildf$workflowName,"/", faildf$workflow_id, "/call-", faildf$call,
                                "/shard-", faildf$shardIndex, "/", faildf$call, "-", faildf$shardIndex,
                                "-stderr.log")

      if ("failures.message" %in% colnames(faildf)) {
        faildf <- dplyr::filter(faildf, is.na(failures.message) == F)
      } else {
        faildf <- faildf[0,]
      }
    } else {
      faildf <-
        data.frame("workflow_id" = "No failure metadata available.", stringsAsFactors = F)
    }
  } else {
    faildf <-
      data.frame("workflow_id" = "No failure metadata available.", stringsAsFactors = F)
  }
  return(faildf)
}

#' Pull Cromwell Call Caching Data
#'
#' Gets info about call caching status for the calls of a workflow
#'
#' @param workflow_id The workflow ID to return call caching metadata for.
#' @param expandSubWorkflows Boolean, whether to expand subworkflows in the results or not, default is F.
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
cromwellCache <- function(workflow_id, expandSubWorkflows = F) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0(
      "Querying for call caching metadata for workflow id: ",
      workflow_id
    ))
  }
  if (expandSubWorkflows == F) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=false"
        )
      ), as = "parsed")
  }
  if (expandSubWorkflows == T) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=true"
        )
      ), as = "parsed")
  }

  if (length(crommetadata$calls) > 0) {
    # if there are calls to be queried, continue
    bobCalls <-
      purrr::pluck(crommetadata, "calls") # we only want the calls data from the metadata for this workflow
    bobCallMeta <-
      purrr::map(bobCalls, function(callData) {
        # for each of the calls in the workflow...
        purrr::map_dfr(callData , function(shardData) {
          # and for each of the shards in that workflow...
          if ("inputs" %in% names(shardData) == T) {
            a <-
              purrr::keep(shardData,
                          names(shardData) %in% c("callCaching", "inputs", "outputs")) # select only these lists
            b <-
              data.frame(rbind(unlist(a)), stringsAsFactors = F) # flatten them and make them a data frame
            b$shardIndex <-
              shardData$shardIndex # add the shard Index associated
          } else {
            b <-
              data.frame("shardIndex" = shardData$shardIndex,
                         stringsAsFactors = F)
          }
          b$shardIndex <- as.character(b$shardIndex)
          b$executionStatus <- shardData$executionStatus # and this
          b$returnCode <- shardData$returnCode # and this
          #b$returnCode <- as.character(b$returnCode)
          b$jobId <- shardData$jobId # and especially this
          b <-
            b %>% dplyr::select(-dplyr::starts_with("callCaching.hitFailures")) # then remove any data from the messy hitFailures lists
          return(b)
        })
      })
    geocache <- purrr::map_dfr(bobCallMeta, rbind, .id = "callName")
  } else {
    geocache <-
      data.frame("workflow_id" = "No call caching metadata available.", stringsAsFactors = F)
  }
  return(geocache)
}

#' Pull a glob of metadata for a specific Cromwell workflow job
#'
#' Retrieve a glob of workflow level metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return metadata for.
#' @param expandSubWorkflows Boolean, whether to expand subworkflows in the results or not, default is F.
#' @return Returns a gross list of lists of metadata on a workflow.
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
#' workflowMeta <- cromwellGlob(workflow_id = thisWorkflowID)
#' @export
cromwellGlob <- function(workflow_id, expandSubWorkflows = F) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for metadata for workflow id: ", workflow_id))
  }
  if (expandSubWorkflows == F) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=false"
        )
      ), as = "parsed")
  }
  if (expandSubWorkflows == T) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=true"
        )
      ), as = "parsed")
  }
  return(crommetadata)
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
