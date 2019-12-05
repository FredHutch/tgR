#' Request next subsequent molecular id in the TGR
#' @description Requests and initializes a new record in REDCap in the specified Data Access Group (DAG)
#' @param DAG Data access group to assign the new record to.
#' @return An empty data frame with the new identifier and it's data access group, ready for additions and upload to REDCap.  Also initializes the record in REDCap.
#' @author Amy Paguirigan
#' @details Requires valid admin credentials to be set in the environment with setCreds()
#' @export
getTGRid <- function(DAG = NULL) {
    if ("" %in% Sys.getenv(c("REDURI", "S3A", "S3SA", "TGR"))) {
        stop("You have missing environment variables.  Please set creds in env vars.")}
    else message("Credentials set successfully.")
    
    if (is.null(DAG) == TRUE) {
       stop("Please specify a valid Data Access Group (DAG) for this record.")
        } else {
         usedIDs <- suppressMessages(REDCapR::redcap_read_oneshot(
            Sys.getenv("REDURI"), Sys.getenv("TGR"), fields = "molecular_id",
            export_data_access_groups = TRUE, guess_type = F)$data)
         if (DAG %in% usedIDs$redcap_data_access_group == F) {
             stop("This DAG is invalid.")
             return()
         } else {
             sorted <- dplyr::arrange(usedIDs, molecular_id)
             numerics <- as.numeric(gsub("M", "", sorted$molecular_id))
             nextRecord <- max(numerics)+1
             newID <- paste0("M",stringr::str_pad(nextRecord, 8, pad = "0"))
             
             message(paste0("New ID is: ", newID))
             stub <- data.frame("molecular_id" = newID, "redcap_data_access_group" = DAG)
             initializeResults <- REDCapR::redcap_write_oneshot(stub, redcap_uri = Sys.getenv("REDURI"), token = Sys.getenv("TGR"))
             return(list("dataframe" = stub, "initializeResults" = initializeResults))
         }
    }
}

