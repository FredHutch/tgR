#' Queries TGR REDCap for dataset annotations
#'
#' @param DAG (Optional) A character vector contining the name(s) of the dtgrata access group(s) for which to request data, or "all" (parameter applies to admin only).
#' @param harmonizedOnly Whether you want only the annotations that are harmonized across all projects (TRUE) or a complete data set (FALSE).
#' @param evenEmptyCols Whether you want even the empty fields in your final data frame (TRUE) or if you just want columns where there is at least one value in the resulting dataset (FALSE).
#' @return Returns a long form data frame containing the requested dataset.
#' @author Amy Paguirigan
#' @details
#' Requires setCreds() to have been run successfully.
#' @export
tgrAnnotate <- function(DAG, harmonizedOnly = FALSE, evenEmptyCols = FALSE) {
  if ("" %in% Sys.getenv(c("REDURI", "TGR"))) {
    stop("You have missing environment variables.  Please setCreds().")}
  else message("Credentials set successfully.")
        tgrData <- suppressMessages(
          a<- REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("TGR"),
          export_data_access_groups = T, guess_type = F)$data %>%
          dplyr::select(-dplyr::ends_with("_complete")))

        if (harmonizedOnly == TRUE) {
          harmfields <- suppressMessages(
            REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("TGR"))$data)
          Keep <- harmfields %>% dplyr::filter(grepl("tgr_*", form_name)==T) %>% dplyr::select(field_name)
          tgrData <- tgrData %>% dplyr::select(dplyr::one_of("molecular_id", "redcap_data_access_group", Keep$field_name))
        }

        tgrData[tgrData == ""] <- NA
    if (missing(DAG)) {message("Returning all data you have access to.")
    } else {
        if (DAG == "all") {message("Returning all data you have access to.")
          message(paste0("DAGs returned: ", paste(unique(tgrData$redcap_data_access_group), collapse = ", ")))
        } else {
          if (DAG %in% tgrData$redcap_data_access_group){
            tgrData <- tgrData %>% dplyr::filter(redcap_data_access_group == DAG)
            message(paste0("DAGs returned: ", paste(unique(tgrData$redcap_data_access_group), collapse = ", ")))
          } else {stop("Invalid DAG or you do not have permissions.")}
        }
    }
    if (evenEmptyCols == F) {
      tgrData <- tgrData %>%
        Filter(function(x)!all(is.na(x)), .) %>%
        Filter(function(x)!all(x==0), .)
    }
    return(tgrData)
}


