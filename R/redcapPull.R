#' Queries all TGR REDCap Projects for metadata
#'
#'
#'
#' @param domain The domain of the data type to return.  Values include: `all`, `specimen`, `assay`, `molecular`
#' @param harmonizedOnly Whether you want only the data that is harmonized across all projects (TRUE) or a complete data set (FALSE).
#' @param evenEmptyCols Whether you want even the empty columns in your final data frame (TRUE) or if you just want columns where there is at least one value in the resulting dataset (FALSE).
#' @param DAG (Optional) A character vector contining the name(s) of the data access group(s) for which to request data, or "all" (parameter applies to admin only).
#' @return Returns a long form data frame containing the requested dataset.
#' @author Amy Paguirigan
#' @details
#' Requires REDCap credentials to be set in the environment.
#' @export
redcapPull <- function(domain = "all", harmonizedOnly = FALSE, evenEmptyCols = FALSE, DAG) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT"))) {
    stop("You have missing environment variables.  Please setCreds().")}
  else print("Credentials set successfully.")

  if (domain %in% c("all", "specimen")) {
        INData <- suppressMessages(
          REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("INT"),
          export_data_access_groups = T)$data %>%
          dplyr::select(-dplyr::ends_with("_complete")))

        if (harmonizedOnly == TRUE) {
          INfields <- suppressMessages(
            REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("INT"))$data)
          INKeep <- INfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
          INData <- INData %>% dplyr::select(dplyr::one_of("biospecimen_id", "redcap_data_access_group", INKeep$field_name))
        }
    }
    if (domain %in% c("all", "assay")) {
        FCData <- suppressMessages(
          REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("FCT"),
          export_data_access_groups = T)$data %>%
          dplyr::select(-dplyr::ends_with("_complete")))

        if (harmonizedOnly == TRUE) {
          FCfields <- suppressMessages(
            REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("FCT"))$data)
          FCKeep <- FCfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
          FCData <- FCData %>% dplyr::select(dplyr::one_of("assay_material_id", "redcap_data_access_group", FCKeep$field_name))
          }
    }
    if (domain %in% c("all", "molecular")) {
        MHData <- suppressMessages(
          REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("MHT"),
          export_data_access_groups = T)$data %>%
          dplyr::select(-dplyr::ends_with("_complete")))

        if (harmonizedOnly == TRUE) {
          MHfields <- suppressMessages(
            REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("MHT"))$data)
          MHKeep <- MHfields %>% dplyr::filter(grepl("min_*", form_name)==T | grepl("data_is*", field_name) == T) %>% dplyr::select(field_name)
          MHData <- MHData %>% dplyr::select(dplyr::one_of("molecular_id", "redcap_data_access_group", MHKeep$field_name))
          }
      }

    alldfs <- list(get0("INData"), get0("FCData"), get0("MHData"))
    dflist <- purrr::discard(alldfs, is.null)
    results <- purrr::reduce(dflist, dplyr::full_join)
    results[results == ""] <- NA
    if (missing(DAG)) {print("Returning all data you have access to.")
    } else {
        if (DAG == "all") {print("Returning all data you have access to.")
          print(paste0("DAGs returned: ", paste(unique(results$redcap_data_access_group), collapse = ", ")))
        } else {
          if (DAG %in% results$redcap_data_access_group){
            results <- results %>% dplyr::filter(redcap_data_access_group == DAG)
            print(paste0("DAGs returned: ", paste(unique(results$redcap_data_access_group), collapse = ", ")))
          } else {stop("Invalid DAG or you do not have permissions.")}
        }
    }
    if (evenEmptyCols == F) {
      results <- results %>%
        Filter(function(x)!all(is.na(x)), .) %>%
        Filter(function(x)!all(x==0), .)
    }
    return(results)
}


