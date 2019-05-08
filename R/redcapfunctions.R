#' Queries all TGR REDCap Projects for metadata
#'
#'
#'
#' @param DAG A character vector contining the name(s) of the data access group(s) for which to request data. Current DAG options include: 'paguirigana', 'aberger'.
#' @param domain The domain of the data type to return.  Values include: `all`, `specimen`, `assay`, `molecular`
#' @param harmonizedOnly Whether you want only the data that is harmonized across all projects (TRUE) or a complete data set (FALSE).
#' @param evenEmptyCols Whether you want even the empty columns in your final data frame (TRUE) or if you just want columns where there is at least one value in the resulting dataset (FALSE).
#' @return Returns a long form data frame containing the requested dataset.
#' @author Amy Paguirigan
#' @details
#' Requires REDCap credentials to be set in the environment.
#' @export
redcapPull <- function(DAG = c("paguirigana", "bergera"), domain = "all", harmonizedOnly = FALSE, evenEmptyCols = FALSE) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT"))) {
    print("You have missing environment variables.  Please setCreds().")} else print("Credentials set successfully.")
    if (domain %in% c("all", "specimen")) {
        INData <- REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("INT"),
          export_data_access_groups = T)$data %>%
          dplyr::filter(redcap_data_access_group %in% DAG) %>% dplyr::select(-dplyr::ends_with("_complete"))

        if (harmonizedOnly == TRUE) {
          INfields <- REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("INT"))$data
          INKeep <- INfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
          INData <- INData %>% dplyr::select(dplyr::one_of(INKeep$field_name, "biospecimen_id"))
        }
    }
    if (domain %in% c("all", "assay")) {
        FCData <- REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("FCT"),
          export_data_access_groups = T)$data %>%
          dplyr::filter(redcap_data_access_group %in% DAG) %>% dplyr::select(-dplyr::ends_with("_complete"))
        FCData$survival_time <- NULL

        if (harmonizedOnly == TRUE) {
          FCfields <- REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("FCT"))$data
          FCKeep <- FCfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
          FCData <- FCData %>% dplyr::select(dplyr::one_of(FCKeep$field_name, "assay_material_id"))
          }
    }
    if (domain %in% c("all", "molecular")) {
        MHData <- REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("MHT"),
          export_data_access_groups = T)$data %>%
          dplyr::filter(redcap_data_access_group %in% DAG) %>% dplyr::select(-dplyr::ends_with("_complete"))

        if (harmonizedOnly == TRUE) {
          MHfields <- REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("MHT"))$data
          MHKeep <- MHfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
          MHData <- MHData %>% dplyr::select(dplyr::one_of(MHKeep$field_name, "molecular_id"))
          }
      }

    alldfs <- list(get0("INData"), get0("FCData"), get0("MHData"))
    dflist <- purrr::discard(alldfs, is.null)
    results <- purrr::reduce(dflist, dplyr::full_join)
    results[results == ""] <- NA
    if (evenEmptyCols == F) {
      results <- results %>% Filter(function(x)!all(is.na(x)), .) %>% Filter(function(x)!all(x==0), .) %>% Filter(function(x)!all(x==""), .) %>% Filter(function(x)!all(x=="noannot"), .)
    }


    return(results)
}
#' Retrieve an example data dictionary from the TGR REDCap projects
#'
#'
#' @param project A string for which REDCap project to return data dictionary for.
#' @return Nothing.  Creates character vectors containing the `categorical` annotations, the `truefalse` annotations, the union of these `fieldList`, and all columns in `summarizeList`.
#' @author Amy Paguirigan
#' @details
#' Requires **admin** REDCap credentials to be set in the environment.
# #' @export
getDictionary <- function(project) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")
  if (project == "specimen") {
    library(RCurl)
    result <- jsonlite::fromJSON(postForm(
      uri=Sys.getenv("REDURI"),
      token=Sys.getenv("INT"),
      content='metadata',
      format='json',
      returnFormat='json'
    ))
  }

  tobeFilled <- data.frame()
  # Get representative actual column names from REDCap by pulling one dataset

  FCData <- REDCapR::redcap_read_oneshot(
    Sys.getenv("REDURI"), Sys.getenv("FCT"),
    export_data_access_groups = TRUE, records = "22290")$data
  MHData <- REDCapR::redcap_read_oneshot(
    Sys.getenv("REDURI"), Sys.getenv("MHT"),
    export_data_access_groups = TRUE, records = "M00000001")$data
}

#' Separate a data frame by TGR REDCap Project for upload
#'
#'
#'
#' @param fileStem A character vector that you want the resulting csv's to have as the beginning of the filenames.
#' @return Writes up to 3 csv's ready for upload into the individual REDCap Projects of the TGR to the working directory.
#' @author Amy Paguirigan
#' @details
#' Requires REDCap credentials to be set in the environment.
# #' @export
prepForUpload <- function(fileStem) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT"))) {
    print("You have missing environment variables.  Please setCreds().")} else print("Credentials set successfully.")
  if (is.character(fileStem) && length(fileStem) == 1) {
    INData <- REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("INT"),
      export_data_access_groups = T)$data %>%
      dplyr::filter(redcap_data_access_group %in% DAG) %>% dplyr::select(-dplyr::ends_with("_complete"))

    if (harmonizedOnly == TRUE) {
      INfields <- REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("INT"))$data
      INKeep <- INfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
      INData <- INData %>% dplyr::select(dplyr::one_of(INKeep$field_name, "biospecimen_id"))
    }
  }
  if (domain %in% c("all", "assay")) {
    FCData <- REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("FCT"),
      export_data_access_groups = T)$data %>%
      dplyr::filter(redcap_data_access_group %in% DAG) %>% dplyr::select(-dplyr::ends_with("_complete"))
    FCData$survival_time <- NULL

    if (harmonizedOnly == TRUE) {
      FCfields <- REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("FCT"))$data
      FCKeep <- FCfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
      FCData <- FCData %>% dplyr::select(dplyr::one_of(FCKeep$field_name, "assay_material_id"))
    }
  }
  if (domain %in% c("all", "molecular")) {
    MHData <- REDCapR::redcap_read_oneshot(
      Sys.getenv("REDURI"), Sys.getenv("MHT"),
      export_data_access_groups = T)$data %>%
      dplyr::filter(redcap_data_access_group %in% DAG) %>% dplyr::select(-dplyr::ends_with("_complete"))

    if (harmonizedOnly == TRUE) {
      MHfields <- REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("MHT"))$data
      MHKeep <- MHfields %>% dplyr::filter(grepl("min_*", form_name)==T) %>% dplyr::select(field_name)
      MHData <- MHData %>% dplyr::select(dplyr::one_of(MHKeep$field_name, "molecular_id"))
    }
  }

  alldfs <- list(get0("INData"), get0("FCData"), get0("MHData"))
  dflist <- purrr::discard(alldfs, is.null)
  results <- purrr::reduce(dflist, dplyr::full_join)
  results[results == ""] <- NA
  if (evenEmptyCols == F) {
    results <- results %>% Filter(function(x)!all(is.na(x)), .) %>% Filter(function(x)!all(x==0), .) %>% Filter(function(x)!all(x==""), .) %>% Filter(function(x)!all(x=="noannot"), .)
  }


  return(results)
}


