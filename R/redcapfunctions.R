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
    print("You have missing environment variables.  Please setCreds().")} else print("Credentials set successfully.")
  if (domain %in% c("all", "specimen")) {
        INData <- REDCapR::redcap_read_oneshot(
          Sys.getenv("REDURI"), Sys.getenv("INT"),
          export_data_access_groups = T)$data %>%
          dplyr::select(-dplyr::ends_with("_complete"))

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
          dplyr::select(-dplyr::ends_with("_complete"))

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
          dplyr::select(-dplyr::ends_with("_complete"))

        if (harmonizedOnly == TRUE) {
          MHfields <- REDCapR::redcap_metadata_read(Sys.getenv("REDURI"), Sys.getenv("MHT"))$data
          MHKeep <- MHfields %>% dplyr::filter(grepl("min_*", form_name)==T | grepl("data_is*", field_name) == T) %>% dplyr::select(field_name)
          MHData <- MHData %>% dplyr::select(dplyr::one_of(MHKeep$field_name, "molecular_id"))
          }
      }

    alldfs <- list(get0("INData"), get0("FCData"), get0("MHData"))
    dflist <- purrr::discard(alldfs, is.null)
    results <- purrr::reduce(dflist, dplyr::full_join)
    results[results == ""] <- NA
    if (missing(DAG)) {print("Returning all data you have access to.")
    } else {
        if (DAG == "all") {print("Returning all data you have access to.")
        } else {
          if (DAG %in% results$redcap_data_access_group){
            results <- results %>% dplyr::filter(redcap_data_access_group == DAG)
          } else {print("Invalid DAG or you do not have permissions.")}
        }
    }
    if (evenEmptyCols == F) {
      results <- results %>%
        Filter(function(x)!all(is.na(x)), .) %>%
        Filter(function(x)!all(x==0), .)
    }
    return(results)
}
#' Retrieve an example data dictionary from the TGR REDCap projects
#'
#'
#' @param project A string for which REDCap project to return data dictionary for, such as "specimen", "assay", or "molecular".
#' @return An array of field names for the project specified.
#' @author Amy Paguirigan
#' @details
#' Requires REDCap credentials to be set in the environment.
#' @export
getDictionary <- function(project) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT", "S3A", "S3SA"))) {
    print("You have missing environment variables.  Please set creds in env vars.")} else print("Credentials set successfully.")
  if (project == "specimen") {
    result <- jsonlite::fromJSON(RCurl::postForm(
      uri=Sys.getenv("REDURI"),
      token=Sys.getenv("INT"),
      content='metadata',
      format='json',
      returnFormat='json'
    ))
  }
  if (project == "assay") {
    result <- jsonlite::fromJSON(RCurl::postForm(
      uri=Sys.getenv("REDURI"),
      token=Sys.getenv("FCT"),
      content='metadata',
      format='json',
      returnFormat='json'
    ))
  }
  if (project == "molecular") {
    result <- jsonlite::fromJSON(RCurl::postForm(
      uri=Sys.getenv("REDURI"),
      token=Sys.getenv("MHT"),
      content='metadata',
      format='json',
      returnFormat='json'
    ))
  }
  fields <- result$field_name
  return(fields)

}

#' Separate a data frame by TGR REDCap Project for upload
#'
#'
#'
#' @param fileStem A character vector that you want the resulting csv's to have as the beginning of the filenames.
#' @param dataToSplit A data frame WITHOUT rownames, containing the data you want to split and upload. Note:  For a valid dataset to be uploaded to REDCap, any relevant Repository identifier needs to be included in the data frame.
#' @return Writes up to 3 csv's ready for upload into the individual REDCap Projects of the TGR to the working directory.
#' @author Amy Paguirigan
#' @details
#' Requires REDCap credentials to be set in the environment.  Does not allow for changes in DAG or marking if an instrument is complete.
#' @export
prepForUpload <- function(fileStem, dataToSplit) {
  if ("" %in% Sys.getenv(c("REDURI", "INT", "FCT", "MHT"))) {
    print("You have missing environment variables.  Please use setCreds().")} else print("Credentials set successfully.")
  if (is.character(fileStem) & length(fileStem) == 1) {
    if (is.data.frame(dataToSplit) & nrow(dataToSplit) > 0) {

      specimen <- dataToSplit %>% dplyr::select(dplyr::one_of(getDictionary(project = "specimen"))) %>%
        unique() %>% dplyr::filter(biospecimen_id != "")
      if(ncol(specimen) > 0) {
      if ("biospecimen_id" %in% colnames(specimen)){
        write.csv(specimen, file = paste0(fileStem, "-TGBiospecimens.csv"), row.names = F, na = "")
        print(paste0("Writing File: ", paste0(fileStem, "-TGBiospecimens.csv")))
      } else { print("biospecimen_id is required for this upload.")}
      }

      assay <- dataToSplit %>% dplyr::select(dplyr::one_of(getDictionary(project = "assay"))) %>%
        unique() %>% dplyr::filter(assay_material_id != "")
      if(ncol(assay) > 0) {
      if ("assay_material_id" %in% colnames(assay)){
        write.csv(assay, file = paste0(fileStem, "-TGAssayMaterials.csv"), row.names = F, na = "")
        print(paste0("Writing File: ", paste0(fileStem, "-TGAssayMaterials.csv")))
      } else { print("assay_material_id is required for this upload.")}
      }

      molecular <- dataToSplit %>% dplyr::select(dplyr::one_of(getDictionary(project = "molecular"))) %>%
        unique() %>% dplyr::filter(molecular_id != "")
      if(ncol(molecular) > 0) {
      if ("molecular_id" %in% colnames(molecular)){
        write.csv(molecular, file = paste0(fileStem, "-TGMolecularDatasets.csv"), row.names = F, na = "")
        print(paste0("Writing File: ", paste0(fileStem, "-TGMolecularDatasets.csv")))
      } else { print("molecular_id is required for this upload.")}
      }

    } else {print("Please provide a data frame with more than 0 rows to split for uploading.")}
  } else {print("Please provide a single `fileStem` character string for your files.")}

}

