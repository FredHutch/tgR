#' Validate patient, specimen or assay material level datasets
#'
#' @param validation_id The name of the id you want to validate the dataset with respect to:  subject_id, biospecimen_id or assay_material.
#' @return A data frame containing all of the records for which there are one or more mismatches between records with matching validation_ids.
#' @author Amy Paguirigan
#' @details
#' Requires setCreds() to have been run successfully.
#' @export
validateDataset <- function(validation_id = NULL){
  if ("" %in% Sys.getenv(c("REDURI", "TGR"))) {
    stop("You have missing environment variables.  Please setCreds().")}
  else message("Credentials set successfully.")
  forms <- REDCapR::redcap_metadata_read(redcap_uri = Sys.getenv("REDURI"),
                                         token=Sys.getenv("TGR"),
                                         verbose = FALSE)$data

  ## Group by the validation_id column and then test to see if the appropriate other columns are identical for all groups,
  ## then return any of thsoe that aren't.
  if (validation_id == "subject_id"){
    fieldstoPull <- forms[forms$form_name %in% c("tgr_entry_ids", "tgr_ids", "tgr_duo", "tgr_patient", "outcomes"),]$field_name
    tgrData <- suppressMessages(
      REDCapR::redcap_read_oneshot(
        Sys.getenv("REDURI"), Sys.getenv("TGR"),
        export_data_access_groups = T,
        fields = fieldstoPull, guess_type = F)$data %>%
        dplyr::select(-dplyr::ends_with("_complete")))
    tgrData[tgrData == ""] <- NA
    tobeValidated <- tgrData %>% dplyr::select(-c("ext_biospecimen_id", "biospecimen_id", "assay_material_id", "molecular_id"))
    badrecords <- tobeValidated %>% dplyr::group_by(subject_id) %>% dplyr::distinct()%>% dplyr::filter(dplyr::n()>1)
    misMatches <- tgrData %>% dplyr::filter(subject_id %in% badrecords$subject_id) %>% dropWhen()
    misMatches <- dplyr::arrange(misMatches, subject_id)
  } else if (validation_id == "biospecimen_id"){
    fieldstoPull <- forms[forms$form_name %in% c("tgr_entry_ids", "tgr_ids","tgr_specimen_acquisition", "biomarkersleukemia"),]$field_name
    tgrData <- suppressMessages(
      REDCapR::redcap_read_oneshot(
        Sys.getenv("REDURI"), Sys.getenv("TGR"),
        export_data_access_groups = T,
        fields = fieldstoPull, guess_type = F)$data %>%
        dplyr::select(-dplyr::ends_with("_complete")))
    tgrData[tgrData == ""] <- NA
    tobeValidated <- tgrData %>% dplyr::select(-c("subject_id", "assay_material_id", "molecular_id"))

    badrecords <- tobeValidated %>% dplyr::group_by(biospecimen_id) %>% dplyr::distinct() %>% dplyr::filter(dplyr::n()>1)
    misMatches <- tgrData %>% dplyr::filter(biospecimen_id %in% badrecords$biospecimen_id) %>% dropWhen()
    misMatches <- dplyr::arrange(misMatches, biospecimen_id)
  } else if (validation_id == "assay_material_id"){
    fieldstoPull <- forms[forms$form_name %in% c("tgr_entry_ids", "tgr_ids","tgr_laboratory"),]$field_name
    tgrData <- suppressMessages(
      REDCapR::redcap_read_oneshot(
        Sys.getenv("REDURI"), Sys.getenv("TGR"),
        export_data_access_groups = T,
        fields = fieldstoPull, guess_type = F)$data %>%
        dplyr::select(-dplyr::ends_with("_complete")))
    tgrData[tgrData == ""] <- NA
    tobeValidated <- tgrData %>% dplyr::select(-c("subject_id", "biospecimen_id", "ext_biospecimen_id", "molecular_id"))
    badrecords <- tobeValidated %>% dplyr::group_by(assay_material_id) %>% dplyr::distinct() %>% dplyr::filter(dplyr::n()>1)
    misMatches <- tgrData %>% dplyr::filter(assay_material_id %in% badrecords$assay_material_id) %>% dropWhen()
    misMatches <- dplyr::arrange(misMatches, assay_material_id)
  } else (message("Please choose an id by which to validate your dataset:  subject_id, biospecimen_id, or assay_material_id."))
  return(misMatches)
}


#' Validate an existing data frame prior to upload
#'
#' @param data The data frame containing the data you want to validate.
#' @param validation_id The name of the id you want to validate the dataset with respect to:  subject_id, biospecimen_id or assay_material.
#' @return A data frame containing any rows of the original data frame for which there are one or more mismatches between records with matching validation_ids.
#' @author Amy Paguirigan
#' @details
#' Requires setCreds() to have been run successfully.
#' @export
validateDataFrame <- function(data, validation_id = NULL){
  if ("" %in% Sys.getenv(c("REDURI", "TGR"))) {
    stop("You have missing environment variables.  Please setCreds().")}
  else message("Credentials set successfully.")
  forms <- REDCapR::redcap_metadata_read(redcap_uri = Sys.getenv("REDURI"),
                                         token=Sys.getenv("TGR"),
                                         verbose = FALSE)$data
  data[data == ""] <- NA
  ## Group by the validation_id column and then test to see if the appropriate other columns are identical for all groups,
  ## then return any of thsoe that aren't.
  if (validation_id == "subject_id"){
    if ("subject_id" %in% colnames(data) == F) stop("The validation_id column does not exist in your data frame.")
    fieldstoPull <- forms[forms$form_name %in% c("tgr_entry_ids", "tgr_ids", "tgr_duo", "tgr_patient", "outcomes"),]$field_name
    fieldsToValidate <- fieldstoPull[!fieldstoPull %in% c("ext_biospecimen_id", "biospecimen_id", "assay_material_id", "molecular_id")]

    badrecords <- data[, colnames(data) %in% fieldsToValidate] %>% dplyr::group_by(subject_id) %>% dplyr::distinct() %>% dplyr::filter(dplyr::n()>1)
    misMatches <- data[, colnames(data) %in% fieldstoPull] %>% dplyr::filter(subject_id %in% badrecords$subject_id)
    misMatches <- dplyr::arrange(misMatches, subject_id)
  } else if (validation_id == "biospecimen_id"){
    if ("biospecimen_id" %in% colnames(data) == F) stop("The validation_id column does not exist in your data frame.")
    fieldstoPull <- forms[forms$form_name %in% c("tgr_entry_ids", "tgr_ids","tgr_specimen_acquisition", "biomarkersleukemia"),]$field_name
    fieldsToValidate <- fieldstoPull[!fieldstoPull %in% c("subject_id", "assay_material_id", "molecular_id")]

    badrecords <- data[, colnames(data) %in% fieldsToValidate] %>% dplyr::group_by(biospecimen_id) %>% dplyr::distinct() %>% dplyr::filter(dplyr::n()>1)
    misMatches <- data[, colnames(data) %in% fieldstoPull] %>% dplyr::filter(biospecimen_id %in% badrecords$biospecimen_id)
    misMatches <- dplyr::arrange(misMatches, biospecimen_id)
  } else if (validation_id == "assay_material_id"){
    if ("assay_material_id" %in% colnames(data) == F) stop("The validation_id column does not exist in your data frame.")
    fieldstoPull <- forms[forms$form_name %in% c("tgr_entry_ids", "tgr_ids","tgr_laboratory"),]$field_name
    fieldsToValidate <- fieldstoPull[!fieldstoPull %in% c("subject_id", "biospecimen_id", "ext_biospecimen_id", "molecular_id")]

    badrecords <- data[, colnames(data) %in% fieldsToValidate] %>% dplyr::group_by(assay_material_id) %>% dplyr::distinct() %>% dplyr::filter(dplyr::n()>1)
    misMatches <- data[, colnames(data) %in% fieldstoPull] %>% dplyr::filter(assay_material_id %in% badrecords$assay_material_id)
    misMatches <- dplyr::arrange(misMatches, assay_material_id)
  } else (message("Please choose an id by which to validate your dataset:  subject_id, biospecimen_id, or assay_material_id."))
  return(misMatches)
}


