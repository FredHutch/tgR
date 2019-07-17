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
    stop("You have missing environment variables.  Please use setCreds().")}
  else print("Credentials set successfully.")
  if (is.character(fileStem) & length(fileStem) == 1) {
    if (is.data.frame(dataToSplit) & nrow(dataToSplit) > 0) {

      specimen <- dataToSplit[colnames(dataToSplit) %in% getDictionary(project = "specimen")]
      specimen <- specimen %>% unique() %>% dplyr::filter(biospecimen_id != "")
      if(ncol(specimen) > 0) {
        if ("biospecimen_id" %in% colnames(specimen)){
          specimen <- specimen %>% select("biospecimen_id", everything())
          write.csv(specimen, file = paste0(fileStem, "-TGBiospecimens.csv"), row.names = F, na = "")
          print(paste0("Writing File: ", paste0(fileStem, "-TGBiospecimens.csv")))
        } else { stop("biospecimen_id is required for this upload.")}
      }

      assay <- dataToSplit[colnames(dataToSplit) %in% getDictionary(project = "assay")]
      assay <- assay %>% unique() %>% dplyr::filter(assay_material_id != "")
      if(ncol(assay) > 0) {
        if ("assay_material_id" %in% colnames(assay)){
          assay <- assay %>% select("assay_material_id", "biospecimen_id", everything())
          write.csv(assay, file = paste0(fileStem, "-TGAssayMaterials.csv"), row.names = F, na = "")
          print(paste0("Writing File: ", paste0(fileStem, "-TGAssayMaterials.csv")))
        } else { stop("assay_material_id is required for this upload.")}
      }

      molecular <- dataToSplit[colnames(dataToSplit) %in% getDictionary(project = "molecular")]
      molecular <- molecular %>% unique() %>% dplyr::filter(molecular_id != "")
      if(ncol(molecular) > 0) {
        if ("molecular_id" %in% colnames(molecular)){
          molecular <- molecular %>% select("molecular_id", "assay_material_id", everything())
          write.csv(molecular, file = paste0(fileStem, "-TGMolecularDatasets.csv"), row.names = F, na = "")
          print(paste0("Writing File: ", paste0(fileStem, "-TGMolecularDatasets.csv")))
        } else { stop("molecular_id is required for this upload.")}
      }

    } else {stop("Please provide a data frame with more than 0 rows to split for uploading.")}
  } else {stop("Please provide a single `fileStem` character string for your files.")}

}

