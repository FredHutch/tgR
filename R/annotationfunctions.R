#' Pull Current TGR Annotation and Value definitions
#'
#' @description Pulls current data about annotations from GitHub for use in annotating molecular data sets in the Repository.
#' @return A data frame containing the current TGR Annotations, Values and their Definitions
#' @author Amy Paguirigan
#' @return Returns a data frame containing TGR metadata definitions.
#' @details
#' Nothing to see here.
#' @export
tgrDefinitions <- function() {
  message("pulling annotations from the master branch of the tgr-annotations repo")
  suppressMessages(commonKnowledge <- httr::content(httr::GET("https://raw.github.com/FredHutch/tgr-annotations/master/commonKnowledge.csv"),
                                   as = "parsed", type = "text/csv"))
  return(commonKnowledge)
}


#' (Admin) Finds undefined REDCap variables
#'
#' Pulls sample data down from REDCap and compares them with the defined annotation list in GitHub and identifies variables in REDCap that need defining in GitHub.  Only variables for which there is a value in at least one record are returned.
#'
#' @return A data frame that is a template to add to the commonKnowledge data in the tgr-annotations repo with new definitions or corrections.
#' @author Amy Paguirigan
#' @details
#' Requires **admin** REDCap credentials to be set in the environment.
#' @export
undefinedAnnotations <- function() {
  if ("" %in% Sys.getenv(c("REDURI", "TGR", "S3A", "S3SA"))) {
    stop("You have missing environment variables.  Please set creds in env vars.")}
  else message("Credentials set successfully.")
  commonKnowledge <- tgrDefinitions()
  sciMeta <- tgrAnnotate(harmonizedOnly = T, DAG = "all", evenEmptyCols = F)
  # Remove project memberships
  defineMe <- sciMeta %>% dplyr::select(-dplyr::starts_with("data_is_"))
  categorical <- commonKnowledge %>% dplyr::filter(Type == "categorical")
  noLevelAnnots <- commonKnowledge %>% dplyr::filter(Type != "categorical")

  usedAnnots <- purrr::map_dfr(colnames(defineMe), function(x){
    Y <- unique(dplyr::select(defineMe, x))
    colnames(Y) <- "Value"
    Y$Value <- as.character(Y$Value)
    Y$Annotation <- x
    Y
  })

  usedCat <- usedAnnots %>% dplyr::filter(Annotation %in% categorical$Annotation & is.na(Value) !=T)
  missingCat <- dplyr::anti_join(usedCat, categorical)

  usedOther <- usedAnnots %>% dplyr::filter(!Annotation %in% categorical$Annotation) %>% dplyr::select(Annotation) %>% unique()
  missingOther <- dplyr::anti_join(usedOther, noLevelAnnots)

  makeMeaning <- dplyr::full_join(missingCat, missingOther)

  suppressWarnings(makeMeaning <- dplyr::bind_rows(commonKnowledge[0,], makeMeaning)) # make a sample data frame to fill in

  return(makeMeaning)
}

