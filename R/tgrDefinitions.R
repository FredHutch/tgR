#' Pull Current TGR Annotation and Value definitions
#'
#' @description Pulls current data about annotations from GitHub for use in annotating molecular data sets in the Repository.
#' @return A data frame containing the current TGR Annotations, Values and their Definitions
#' @author Amy Paguirigan
#' @export
tgrDefinitions <- function() {
  message("pulling annotations from the master branch of the tgr-annotations repo")
  suppressMessages(commonKnowledge <- httr::content(httr::GET("https://raw.github.com/FredHutch/tgr-annotations/master/commonKnowledge.csv"),
                                                    as = "parsed", type = "text/csv"))
  return(commonKnowledge)
}


#' Pull Current TGR Annotation and Value definitions
#'
#' @description Pulls current data about annotations from GitHub for use in annotating molecular data sets in the Repository.
#' @return A data frame containing the current TGR Annotations, Values and their Definitions
#' @author Amy Paguirigan
#' @export
tgrDefinedAnnotations <- function() {
  suppressMessages(annotations <- jsonlite::fromJSON(httr::content(httr::GET("https://raw.github.com/FredHutch/tgr-annotations/main/annotations.json"),
                                                    as = "parsed")))
  suppressMessages(values <- jsonlite::fromJSON(httr::content(httr::GET("https://raw.github.com/FredHutch/tgr-annotations/main/values.json"),
                                                as = "parsed")))
  commonKnowledge <- dplyr::full_join(annotations, values) %>% dplyr::arrange(Category, Annotation, Value)

  return(commonKnowledge)
}
