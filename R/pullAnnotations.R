#' Pull Current TGR Annotation definitions
#'
#' Pulls current data about annotations from GitHub for use in annotating molecular data sets in the Repository.
#'
#' @return A data frame containing the current TGR Annotations, Values and their Definitions
#' @author Amy Paguirigan
#' @details
#' Nothing to see here.
#' @export

pullAnnotations <- function() {
    print("pulling annotations from the master branch of the tgr-annotations repo")
    commonKnowledge <- httr::content(httr::GET("https://raw.github.com/FredHutch/tgr-annotations/master/commonKnowledge.csv"),
        as = "parsed", type = "text/csv")
    return(commonKnowledge)
}
