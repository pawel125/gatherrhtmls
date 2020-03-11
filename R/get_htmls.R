#' @importFrom stringr str_replace
#' @export
get_htmls <- function(path = ".") {
  list.files(path, pattern = "\\.html", recursive = T, full.names = T) %>%
    str_replace("^./", "")
}
