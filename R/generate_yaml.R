#' Generates configuration file for \code{gather()}
#'
#' @param files Character vector, of HTML files to be included in configuration.
#'   file
#' @param output_dir Character, path where \code{gather} will save output files.
#'
#' @importFrom yaml write_yaml
#' @export
generate_yaml <- function(files = get_htmls(), output_dir = "report") {
  content <- list(
    files = files,
    output_dir = output_dir
  )
  write_yaml(content, file = "_gatherrhtmls.yaml")
}
