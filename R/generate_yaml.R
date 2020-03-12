#' @importFrom yaml write_yaml
#' @export
generate_yaml <- function(files = get_htmls(), output_dir = "report") {
  content <- list(
    files = files,
    output_dir = output_dir
  )
  write_yaml(content, file = "_gatherrhtmls.yaml")
}
