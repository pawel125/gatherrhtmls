#' @importFrom magrittr %>%
#' @import stringr
#' @import purrr
#' @import dplyr
#' @import readr

read_report_file <- function(path) {
  read_file(path) %>%
    str_split("\r\n") %>%
    .[[1]]
}

extract_tags <- function(html_file, tag) {
  html_file[grep(tag, html_file)]
}

drop_xml <- function(xml) {
  gsub("<[^>]*>", "", xml)
}

get_content_data <- function(html) {
  h1_loc <- grep("<h1>", html)
  if(length(h1_loc) > 0) {
    content <- tibble(h1_loc)
    content %>%
      mutate(
        old_tag = html[h1_loc],
        title = drop_xml(old_tag),
        tag_id = str_c(row_number(), str_replace_all(title, " ", "_"), sep = "_"),
        new_tag = str_replace(old_tag, '>', str_c(' id="', tag_id, '">'))
      )
  } else {
    NULL
  }
}

#' @export
gather <- function(files = NULL, output_dir = "report") {
  if (is.null(files)) {
    files <- get_htmls(".")
  }

  reports <- tibble(files)
  reports <- reports %>%
    mutate(
      nr = row_number(),
      output_files = str_replace_all(files, "/", "_"),
      html = map(files, read_report_file),
      title = map(html, ~drop_xml(extract_tags(.x, "<title>"))),
      content = map(html, get_content_data)
    )

  create_simple_nav_item <- function(nr, title, ref) {
    sprintf('<li><a href="%s"><b>%s. </b>%s</a></li>',
            ref, nr, title)
  }

  create_complex_nav_item <- function(nr, title, ref, content) {
    subitems <- pmap_chr(content, function(tag_id, title, ...) {
      sprintf('    <li><a href="#%s">%s</a></li>', tag_id, title)
    })
    str_c(
      sprintf('<li><a href=%s><b>%s. </b>%s</a>', ref, nr, title),
      '  <ul>',
      str_c(subitems, collapse = "\n"),
      '  </ul>',
      '</li>',
      sep = "\n"
    )
  }

  create_nav_item <- function(content, nr, title, output_files, ...) {
    if(is.null(content)) {
      create_simple_nav_item(nr, title, output_files)
    } else {
      create_complex_nav_item(nr, title, output_files, content)
    }
  }

  reports$nav_items <- pmap(reports, create_nav_item)

  nav_menu <- str_c(
    '  <link href="styles.css" rel="stylesheet" />',
    '  <div class="rmaker-container">',
    '    <div class="rmaker-sidebar">',
    '      <nav>',
    '        <ol>',
    str_c("          ", reports$nav_items, collapse = "\n"),
    '        </ol>',
    '      </nav>',
    '    </div>',
    '  </div>',
    '</head>',
    sep = "\n"
  )

  reports$html2 <- pmap(reports, function(html, content, ...) {
    if(is.null(content)) {
      return(html)
    }
    html[content$h1_loc] <- content$new_tag
    html
  })

  reports$final <- map(reports$html2, function(html2) {
    html2 %>%
      str_c(collapse = "\n") %>%
      str_replace("</head>", nav_menu)
  })

  dir.create(output_dir)
  file.copy("inst/resources/styles.css", sprintf("%s/styles.css", output_dir))
  walk2(reports$final, reports$output_files,
        ~write_file(.x, path = str_c(output_dir, .y, sep = "/"))
  )
}
