# devtools::install_github("pawel125/gatherrhtmls")
library(gatherrhtmls)
files <- get_htmls()
gather(files, output_dir = "report")

# or simply to gather all reports in the project directory
gather(output_dir = "report")