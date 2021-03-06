# gatherrhtmls

**gatherrhtmls** is inspired by *bookdown* package, which lets you easely convert your .Rmd analysis notebooks into one, 
elegant webpage with elegant navigation. *bookdown* is not limited to html output (Word and odf files are also implemented) and 
due to this limits usage of some html widgets. It also takes as input raw .Rmd files and processes it each time when new any 
of files has changed, which takes a while, espetially if you project contains many analysis files. 

**gatherrhtmls** takes as input list of ready-to-publish html reports and slightly modifies them inserting a generated 
sidebar navigation menu and stylesheet file. It is not as elegant as *bookdown*, but works much faster.

Installation
------------

```r
# devtools::install_github("pawel125/gatherrhtmls")
```

Usage
-----

Minimal example is placed in inst/gatherrhtmls-example directory

1. Create "_gatherrhtmls.yaml" configuration file. You can do it on your own, or use `generate_yaml()` function to create the template and then modify it by hand.
```r
gatherrhtmls::generate_yaml()
```

2. Run `gather()` to create final one-site report. If you used non-default name of the configuration file, new name must be supplied in the `gatherrhtmls_file` parameter.
```r
gatherrhtmls::gather()
```

If you skip the first step, `gather()` will use default values

Configuration .yaml file structure
---------------------------------
```yaml
title: Raport               # Raport name
data:                       # List of files to include
- (PART) Data 1             # Part marker
- path/to/file1.html
- path/to/file2.html
- (PART) Data 2
- path/to/file3.html
output_dir: output/path     # Path to write
```

Known issues
------------
`gather()` fails if the files previously generated by it are given to it in a new run. Problem occurs for example when one has already generated report and run `gather()` without any arguments and configuration file does not exist or was generated by `generate_yaml()` and contains files from the previous run.
