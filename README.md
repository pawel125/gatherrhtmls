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

Minimal example is in inst/gatherrhtmls-example directory

```r
librarry(gatherrhtmls)
files <- get_htmls()  # get all html reports in the working directory and its subdirectories
gater(files, path = "reposr") # to gather it
```
