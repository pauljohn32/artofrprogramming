library(formatR)

?tidy.source

## blurts result to screen
tidy.source(source="qgraph.R", replace.assign = TRUE)

## writes result to file
tidy.source(source="qgraph.R", replace.assign = TRUE, file = "qgraph-clean.R")
