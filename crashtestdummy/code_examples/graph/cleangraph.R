library(formatR)

?tidy.source

## blurts result to screen
tidy.source(source="graph.R", replace.assign = TRUE)

## writes result to file
tidy.source(source="graph.R", replace.assign = TRUE, file = "graph-clean.R")
