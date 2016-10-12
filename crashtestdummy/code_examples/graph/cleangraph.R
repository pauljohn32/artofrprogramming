library(formatR)

?tidy.source

## blurts result to screen
tidy_source(source="graph.R", arrow = TRUE)

## writes result to file
tidy_source(source="graph.R", arrow = TRUE,
            file = "graph-clean-20161012.R")
