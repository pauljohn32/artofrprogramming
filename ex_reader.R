#' ex_reader
#'
#' @description Reads .txt and .md files with exercise
#'   questions by chapter for the Art of R Programming.
#'
#' @param path Path to artofrprogramming directory
#'
#' @return Generates \code{answers.md} files for each
#'   chapter consisting of exercise questions (include those
#'   that use R code).
#' @export
ch_reader <- function(path = getwd()) {
    ## paths vectors
    folders <- list.files(path)
    ## select ch folders
    folders <- grep("^ch.", folders, value = TRUE,
                    ignore.case = TRUE)
    ## return only question files
    folder_files <- lapply(folders, is_question_files)

    ## make into full paths
    paths <- lapply(seq_along(folders), function(i)
        path_maker(folders[[i]], folder_files[[i]]))

    ## read each folder
    x <- lapply(paths, folder_reader)

    # for reconstructing paths
    compiled_paths <- paste0(folders, "/")

    # spacing between questions
    x <- lapply(x, function(x)
        paste0(paste0(x, collapse = "\n\n"), "\n\n\n"))

    ## add headings
    x <- lapply(seq_along(folders), function(i)
        paste0("# ", folders[[i]], "\n\n",
               paste(x[[i]], collapse = "\n\n")))

    ## save markdown file with all questions for each chapter
    invisible(lapply(seq_along(x), function(i)
        cat(x[[i]], file = paste0(compiled_paths[[i]],
                                  "questions.md"))))
}

path_maker <- function(folder, folder_files) {
    vapply(folder_files, function(i)
        ifelse (identical(i, ""),
                invisible(NA_character_),
                paste0(folder, "/", i)),
        character(1), USE.NAMES = FALSE)
}

folder_reader <- function(x) {
    ## clearly was going thru a lapply phase
    not.na <- vapply(x, function(x) isTRUE(is.na(x)), logical(1))
    x <- lapply(x[not.na], readLines, warn = FALSE)
    x <- lapply(x, function(x)
        unlist(linebreaker(x), use.names = FALSE))
    paste0(unlist(x, use.names = FALSE), collapse = "")
}

linebreaker <- function(x) {
    ## an ode to functional programming
    k <- FALSE
    for (i in seq_along(x)) {
        x[[i]] <- gsub("^[0-9].\\s", "", x[[i]])
        if (grepl("```", x[[i]])) {
            if (k) {
                x[[i]] <- paste0(x[[i]], "\n\n")
                k <- !k
            } else {
                x[[i]] <- paste0(x[[i]], "\n")
                k <- !k
            }
        } else if (k) {
            x[[i]] <- paste0(x[[i]], "\n")
        } else {
            x[[i]] <- paste0(x[[i]], "\n\n")
        }
    }
    x
}


is_question_files <- function(x, mode = "paths") {
    ## create paths object
    paths <- list.files(x)

    ## replace non alpha num chars with space
    x <- gsub("[^[:alnum:]_]", " ", paths)
    ## split files into words
    x <- lapply(x, function(x) strsplit(x, " ", fixed = TRUE)[[1]])
    ## remove prefixes ch and ex
    x <- lapply(x, function(x) gsub("ch|ex", "", x))
    ## return numbers in each file name
    x <- lapply(x, function(x) grep("[0-9]", x, value = TRUE))
    ## select first number in each
    x <- lapply(x, function(x) x[1])

    ## logical vector indicating questions
    x <- vapply(x, function(x) all(identical(x, 1),
                                   isTRUE(!is.na(x))),
                logical(1), USE.NAMES = FALSE)

    ## convert to character vector of pathnames
    if (mode == "paths") {
        x <- paths[x]
    }

    ## if empty return blank
    if (identical(length(x), 0L)) return("")

    ## return
    x
}
