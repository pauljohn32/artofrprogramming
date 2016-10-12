#' ex_reader
#'
#' @description Reads .txt and .md files with exercise
#'   questions by chapter for the Art of R Programming.
#'
#' @param ch Integer indicating chapter number
#'
#' @return Recursive list consisting of character vectors
#'   for each chapter.
#' @export
ch_reader <- function(path = getwd()) {
  folders <- list.files(path)
  folders <- grep("^ch.",
    folders, value = TRUE,
    ignore.case = TRUE)
  folder_files <- lapply(folders, is_question_files)
  paths <- lapply(seq_along(folders), function(i)
    path_maker(folders[[i]], folder_files[[i]]))
  x <- lapply(paths, folder_reader)
  compiled_paths <- paste0(folders, "/")
  x <- lapply(x, function(x) paste0(paste0(x, collapse = "\n"), "\n\n"))
  x <- lapply(seq_along(folders), function(i)
    paste0("# ", folders[[i]], "\n",
      paste(x[[i]], collapse = "\n")))
  lapply(seq_along(x), function(i)
    cat(x[[i]], file = paste0(compiled_paths[[i]], "questions.md")))
}

ch_questions <- function(x, ch, path) {
  x <- paste0(paste0("# Chapter ", ch, " Questions\n"),
    paste0(ch, collapse = "\n"), "\n")
}



    paste0(paste0("#", folders[[i]]),
      paste0(folders, collapse = "\n"),
    paste0(paths[[i]], collapse = "\n")))
path_maker <- function(folder, folder_files) {
  unlist(lapply(folder_files, function(i) {
    if (identical(i, "")) return(invisible())
    paste0(folder, "/", i)
  }), use.names = FALSE)
}

folder_reader <- function(x) {
  unlist(lapply(x, file_reader),
    use.names = FALSE)
}
file_reader <- function(path) {
  x <- readLines(path, warn = FALSE)
  paste0(unlist(lapply(x, function(i)
    gsub("^[0-9].\\s", "", i)),
    use.names = FALSE), collapse = "\n")
}

is_question_files <- function(x, mode = "paths") {
  paths <- list.files(x)
  x <- paths
  x <- gsub("[^[:alnum:]_]", " ", x)
  x <- lapply(x, function(x) unlist(strsplit(x, " ", fixed = TRUE),
    use.names = FALSE))
  x <- lapply(x, function(x) gsub("ch|ex", "", x))
  x <- lapply(x, function(x) grep("[0-9]", x, value = TRUE))
  x <- lapply(x, function(x) x[1])
  if (mode == "paths") {
    x <- lapply(x, function(x) all(length(x) == 1, !is.na(x)))
    x <- paths[unlist(x, use.names = FALSE)]
  } else if (mode == "logical") {
    x <- lapply(x, function(x) all(length(x) == 1, !is.na(x)))
  }
  if (identical(length(x), 0L)) return("")
  unlist(x, use.names = FALSE)
}
