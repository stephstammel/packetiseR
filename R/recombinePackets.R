#' recombinePackets
#'
#'Once you've broken your big data frame into little ones, you need to put
#' it back together.
#'
#' @param dir, string. Directory where you find the files.
#' @param form, string. '.rds' or '.csv'.
#' @param namesCol a vector with the names of the columns you're going to call them
#'
#' @return newFile, a data frame with all the bits!
#' @export
#'
#' @examples
#'
#' recombinePackts('./', '.rds', c("a", "b", "c"))
#'
#'
recombinePackets <- function(dir, form, namesCol){
  # See https://t.co/1Jw1ZWeDRb and https://aosmith.rbind.io/2017/12/31/many-datasets/
  # for details on why this works well. Thanks for the tips!
  require(dplyr)
  require(purrr)
  require(data.table)

  filesGet <- list.files(path = dir,
                         pattern = paste("*", form, sep = ""),
                         full.names = TRUE,
                         recursive = FALSE)

  newFile <- map(filesGet, filesRead, form, namesCol)
  newFile <- do.call("rbind", newFile)

  return(newFile)
}

#' filesRead
#'
#' A custom function for reading all those files we split.
#'
#' @param nameFile the name of the file, string
#' @param form , string '.rds' or '.csv'
#'
#' @return the individual file piece.
#' @export
#'
#' @examples
#'
#' x <- filesRead('./namehere.rds', '.rds')
#'
filesRead <- function(nameFile, form, namesCol){
  if (form == '.rds'){
    data <- readRDS(nameFile)
    col.names <- 'overwrite or else it is unused'
  } else {
    data <- read.csv(nameFile, col.names = TRUE, sep = ",")
  }
  colnames(data) <- namesCol
  return(data)
}
