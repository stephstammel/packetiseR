#' makePackets
#'
#' Turn a very large data frame into an ordered set of smaller ones.
#'
#' @param file, a data frame. The data frame that needs converting into many files
#' @param n, an integer. The number of mini data frames you'd like to make
#' @param form, a string. Can take either '.rds' for R data files or '.csv' for ,csv files.
#' Will determine the format of the smaller data frames.
#' @param chosenDir, a string. Where would you like to put all these small data frames?
#' The function will check if that directory exists, if not it will make it.
#'
#' @return does not return.
#' @export
#'
#' @examples
#'
#' data <- matrix(runif(1,0,1), nrow = 1000, ncol = 20)
#' makePackets(data, 10, '.rds', './data')
#'
makePackets <- function(file, n, form, chosenDir){

  if (is.null(nrow(file))){
    print ("need more than two columns right now, sorry")
  }

  if(!dir.exists(chosenDir)){
    dir.create(chosenDir)
  }
  j <- nrow(file)
  rows <- floor(j/n) + 1

  makeFiles(file, n, form, j, rows, chosenDir)
}


#' makeIndvFiles
#'
#' This function does the heavy lifting for the makePackets function.
#' It builds all the individual files from the larger data frame.
#'
#' @param file, a data frame. The data frame that needs converting into many files
#' @param n, an integer. The number of mini data frames you'd like to make
#' @param form, a string. Can take either '.rds' for R data files or '.csv' for ,csv files.
#' Will determine the format of the smaller data frames.
#' @param j, the number of rows in the big data frame.
#' @param rows, the number of rows in each smaller data frame.
#' @param numberFile, the file number. The smaller data frames are built sequentially so they
#' can be rebuilt into one data frame in the same order they came in.
#' @param chosenDir, a string. Where would you like to put all these small data frames?
#' Built by makePackets().
#' @return doesn't return anything.
#' @export
#'
#' @examples
#'
#' makeIndvFiles(file, 10, '.csv', 1000, 101, 2, './data')
#'
makeIndvFiles <- function(file, n, form, j, rows, numberFile, chosenDir){
  if (numberFile < 10){
    nFile <- paste("0", numberFile, sep = "")
  } else {
    nFile <- numberFile
  }
  name <- paste(chosenDir,"/", nFile, "file", form, sep = "")
  if (numberFile == n){
    fileTemp <- file[((n-1)*rows + 1):nrow(file),]
  } else {
    fileTemp <- file[((numberFile-1)*rows +1):(numberFile*rows),]
  }
  if (form == '.rds'){
    saveRDS(fileTemp, name)
  } else {
    write.csv(fileTemp, name, sep =",")
  }
}

#' makeFiles
#'
#' @param file, a data frame. The data frame that needs converting into many files
#' @param n, an integer. The number of mini data frames you'd like to make
#' @param form, a string. Can take either '.rds' for R data files or '.csv' for ,csv files.
#' Will determine the format of the smaller data frames.
#' @param j, the number of rows in the big data frame.
#' @param rows, the number of rows in each smaller data frame.
#' @param chosenDir, a string. Where would you like to put all these small data frames?
#' Built by makePackets().
#'
#' @return does not return anything
#' @export
#'
#' @examples
#'
#' makeFiles(file, n, form, j, rows, i, chosenDir)
#'
makeFiles <- function(file, n, form, j, rows, chosenDir){
  for (i in 1:n){
    makeIndvFiles(file, n, form, j, rows, i, chosenDir)
  }
}
