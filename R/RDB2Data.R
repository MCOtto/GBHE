#' Convert SQLite table to a dataframe
#'
#' A specialty function to take the \code{DFSurvey.sq3} table in the
#' \code{extdata} directory and assign it to
#'
#' The function simplifies the R functions in the \code{Data} directory that
#' extract tables from the \code{../ExtData/DFSurvey.sq3} database.
#'
#' @param Tbl character string of the table name
#' @param RDB Name of the the SQLite database with the path name
#' @param envir If the table is to be assigned to a particular environment
#' @param RowName Names of the columns to be used as the row names.  If
#' \code{FALSE}, no column is used for the row name (default)
#' @return A dataframe with the same columns as the table
#' @note I would like to add the option that if the table name is the same as
#' the file name of the database less the extension and the environment is not
#' NULL, then the function would assign all the tables to \code{envir}.  Then,
#' this extended functionality should go in \code{locdata}, allowing an RDB to
#' exist in a data directory. If the database name was given as one of the
#' objects to be extracted, then all the tables would be assigned to dataframes
#' of the same names, or if a table name was specified, that table would be
#' extracted.
#' @author Mark C. Otto
#' @seealso \code{\link{locdata}}
#' @import RSQLite here
#' @examples
#'
#'
#' \dontrun{
#' RDB2Data ("SmpObs", RowName = "NestID")
#' }
#'
#'
#' @export RDB2Data
RDB2Data <-
  function (Tbl,
            RowNames = FALSE,
            RDB = here("extdata/DFSurvey.sq3"),
            envir = NULL)
  {
    dbh <- dbConnect(RSQLite::SQLite(), RDB)
    if (!dbExistsTable(dbh, Tbl))
      stop(Tbl, " does not exist in ", RDB)
    tTbl <- dbReadTable(dbh, Tbl, row.names = FALSE)
    if (!is.logical(RowNames) & !is.null(RowNames)) {
      tRN <- tTbl[, RowNames]
      RN <- if (class(tRN) == "data.frame") {
        apply(as.matrix(tRN), 1, paste, collapse = ".")
      } else {
        tRN
      }
      RN2 <- 1:length(RN)
      RN <- ifelse(duplicated(RN), paste(RN, RN2, sep = "."), RN)
      rownames(tTbl) <- RN
    }
    dbDisconnect(dbh)
    rm(dbh)
    if (!is.null(envir))
      assign(Tbl, tTbl, envir = envir)
    tTbl
  }
