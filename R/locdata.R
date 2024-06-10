#' Local Data Directory
#' 
#' Loads specified data sets, or list the available data sets from a local
#' directory or a specific file (usually ExtData) from a package or otherwise
#' act like \code{data}.
#' 
#' Currently, five formats of data files are supported.  Note that these are
#' different than for \code{data} function.
#' 
#' \enumerate{ \item files ending \file{.R} or \file{.r} are
#' \code{\link{source}()}d in, with the working directory changed temporarily
#' to the directory containing the respective file.  (\code{data} ensures that
#' the \pkg{utils} package is attached, in case it had been run \emph{via}
#' \code{utils::data}.)
#' 
#' \item files ending \file{.RData} or \file{.rda} are \code{\link{load}()}ed.
#' 
#' \item files ending \file{.tab}, \file{.txt} or \file{.TXT} are read using
#' \code{\link{read.table}(\dots{}{}, header = TRUE)}, and hence result in a
#' data frame.
#' 
#' \item files ending \file{.csv} or \file{.CSV} are read using
#' \code{\link{read.table}(\dots{}{}, header = TRUE, sep = ",")}, and also
#' result in a data frame.
#' 
#' \item files ending \file{.scs} or \file{.SCS} are read using
#' \code{\link{read.table}(\dots{}{}, header = TRUE, sep = ";")}, and also
#' result in a data frame.  } If more than one matching file name is found, the
#' first on this list is used.  (Files with extensions \file{.txt}, \file{.tab}
#' or \file{.csv} can be compressed, with or without further extension
#' \file{.gz}, \file{.bz2} or \file{.xz}.)
#' 
#' The data sets to be loaded can be specified as a set of character strings or
#' names, or as the character vector \code{list}, or as both.
#' 
#' For each given data set, the first two types (\file{.R} or \file{.r}, and
#' \file{.RData} or \file{.rda} files) can create several variables in the load
#' environment, which might all be named differently from the data set.  The
#' third and fourth types will always result in the creation of a single
#' variable with the same name (without extension) as the data set.
#' 
#' If no data sets are specified, \code{locdata} lists the available data sets.
#' It looks for a new-style data index in the \file{Meta} or, if this is not
#' found, an old-style \file{00Index} file in the file given or in the
#' \file{data} directory if no package is specfied or in the specified
#' directory of each specified package.  The function will use these files to
#' prepare a listing.  If there is no index, available data files for loading
#' are computed and included in the listing, and a warning is given: such
#' packages are incomplete.  The information about available data sets is
#' returned in an object of class \code{"packageIQR"}.  The structure of this
#' class is experimental.  Where the datasets have a different name from the
#' argument that should be used to retrieve them the index will have an entry
#' like \code{beaver1 (beavers)} which tells us that dataset \code{beaver1} can
#' be retrieved by the call \code{data(beaver)}.
#' 
#' If \code{lib.loc} and \code{package} are both \code{NULL} (the default), the
#' \file{data} directory (if any) of the current working directory then in the
#' data sets will be searched for all the currently loaded packages.
#' 
#' If \code{lib.loc = NULL} but \code{package} is specified as a character
#' vector, the specified package(s) are searched for first amongst loaded
#' packages and then in the default library/ies (see \code{\link{.libPaths}}).
#' 
#' If \code{lib.loc} \emph{is} specified (and not \code{NULL}), packages are
#' searched for in the specified library/ies, even if they are already loaded
#' from another library.
#' 
#' To just look in the \file{data} directory of the current working directory,
#' set \code{package = character(0)} (and \code{lib.loc = NULL}, the default).
#' 
#' @param \dots literal character strings or names.
#' @param list a character vector.
#' @param package a character vector giving the package(s) to look in for data
#' sets, or \code{NULL}.
#' 
#' By default, all packages in the search path are used, then the \file{data}
#' subdirectory (if present) of the current working directory.
#' @param lib.loc a character vector of directory names of libraries, or
#' \code{NULL}.  The default value of \code{NULL} corresponds to all libraries
#' currently known.
#' @param datadir a character string for a directory name if
#' \code{package=NULL}, or a directory in the given package.  The default value
#' of \code{NULL} corresponds to the \code{data} or \code{Data} directory in
#' the current path or in the specified package.
#' @param verbose a logical.  If \code{TRUE}, additional diagnostics are
#' printed.
#' @param envir the \link{environment} where the data should be loaded.
#' Defaults to the global environment.  (Have not figured out a way to make it
#' the current environment.)
#' @return A character vector of all data sets specified, or information about
#' all available data sets in an object of class \code{"packageIQR"} if none
#' were specified.
#' @note One can take advantage of the search order and the fact that a
#' \file{.R} file will change directory.  If raw data are stored in
#' \file{mydata.txt} then one can set up \file{mydata.R} to read
#' \file{mydata.txt} and pre-process it, e.g., using \code{transform}.  For
#' instance one can convert numeric vectors to factors with the appropriate
#' labels.  Thus, the \file{.R} file can effectively contain a metadata
#' specification for the plaintext formats.
#' @section Good practice: \code{locdata()} was originally intended to allow
#' users to load datasets from packages for use in their examples, and as such
#' it loaded the datasets into the workspace \code{\link{.GlobalEnv}}.  This
#' avoided having large datasets in memory when not in use.  That need has been
#' almost entirely superseded by lazy-loading of datasets.
#' 
#' The ability to specify a dataset by name (without quotes) is a convenience:
#' in programming the datasets should be specified by character strings (with
#' quotes).
#' 
#' Use of \code{locdata} within a function without an \code{envir} argument has
#' the almost always undesirable side-effect of putting an object in the user's
#' workspace (and indeed, of replacing any object of that name already there).
#' It would almost always be better to put the object in the current evaluation
#' environment by \code{locdata(\dots{}{}, envir = environment())}.  However,
#' two alternatives are usually preferable, both described in the
#' \sQuote{Writing R Extensions} manual.  \itemize{ \item For sets of data, set
#' up a package to use lazy-loading of data.  \item For objects which are
#' system data, for example lookup tables used in calculations within the
#' function, use a file \file{R/sysdata.rda} in the package sources or create
#' the objects by code at package installation time.  } A sometimes important
#' distinction is that the second approach places objects in the namespace but
#' the first does not.  So if it is important that the function sees
#' \code{mytable} as an object from the package, it is system data and the
#' second approach should be used.
#' @seealso \code{\link{help}} for obtaining documentation on data sets,
#' \code{\link{data}} for the function with the standard defaults,
#' \code{\link{save}} for \emph{creating} the second (\file{.rda}) kind of
#' data, typically the most efficient one.
#' 
#' The \sQuote{Writing R Extensions} for considerations in preparing the
#' \file{data} directory of a package.
#' @keywords datasets documentation
#' @examples
#' 
#' 
#' require(utils)
#' locdata()                      # list all available data sets
#' locdata(package="DualFrame",datadir="ExtData")              #
#' try(data(package = "rpart") )  # list the data sets in the rpart package
#' locdata(USArrests, "VADeaths")    # load the data sets 'USArrests' and 'VADeaths'
#' \dontrun{## Alternatively
#' ds <- c("USArrests", "VADeaths"); locdata(list = ds)}
#' help(USArrests)                # give information on data set 'USArrests'
#' 
#' 
#' @export locdata
locdata<-function (..., list = character(),  
  package = NULL, lib.loc = NULL, datadir = NULL,
  verbose = getOption("verbose"), 
  envir = .GlobalEnv) 
{
    fileExt <- function(x) {
        db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
        ans <- sub(".*\\.", "", x)
        ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
            x[db])
        ans
    }

# Replace the version in the tools package
    .make_file_exts<-function (
     type = c("code", "data", "demo", "docs", "vignette")) 
    {
     switch(type, code = c("R", "r", "S", "s", "q"), data = c("R", 
         "r", "RData", "rdata", "rda", "tab", "txt", "TXT", "tab.gz", 
         "txt.gz", "tab.bz2", "txt.bz2", "tab.xz", "txt.xz", "csv", 
         "CSV", "csv.gz", "csv.bz2", "csv.xz",
         "SCS","scs.gz","scs.bz2","scs.xz"), demo = c("R", 
         "r"), docs = c("Rd", "rd", "Rd.gz", "rd.gz"), 
         vignette = c(outer(c("R", 
         "r", "S", "s"), c("nw", "tex"), paste, sep = ""), "Rmd"))
    }
    names <- c(as.character(substitute(list(...))[-1L]), list)
    if (!is.null(package)) {
        if (!is.character(package)) 
            stop("'package' must be a character string or NULL")
        if (any(package %in% "base")) 
            warning("datasets have been moved from package 'base' to package 'datasets'")
        if (any(package %in% "stats")) 
            warning("datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] <- "datasets"
    }

# Finds the package directories or if package is NULL, the loaded package
# directories.
    paths <- find.package(package, lib.loc, verbose = verbose)
    if (!length(package))paths<-c(getwd(),paths)      
    if (is.null(lib.loc)) 
        paths <- c(path.package(package, TRUE),paths)
    paths <- unique(paths[file.exists(paths)])
    if(is.null(datadir))datadir<-"data"
    paths <- paths[file_test("-d", file.path(paths, datadir))]
# Add semicolon separated files.
#     dataExts <- tools:::.make_file_exts("data")
    dataExts <- .make_file_exts("data")
    if (length(names) == 0L) {
        db <- matrix(character(), nrow = 0L, ncol = 4L)
        for (path in paths) {
            entries <- NULL
            packageName <- if (file_test("-f", file.path(path, 
                "DESCRIPTION"))) 
                basename(path)
            else "."
            if (file_test("-f", INDEX <- file.path(path, "Meta", 
                "data.rds"))) {
                entries <- readRDS(INDEX)
            }
            else {
                dataDir <- file.path(path, datadir)
                entries <- tools::list_files_with_type(dataDir, 
                  "data")
                if (length(entries)) {
                  entries <- unique(tools::file_path_sans_ext(basename(entries)))
                  entries <- cbind(entries, "")
                }
            }
            if (NROW(entries)) {
                if (is.matrix(entries) && ncol(entries) == 2L) 
                  db <- rbind(db, cbind(packageName, dirname(path), 
                    entries))
                else warning(gettextf("data index for package %s is invalid and will be ignored", 
                  sQuote(packageName)), domain = NA, call. = FALSE)
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")
        footer <- if (missing(package)) 
            paste0("Use ", sQuote(paste("locdata(package =", ".packages(all.available = TRUE))")), 
                "\n", "to list the data sets in all *available* packages.")
        else NULL
        title<-paste(datadir,"data")
        y <- list(title = title, header = NULL, results = db, 
            footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }
    paths <- file.path(paths, datadir)
    for (name in names) {
        found <- FALSE
        for (p in paths) {
            if (file_test("-f", file.path(p, "Rdata.rds"))) {
                rds <- readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                  found <- TRUE
                  if (verbose) 
                    message(sprintf("name=%s:\t found in Rdata.rds", 
                      name), domain = NA)
                  thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
                  thispkg <- sub("_.*$", "", thispkg)
                  thispkg <- paste0("package:", thispkg)
                  objs <- rds[[name]]
                  lazyLoad(file.path(p, "Rdata"), envir = envir, 
                    filter = function(x) x %in% objs)
                  break
                }
                else if (verbose) 
                  message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
                    name, paste(names(rds), collapse = ",")), 
                    domain = NA)
            }
            if (file_test("-f", file.path(p, "Rdata.zip"))) {
                warning("zipped data found for package ", sQuote(basename(dirname(p))), 
                  ".\nThat is defunct, so please re-install the package.", 
                  domain = NA)
                if (file_test("-f", fp <- file.path(p, "filelist"))) 
                  files <- file.path(p, scan(fp, what = "", quiet = TRUE))
                else {
                  warning(gettextf("file 'filelist' is missing for directory %s", 
                    sQuote(p)), domain = NA)
                  next
                }
            }
            else {
                files <- list.files(p, full.names = TRUE)
            }
            files <- files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1L) {
                o <- match(fileExt(files), dataExts, nomatch = 100L)
                paths0 <- dirname(files)
                paths0 <- factor(paths0, levels = unique(paths0))
                files <- files[order(paths0, o)]
            }
            if (length(files)) {
                for (file in files) {
                  if (verbose) 
                    message("name=", name, ":\t file= ...", .Platform$file.sep, 
                      basename(file), "::\t", appendLF = FALSE, 
                      domain = NA)
                  ext <- fileExt(file)
                  if (basename(file) != paste0(name, ".", ext)) 
                    found <- FALSE
                  else {
                    found <- TRUE
                    zfile <- file
                    zipname <- file.path(dirname(file), "Rdata.zip")
                    if (file.exists(zipname)) {
                      Rdatadir <- tempfile("Rdata")
                      dir.create(Rdatadir, showWarnings = FALSE)
                      topic <- basename(file)
                      rc <- .Call(C_unzip, zipname, topic, Rdatadir, 
                        FALSE, TRUE, FALSE)
                      if (rc == 0L) 
                        zfile <- file.path(Rdatadir, topic)
                    }
                    if (zfile != file) 
                      on.exit(unlink(zfile))
                    switch(ext, R = , r = {
                      library("utils")
                      sys.source(zfile, chdir = TRUE, envir = envir)
                    }, RData = , rdata = , rda = load(zfile, 
                      envir = envir), TXT = , txt = , tab = , 
                      tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
                      txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
                        header = TRUE, as.is = FALSE), envir = envir), 
# Make the csv files separated by a comma not a semicolon.
                      CSV = , csv = , csv.gz = , csv.bz2 = , 
                      csv.xz = assign(name, read.table(zfile, 
                        header = TRUE, #sep = ";"
                        , as.is = FALSE), 
                        envir = envir),
# Semicolon separated datasets.
                      SCS = , scs = , scs.gz = , scs.bz2 = , 
                      scs.xz = assign(name, read.table(zfile, 
                        header = TRUE, sep = ";", as.is = FALSE), 
                        envir = envir),
                     found <- FALSE)
                  }
                  if (found) 
                    break
                }
                if (verbose) 
                  message(if (!found) 
                    "*NOT* ", "found", domain = NA)
            }
            if (found) 
                break
        }
        if (!found) 
            warning(gettextf("data set %s not found", sQuote(name)), 
                domain = NA)
    }
    invisible(names)
}
