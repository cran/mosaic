
#' @importFrom utils read.delim
NA

#' Read data files
#' 
#' A wrapper around various file reading functions.
#' 
#' @param file character:
#' 	 The name of the file which the data are to be read from. 
#' 	 This may also be a complete URL or a path to a compressed file.
#' 	 If it does not contain an absolute path, the file name is 
#' 	 relative to the current working directory, 
#' 	 `getwd()`.  Tilde-expansion is performed where supported. 
#' 	 See [read.table()] for more details.
#' 
#' @param header logical;  
#'     For `.txt` and `.csv` files, this indicates whether the first line of the file includes variables names.
#' 
#' @param na.strings character: strings that indicate missing data.
#' 
#' @param comment.char 
#' character: a character vector of length one containing a single character or an empty string. Use "" to turn 
#' off the interpretation of comments altogether.
#' 
#' @param filetype one of `"default"`, `"csv"`, `"txt"`, or `"rdata"`
#' indicating the type of file being loaded.  The default is to use the filename
#' to guess the type of file.
#' 
#' @param readr a logical indicating whether functions from the `readr` package should be
#'   used, if available.
#' 
#' @param \dots  additional arguments passed on to 
#'   [read.table()], or [load()] or one of the functions
#'   in the `readr` package.  Note that a message will indicate which 
#'   underlying function is being used.
#'   
#' @details
#' Unless `filetype` is specified,
#' `read.file` uses the (case insensitive) file extension to determine how to read
#' data from the file.  If `file` ends in `.rda` or `.rdata`, then
#' [load()] is used to load the file.  If `file`
#' ends in `.csv`, then [readr::read_csv()] or [read.csv()] is used.  
#' Otherwise, [read.table()] is used.
#' @param package if specified, files will be searched for among the documentation
#' files provided by the package.
#' @param stringsAsFactors a logical indicating whether strings should be converted to factors.
#'   This has no affect when using `readr`.
#' 
#' @return A data frame, unless `file` unless `filetype` is `"rdata"`, 
#' in which  case arbitrary objects may be loaded and a character vector
#' holding the names of the loaded objects is returned invisibly.
#' @seealso [read.csv()], [read.table()], 
#' [readr::read_table()], [readr::read_csv()], 
#' [load()].
#' 
#' @keywords util 
#' @examples
#' \dontrun{
#' Dome <- read.file("http://www.mosaic-web.org/go/datasets/Dome.csv")
#' }
#' @export

read.file <-
  function (file, header = T, na.strings = "NA",
            comment.char = NULL, 
            filetype = c("default", "csv", "txt", "tsv", "fw", "rdata"), 
            stringsAsFactors = FALSE,
            readr = FALSE,
            package = NULL, ...) 
{
    readr_available <- readr && rlang::is_installed('readr')
    using_readr <- FALSE
    
    if (!is.null(package)) {
      file <- docFile(file, package=package, character.only=TRUE)
    }
    filetype <- match.arg(tolower(filetype), choices=filetype)
    if (filetype == "default") {
      filetype <- "txt"
      if (regexpr("\\.csv$", tolower(file)) > 0) {
        filetype <- "csv"
      } 
      
      if (regexpr("\\.tsv$", tolower(file)) > 0) {
        filetype <- "tsv"
      } 
      
      if (regexpr("\\.fw$", tolower(file)) > 0) {
        using_readr <- TRUE
      }
      
      if ( regexpr("\\.rdata$", tolower(file)) > 0 || 
           regexpr("\\.rda", tolower(file)) >0 ){
        filetype <- "rdata"
      } 
    }
    
    if (filetype %in% c("csv", "tsv", "fw")) {
      # try to use readr for these
      using_readr <- TRUE
    }
    
    if (!file.exists(file) && grepl("https://", file)) {  # assume we are reading a URL
      if (! requireNamespace("RCurl")) stop("Package `RCurl' must be installed.")
      file <- textConnection(RCurl::getURL(file))
      using_RCurl <- TRUE
      using_readr <- FALSE
    }
    
    using_readr <- using_readr && readr_available
    
    if (using_readr){ rlang::check_installed('readr') }
    
    if (using_readr) { 
      if (! is.null(comment.char)) message("comment.char is currently being ignored.")
      if (length(na.strings) > 1) {
        message("Currently, only the first item in na.strings is used.")
        message("Additional items will be ignored.")
        na.strings = na.strings[1]
      } 
    }
    if (filetype == "csv") {
      if (using_readr) {
        message("Reading data with readr::read_csv()")
        return(as.data.frame(readr::read_csv(file, col_names = header, na = na.strings, ...)))
      } else {
        message("Reading data with read.csv()")
        return(read.csv(file, header=header, na.strings = na.strings, 
                        stringsAsFactors = stringsAsFactors, ...))
      }
    }
    
    if (filetype == "fw" && readr_available) {
      rlang::check_installed('readr')
      message("Reading data with readr::read_table()")
      return(as.data.frame(readr::read_table(file, col_names = header, na = na.strings, ...)))
    }
    
    if (filetype == "tsv") {
      if (using_readr) {
        message("Reading data with readr::read_tsv()")
        return(as.data.frame(readr::read_tsv(file, col_names = header, na = na.strings, ...)))
      } else {
        message("Reading data with read.delim()")
        return(read.delim(file, header = header, na = na.strings, ...))
      }
    }
    
    if (filetype == "rdata") {
      message("Reading data with load(); ignoring extra arguments.")
      varNames <- load(file)  
      return(invisible(varNames))
    }
    
    # fall through to read.table() for any other file format.
    message("Reading data with read.table()")
    return(
      read.table(file, header = header, na.strings = na.strings, stringsAsFactors=stringsAsFactors,...)
    )
}
