read.file <-
function (file, header = T, na.strings = c("NA", "", ".", "na", 
    "-"), comment.char = "#", ...) 
{
    if (!file.exists(file)) {
        return(NULL)
        message(paste("Missing file: ", file))
    }
    if (regexpr("\\.csv", file) > 0) {
        return(read.csv(file, header = header, na.strings = na.strings, 
            comment.char = comment.char, ...))
    }
    if (regexpr("\\.Rdata", file) > 0) {
        varNames <- load(file)
        invisible(varNames)
    }
    return(read.table(file, header = header, na.strings = na.strings, 
        comment.char = comment.char, ...))
}
