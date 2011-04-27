dfapply <-
function (data, FUN, select = is.numeric, ...) 
{
    if (is.function(select)) {
        select <- sapply(data, select)
    }
    if (is.logical(select)) {
        select <- rep(select, length.out = dim(data)[2])
        select <- which(select)
    }
    if (!is.numeric(select)) {
        stop("Unusable selection parameter.")
    }
    apply(data[, select, drop = F], 2, FUN, ...)
}
