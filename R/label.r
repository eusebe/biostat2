##' add a label (description) to an object (can be the column of a data.frame)
##'
##' @param x an object (can be the column of a data.frame)
##' @param value the label of the object (a character vector of length 1).
##' @author David Hajage
##' @export
`add_label<-` <- function (x, value) {
    if ((!is.character(value) & !is.null(value)) | length(value) > 1) {
        stop("`value` should be a single character string or NULL")
    } 
    attr(x, "label") <- value
    return(x)
}

##' get a label (description) to an object (can be the column of a data.frame)
##'
##' @param x an object (can be the column of a data.frame)
##' @author David Hajage
##' @export
get_label <- function (x) {
    res <- attr(x, "label", exact = TRUE)
    if (is.null(res)) {
        res <- ""
    }
    return(res)
}
