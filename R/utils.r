##' Create functions that capture their values, errors, and warnings
##'
##' Convert plain old functions into functions that capture their values, errors, and warnings. Comes from: https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
##'
##' @param fun A function
##' @author David Hajage
##' @return A function
##' @export
factory <- function(fun) {
    function(...) {
        warn <- err <- NULL
        res <- withCallingHandlers(
            tryCatch(fun(...), error=function(e) {
                err <<- conditionMessage(e)
                NULL
            }), warning=function(w) {
                warn <<- append(warn, conditionMessage(w))
                invokeRestart("muffleWarning")
            })
        if (is.null(warn)) warn = "OK"
        if (is.null(err)) err = "OK"
        list(value = res, warn=warn, err=err)
    }
}
