#' A Simpson object 
#' 
#' Object of class \code{Simpson} are created by the \code{integrateIt} function. Objects of
#' this class have their own \code{print} method.
#'
#' 
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{x} A numeric vector of x values.
#' \item \code{y} A numeric vector of y=f(x) values.
#' \item \code{a} A numeric starting value, corresponding to a value of x, for integration.
#' \item \code{b} A numeric ending value, corresponding to a value of x, for integration.
#' \item \code{result} The result of integration from a to b of f(x) with respect to x using Simpson's rule.
#' }
#'
#' @author Andy Stone: \email{arstone@@wustl.edu}
#' @aliases Simpson-class initialize,Simpson-method 
#' @rdname Simpson
#' @export
setClass(Class="Simpson", 
         slots = c(x = "numeric",
                   y = "numeric",
                   a = "numeric",
                   b = "numeric",
                   result = "numeric"
         ),
         prototype = prototype(
           x = numeric(),
           y = numeric(),
           a = numeric(),
           b = numeric(),
           result = numeric()
         ), 
         # Same validity checks as in integrateIt, just in case user makes object manually
         validity=function(object){
           # This calls integrateIt, and just suppresses integrateIt's default return of a new object
           # I do this so I don't need to write out all the checks again
           integrateIt(x=object@x, y=object@y, a=object@a, b=object@b, rule="Simp", return=F)
         }
)

#' @export
setMethod("initialize", "Simpson", 
          function(.Object, x, y, a, b, result){
            .Object@x <- x
            .Object@y <- y
            .Object@a <- a
            .Object@b <- b
            .Object@result <- result
            value=callNextMethod()
            return(value)
          }
)