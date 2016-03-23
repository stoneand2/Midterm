#' A Trapezoid object 
#' 
#' Object of class \code{Trapezoid} are created by the \code{integrateIt} function. Objects of
#' this class have their own \code{print} method.
#'
#' 
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{x} A numeric vector of x values.
#' \item \code{y} A numeric vector of y=f(x) values.
#' \item \code{a} A numeric starting value, corresponding to a value of x, for integration.
#' \item \code{b} A numeric ending value, corresponding to a value of x, for integration.
#' \item \code{result} The result of integration from a to b of f(x) with respect to x using the Trapezoidal rule.
#' }
#'
#' @author Andy Stone: \email{arstone@@wustl.edu}
#' @aliases Trapezoid-class initialize,Trapezoid-method 
#' @rdname Trapezoid
#' @export
setClass(Class="Trapezoid", 
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
           integrateIt(x=object@x, y=object@y, a=object@a, b=object@b, rule="Trap", return=F)
         }
)

#' @export
setMethod("initialize", "Trapezoid", 
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