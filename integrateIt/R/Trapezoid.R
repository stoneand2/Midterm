#' A Trapezoid object 
#' 
#' Object of class \code{Trapezoid} are created by the \code{integrateIt} function. Objects of
#' this class have their own \code{print} and \code{plot} methods.
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
#' @aliases Trapezoid-class initialize,Trapezoid-method print,Trapezoid-method plot,Trapezoid-method
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

#' @export
# Print method for Trapezoid class (print is a S3 function, so we define it in this way)
print.Trapezoid <- function(trapezoid){
  print(trapezoid@result)
}

#' @export
# Plot method for Trapezoid class
setMethod(f="plot",
          # Class the method is used for
          signature="Trapezoid",
          # The method itself
          definition=function(x=NULL, y=x, ...){
            plot(x=NULL,
                 y=NULL,
                 # Limits slightly above/below min/max values of x, f(x)
                 xlim=c(min(x@x) - 1, max(x@x) + 1),
                 ylim=c(min(x@y)-1, max(x@y)+1), 
                 xlab="X",
                 ylab="f(X)", 
                 main="Graphical Representation of Trapezoidal Integration", 
                 cex.main=1)
            points(x@x, x@y, pch=16, cex=0.5)
            abline(h=0)
            invisible(sapply(1:(length(x@x)-1), FUN=function(i) segments(x0=x@x[i], y0=x@y[i], x1=x@x[i+1], y1=x@y[i+1])))
            invisible(sapply(1:(length(x@x)), FUN=function(i) segments(x0=x@x[i], y0=0, x1=x@x[i], y1=x@y[i])))
            
          }
)


