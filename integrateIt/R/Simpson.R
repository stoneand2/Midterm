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
#' @aliases Simpson-class initialize,Simpson-method print,Simpson-method 
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

#' @export
# Print method for Simpson class (print is a S3 function, so we define it in this way)
print.Simpson<- function(simpson){
  print(simpson@result)
}

#' @export
# Plot method for Simpson class
setMethod(f="plot",
          # Class the method is used for
          signature="Simpson",
          # The method itself
          definition=function(x=NULL, y=x, ...){
            length.x <- length(x@x)
            # Function to get the X values for the midpoints
            mid.function <- function(i){
            (x@x[i] + x@x[i+1]) / 2
            }
            # Getting the X values for the midpoints
            midpoint.xs <- unlist(sapply(1:(length(x@x)-1), FUN=mid.function))
            # Opening plot
            plot(x=NULL,
                 y=NULL,
                 # Limits slightly above/below min/max values of x, f(x)
                 xlim=c(min(x@x) - 1, max(x@x) + 1),
                 ylim=c(min(x@y)-1, max(x@y)+1), 
                 xlab="X",
                 ylab="f(X)", 
                 main="Graphical Representation of Simpson's Integration", 
                 cex.main=1)
            # Plotting observed points
            points(x@x, x@y, pch=16, cex=0.5)
            abline(h=0)
            
            # Calculating f(m) (y values for midpoints)
            mid.fy.calculator <- function(i){
              # X values between the a and b
              X <- seq(from=x@x[i-1], to=x@x[i+1], length.out=100)
              # Using those X values as X in our calculation of p(x)
              # Calculates p(x) for range before and after the midpoint, from a to b
              # First part of equation on slide 14
              first.part <- (x@y[i-1]) * ((X - x@x[i])*(X - x@x[i+1]))/((x@x[i-1] - x@x[i])*(x@x[i-1] - x@x[i+1]))
              # Second part of equation on slide 14
              second.part <- (x@y[i]) * ((X - x@x[i-1])*(X - x@x[i+1]))/((x@x[i] - x@x[i-1])*(x@x[i] - x@x[i+1]))
              # Third part
              third.part <- (x@y[i+1]) * ((X - x@x[i-1])*(X - x@x[i]))/((x@x[i+1] - x@x[i-1])*(x@x[i+1] - x@x[i])) 
              # Calculates p(x) for range before and after the midpoint, from a to b
              pofx <- first.part + second.part + third.part
              # Adds the parabola to the graph
              # Alternating colors for the parabolas
              colors <- c("red","forestgreen","dodgerblue4")
              lines(X, pofx, col=colors[(i%%3)+1])
            }
            # Actually runs the function
            invisible(unlist(sapply(2:(length(x@x)-1), FUN=mid.fy.calculator)))
            # Adds the vertical lines to segment zones
            invisible(sapply(1:(length(x@x)), FUN=function(i) segments(x0=x@x[i], y0=0, x1=x@x[i], y1=x@y[i])))
            
          }
)


