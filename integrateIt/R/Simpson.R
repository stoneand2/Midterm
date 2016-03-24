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
            indices.for.midpoints <- seq(from=x@a, to=x@b, by=2)
            
            mid.function <- function(){
            (x@x[i] + x@x[i+1]) / 2
            }
            sapply(1:length(x@x)-1, FUN=mid.function)
            
            
            
            
            
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

# # Midpoint
# v <- (a + b) / 2
# index.v <- which(x == v)
# # f(a)
# f.a <- y[index.a]
# # f(v)
# f.v <- y[index.v]
# # f(w)
# f.w <- y[index.b]
# 
# # first fraction
# ((x - v)*(x - b)) / ((a - v)*(a - b))
# 
# # second fraction
# ((x - a)*(x - b)) / ((v - a)*(v - b))
# 
# # third fraction
# ((x - a)*(x - v)) / ((b - a)*(b - v))
# 
# function1 <- function(x, y, a, b){
#   v <- (x[i] + x[i+1]) / 2
#   print(v)
#   index.v <- which(x == v)
#   # f(a)
#   f.a <- y[index.a]
#   # f(v)
#   f.v <- y[index.v]
#   # f(w)
#   f.b <- y[index.b]
#   
#   return((f.a*((x - v)*(x - b)) / ((a - v)*(a - b))) + (f.v*((x - a)*(x - b)) / ((v - a)*(v - b))) +
#     (f.b * ((x - a)*(x - v)) / ((b - a)*(b - v))))
#   
# }
# 
# sapply(1:7, FUN=function1(x=x, y=y, a=1, b=7))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
