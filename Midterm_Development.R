# Andy Stone
# Midterm
# March 23, 2016

# Setting working directory
setwd("~/github/Midterm")

# Packages to be utilized 
library(devtools); library(roxygen2)

# Creating the skeleton of the package (only run this once to start!)
# package.skeleton(name="integrateIt")

# devtools functions to be used during package development (run after changing any files)
current.code <- as.package("integrateIt") # Loads your package into R
load_all(current.code) # Loads the functions in your package
document(current.code) # Calls roxygen to document your files

# Creating first class (Trapezoid)
# Trapezoid class
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

# Creating second class (Simpson)
# Simpson class
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

# Creating integrateIt generic and method, starting with just Trapezoidal rule 
setGeneric(name="integrateIt",
           def=function(x, y, a, b, rule, return=T)
           {standardGeneric("integrateIt")}
)


setMethod(f="integrateIt",
          definition=function(x, y, a, b, rule, return=T){
            # Check to make sure rule is specified correctly
            if(rule != "Trap" & rule != "Simp"){
              stop("You need to specify either the `Trap' (Trapezoidal) or `Simp' (Simpson's) rule of integration.")
            }
            
            # Check to make sure x is >= 2 (otherwise, we can't take an integral)
            # This means that n will always be >= 1
            if(length(x) <= 1){
              stop("We can't calculate an integral under a single point.")
            }
            
            # Check to make sure there is a corresponding f(x) for each x
            if(length(x) != length(y)){
              stop("The number of observations in x and y differs. You need to input a f(x) for every x.")
            }

            # Finding index values of x that correspond to starting and ending values
            index.a <- which(x == a)
            index.b <- which(x == b)
            # Check to make sure start and end values actually exist 
            if(length(index.a) == 0 | length(index.b) == 0){
              stop("Please choose starting (a) and ending (b) values that correspond to known x values.")
            }
            # Checking to make sure start/end value aren't same value
            if(index.a == index.b){
              stop("Please choose different starting and ending values.")
            }
            
            # Finding number of x values to be used in integration
            # This allows start/end values to be different than beginning/end of x vector
            number.of.x <- (index.b - index.a) + 1
            
            # Calculating n, number of panels, which will be number of x used - 1
            # This allows n count to be correct when start/end values differ from beginnnig/end of x
            n <- number.of.x - 1
            
            # Calculating h, equal to b-a/n
            h <- (b-a) / (n)
            
            # Trapezoidal rule calculation
            if(rule == "Trap"){
              # Calculation will differ if n=1 or if n>1
              if(n == 1){
                result <- h/2 * (y[index.a] + y[index.b])
              }
              if(n > 1){
                result <- h/2 * (y[index.a] + y[index.b] + sum(2*y[(index.a+1):(index.b-1)]))
              }
              # Creates new instance of Trapezoid class with corresponding values 
              return(new("Trapezoid", x=x, y=y, a=a, b=b, result=result))
            }

          # Simpson's rule
          # This rule requires the number of intervals n to be even, and thus the number of points
          # in x to be odd. This is due to the pattern we use to calculate (4-2-4)
          if(rule=="Simp"){
            # Check to see if n is even
            if(n %% 2 != 0){
              stop("When using Simpson's rule, n must be even. This means the number of points in x should be odd.")
            }
            # Just as in Trapezoidal rule, we calculate differently, here when n=2 and n>2
            # When n=2, no 4-2-4 pattern
            if(n == 2){
              result <- h/3 * (y[index.a] + y[index.b])
            }
            # When n>2 (and, of course, even), we have 4-2-4 pattern
            # Accomplish this 4-2-4 pattern by fixing second to last y (f(x_n-2)) to be times 4
            # Then, we have 4-2 pattern, which will always be repeated (n-2)/2 times 
            # (i.e., (len(y)-3)/2 times)
            if(n > 2){
              result <- h/3 * (y[index.a] + 4*y[index.b-1] + y[index.b] + sum(rep(c(4,2), times=(n-2)/2)*y[(index.a+1):(index.b-2)]))
            }
            if(return==T){
            return(new("Simpson", x=x, y=y, a=a, b=b, result=result))
            }
          }
    }      
)






