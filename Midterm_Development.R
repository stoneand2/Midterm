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
                   result = "numeric"
         ),
         prototype = prototype(
           x = numeric(),
           y = numeric(),
           result = numeric()
         )
)

#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, x, y, result){
            .Object@x <- x
            .Object@y <- y
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
                   result = "numeric"
         ),
         prototype = prototype(
           x = numeric(),
           y = numeric(),
           result = numeric()
         )
)

#' @export
setMethod("initialize", "Simpson", 
          function(.Object, x, y, result){
            .Object@x <- x
            .Object@y <- y
            .Object@result <- result
            value=callNextMethod()
            return(value)
          }
) 

# Creating integrateIt generic and method, starting with just Trapezoidal rule 
setGeneric(name="integrateIt",
           def=function(x, y, a, b, rule)
           {standardGeneric("integrateIt")}
)


setMethod(f="integrateIt",
          definition=function(x, y, a, b, rule){
            # Check to make sure rule is specified correctly
            
            
            # Calculating n, number of panels, which will be x-1
            n <- length(x) - 1
            # Calculating h, equal to b-a/n
            h <- (b-a) / (n)
            # Finding index values of x that correspond to starting and ending values
            index.a <- which(x == a)
            index.b <- which(x == b)
            
            # Trapezoidal calculation
            if(rule == "Trapezoidal"){
              print("Trapezoidal!")
              
              # Calculation will differ if n=1 or if n>1
              if(n == 1){
                result <- h/2 * (y[index.a] + y[index.b])
              }
              if(n > 1){
                result <- h/2 * (y[index.a] + y[index.b] + sum(2*y[(index.a+1):(index.b-1)]))
              }
              
              
            }
            
            
            
            
            
          }
)




























