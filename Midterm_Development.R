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









