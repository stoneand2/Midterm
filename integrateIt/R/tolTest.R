#' Tolerance of integration by the Trapezoidal rule or Simpson's rule.
#'
#' tolTest is a function to conduct integral approximation until reaching a specified level of tolerance.
#'
#' @param fun A function f(x).
#' @param tolerance Indication of how much error user will allow from true integral value. 
#' @param rule A character string `Trap' or `Simp' to denote what type of integration approximation to utilize.
#' @param start An integer denoting a starting number of intervals.
#' @param correct The true answer for the integral. 
#' 
#' @return An object of class `Trapezoid' or 'Simpson' containing
#' \itemize{
#' \item \code{fun} A numeric vector of x values.
#' \item \code{tolerance} Indication of how much error user will allow from true integral value.
#' \item \code{rule} A character string `Trap' or `Simp' to denote what type of integration approximation to utilize.
#' \item \code{start}  An integer denoting a starting number of intervals.
#' \item \code{correct} The true answer for the integral.
#' \item \code{finaln} The final n. 
#' \item \code{abserror} The absolue error of the final estimate.
#' }
#' @author Andy Stone: \email{arstone@@wustl.edu}
#' @seealso \code{\link{integrateIt}}
#' @examples
#' easyfun <- function(x){
#'      x^2
#' }
#' tolTest(fun=easyfun, a=1, b=7, start=2, rule="Simp", correct=114, tolerance=1)
#' @rdname tolTest
#' @aliases tolTest,ANY-method
#' @export
setGeneric(name="tolTest",
           def=function(fun, a, b, tolerance, rule, start, correct)
           {standardGeneric("tolTest")}
)

setMethod(f="tolTest",
          definition=function(fun, a, b, tolerance, rule, start, correct){
            # Initial start value
            initial.start <- start
            # To ensure while loop runs the first time
            abserror <- tolerance+1
            # Trapezoidal 
            if(rule=="Trap"){
              # While statement that will break when we meet or are lower than tolerance level
              while(abserror > tolerance){
                h <- (b-a)/start
                x <- seq(a, b, by=h)
                y <- fun(x)
                if(start == 1){
                  result <- h/2 * (y[1] + y[2])
                }
                if(start > 1){
                  result <- h/2 * (y[1] + y[length(x)] + sum(2*y[(2):(length(x)-1)]))
                }
                abserror <- abs(result-correct)
                start <- start + 1
              }
              # Returns a list of the outputs
              return(list("fun"=fun, "a"=a, "b"=b, "tolerance"=tolerance, "rule"=rule, 
                          "start"=initial.start, "correct"=correct, "finaln"=start, "abserror"=abserror))
            }

            # Simpson's rule
            if(rule=="Simp"){
              # Check to see if n is even
              if(start %% 2 != 0){
                stop("When using Simpson's rule, n must be even. This means the number of points in x should be odd.")
              }
              # While statement that will break when we meet or are lower than tolerance level
              while(abserror > tolerance){
                h <- (b-a)/start
                x <- seq(a, b, by=h)
                y <- fun(x)
          
              # Just as in Trapezoidal rule, we calculate differently, here when n=2 and n>2
              # When n=2, no 4-2-4 pattern
              if(start == 2){
                result <- h/3 * (y[1] + y[2])
              }
              # When n>2 (and, of course, even), we have 4-2-4 pattern
              if(start > 2){
                result <- h/3 * (y[1] + 4*y[length(x)-1] + y[length(x)] + sum(rep(c(4,2), times=(start-2)/2)*y[(2):(length(x)-2)]))
              }
                abserror <- abs(result-correct)
                # Increase by 2 to ensure n remains even
                start <- start + 2
            }
              return(list("fun"=fun, "a"=a, "b"=b, "tolerance"=tolerance, "rule"=rule, 
                          "start"=initial.start, "correct"=correct, "finaln"=start, "abserror"=abserror))

            }
          }
)