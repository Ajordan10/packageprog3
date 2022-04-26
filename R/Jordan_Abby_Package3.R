library(kableExtra)
#' PacakageProject3
#'
#' @param x X variable as as vector
#' @param y Y variable as a vector
#' @param alpha
#'
#' @return A constructor
#' @export
#'
#' @examples
#' Insert Example

myconstr = function(x, y, alpha){
  Rttest2 <- t.test(x, y, mu = 0, var.equal = TRUE, conf.level = 1-alpha)
  Rttest <- list(data = data.frame(x=x, y=y), Confidence_Interval = Rttest2$conf.int, P.value = Rttest2$p.value, Alpha = alpha)
  class(Rttest)
  Rttest
}
#Print Function
print.Rttest <- function(x,...) {
  data = c(Rttest$data,Rttest$Confidence_Interval,Rttest$P.value,Rttest$Alpha)
  #kable(Rttest$data)

}
#data
set.seed(21)
x <- rnorm(30,5,2)

set.seed(23)
y <- rnorm(30,3,2)

alpha <- 0.05

Rttest <-P3THISONE::myconstr(x, y, alpha)

class(Rttest)
kableExtra::kable(print(Rttest))
