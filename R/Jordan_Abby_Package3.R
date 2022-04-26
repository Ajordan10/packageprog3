library(kableExtra)
library(P3THISONE)
#' PackageProject3
#'
#' @param x X variable as as vector
#' @param y Y variable as a vector
#' @param alpha
#'
#' @return A constructor
#' @export
#'
#' @examples
#'
#' Repo Link: https://github.com/Ajordan10/packageprog3.git

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
#CHECK FUNCTION
check(
  pkg = ".",
  document = NULL,
  build_args = NULL,
  manual = FALSE,
  cran = TRUE,
  env_vars = c(NOT_CRAN = "true"),
  quiet = TRUE,
  check_dir = tempdir(),
  cleanup = TRUE,
  vignettes = TRUE,
  error_on = c("never", "error", "warning", "note")
)

#data
set.seed(21)
x <- rnorm(30,5,2)

set.seed(23)
y <- rnorm(30,3,2)

alpha <- 0.05

Rttest <-P3THISONE::myconstr(x=x, y=y, alpha=0.5)
library(P3THISONE)
kableExtra::kable(print(Rttest))
print("https://github.com/Ajordan10/packageprog3.git")
#class(Rttest)


