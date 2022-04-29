#' PackageProject4
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

library(kableExtra)
library(P3THISONE)
library(magrittr)
library(ggplot2)


myconstr = function(x, y, alpha){
  Rttest2 <- t.test(x, y, mu = 0, var.equal = TRUE, conf.level = 1-alpha)
  Rttest <- list(data = data.frame(x=x, y=y), Confidence_Interval = Rttest2$conf.int, P.value = Rttest2$p.value, Alpha = alpha)
  class(Rttest) <- "Rttest"
  Rttest
}
#Print Function
print.Rttest <- function(x,...) {
  data = c(Rttest$data,Rttest$Confidence_Interval,Rttest$P.value,Rttest$Alpha)
  #Rttest$data = x[["data"]][["x"]], x[["data"]][["y"]]
  #Rttest$Alpha = alpha[["Alpha"]]
  #dataframe = data.frame(Rttest$data, Rttest$Alpha)
  #kable(dataframe)
  kable_styling(kableExtra::kable(data,align = "c", col.names = names(data(Rttest))))

}
#plot function
plot.Rttest = function(x, y, pch =21, bg = "blue", cex = 3){
  plot(data,
       pch = pch,
       bg = bg,
       x = "x",
       y = "y"
  )
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
#data
set.seed(21)
x <- rnorm(30,5,2)

set.seed(23)
y <- rnorm(30,3,2)

alpha <- 0.05
Cat <- rep(c("A", "B"), c(30,30))
data.frame(MeanValue = c(x,y), TheData = Cat) -> data
library(ggplot2)
TheData <- ggplot(data) + geom_boxplot(aes(x = TheData, y = MeanValue, fill = TheData))
TheData

Rttest <-myconstr(x=x, y=y, alpha=0.5)
print(Rttest)
plot(Rttest$data)
class(Rttest)


