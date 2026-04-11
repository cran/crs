## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(crs.messages = FALSE)

## -----------------------------------------------------------------------------
library(crs)
set.seed(42)
n <- 250
x1 <- runif(n)
x2 <- runif(n)
y <- sin(2 * pi * x1) + x2 + rnorm(n, sd = 0.2)
dat <- data.frame(y, x1, x2)

fit <- crs(y ~ x1 + x2, data = dat)
summary(fit)

## ----fig.width = 6, fig.height = 4--------------------------------------------
plot(x1, y, cex = 0.35, col = "grey")

grid_x1 <- seq(min(x1), max(x1), length.out = 200)
newdata <- data.frame(
  x1 = grid_x1,
  x2 = mean(x2)
)
pred <- predict(fit, newdata = newdata)

lines(grid_x1, pred, col = 2, lwd = 2)

