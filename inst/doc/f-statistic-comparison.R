## ----include=FALSE------------------------------------------------------------
# Only run code chunks if Suggests packages available
suggestsavailable <-
  sapply(c("lfe", "haven"), requireNamespace, quietly = TRUE)
evalcond <- all(suggestsavailable)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = evalcond
)

## ----setup--------------------------------------------------------------------
library(haven)
library(ivreg)
library(lfe)
library(OneSampleMR)

## -----------------------------------------------------------------------------
url <- "http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta"
dat <- haven::read_dta(url)
mod <- ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
summary(mod)
fsw(mod)

## -----------------------------------------------------------------------------
modst2 <- felm(lwage ~ 1 |
                 0 | (educ | exper ~ age + kidslt6 + kidsge6), data = dat)
summary(modst2)
t(sapply(modst2$stage1$lhs, function(lh) {
  waldtest(modst2$stage1, ~ age | kidslt6 | kidsge6, lhs = lh)
}))
condfstat(modst2, quantiles = c(0.025, 0.975))

