## ----include=FALSE------------------------------------------------------------
# Only run code chunks if Suggests packages available
suggestsavailable <-
  sapply(c("lfe", "haven", "AER", "estimatr", "fixest"), requireNamespace, quietly = TRUE)
evalcond <- all(suggestsavailable)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = evalcond
)

## ----setup--------------------------------------------------------------------
library(haven)
library(ivreg)
library(AER)
library(estimatr)
library(fixest)
library(lfe)
library(OneSampleMR)

## -----------------------------------------------------------------------------
url <- "http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta"
dat <- haven::read_dta(url)
mod <- ivreg::ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
summary(mod)
fsw(mod)

## -----------------------------------------------------------------------------
mod2 <- AER::ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
summary(mod2)
fsw(mod2)

## -----------------------------------------------------------------------------
library(estimatr)
mod3 <- iv_robust(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat, se_type = "classical")
tidy(mod3)
fsw(mod3)

## -----------------------------------------------------------------------------
library(fixest)
mod4 <- feols(lwage ~ 1 | educ + exper ~ age + kidslt6 + kidsge6, data = dat)
summary(mod4)
fsw(mod4)

## -----------------------------------------------------------------------------
modst2 <- felm(lwage ~ 1 |
                 0 | (educ | exper ~ age + kidslt6 + kidsge6), data = dat)
summary(modst2)
t(sapply(modst2$stage1$lhs, function(lh) {
  waldtest(modst2$stage1, ~ age | kidslt6 | kidsge6, lhs = lh)
}))
condfstat(modst2, quantiles = c(0.025, 0.975))

