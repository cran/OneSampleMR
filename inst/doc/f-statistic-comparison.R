## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(haven)
library(ivreg)
library(OneSampleMR)

## -----------------------------------------------------------------------------
url <- "http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta"
dat <- haven::read_dta(url)
mod <- ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
summary(mod)
fsw(mod)

## -----------------------------------------------------------------------------
library(lfe)

modst2 <- felm(lwage ~ 1 | 0 | (educ | exper ~ age + kidslt6 + kidsge6),
               data = dat)
summary(modst2)
t(sapply(modst2$stage1$lhs, 
         function(lh) waldtest(modst2$stage1, 
                               ~ age | kidslt6 | kidsge6, lhs = lh)))
condfstat(modst2, quantiles = c(0.025, 0.975))

