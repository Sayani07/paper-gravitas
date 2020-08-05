## ---- load
library(bookdown)
library(rticles)
library(knitr)
library(tidyverse)
library(lubridate)
library(lvplot)
library(ggridges)
library(tsibble)
if (requireNamespace("gravitas") == F)
  remotes::install_github("Sayani07/gravitas")
library(gravitas)
library(ggpubr)
library(xtable)
library(kableExtra)
library(tibble)

## ---- linear-time
knitr::include_graphics("Figs/linear-ex.png")

## ---- circular-dow
knitr::include_graphics("Figs/circular-ex.png")
