## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)

## ----pressureCode, eval=FALSE-------------------------------------------------
#  library(stats)
#  set.seed(753)
#  bx.p <- boxplot(split(rt(100, 4),
#                        gl(5, 20)), plot=FALSE)
#  bxp(bx.p, notch = FALSE, boxfill = "lightblue",
#      frame = FALSE, outl = TRUE,
#      main = "Example from help(bxp)")

## ----pressureFig, echo=FALSE--------------------------------------------------
library(stats)
set.seed(753)
bx.p <- boxplot(split(rt(100, 4),
                      gl(5, 20)), plot=FALSE)
bxp(bx.p, notch = FALSE, boxfill = "lightblue",
    frame = FALSE, outl = TRUE,
    main = "Example from help(bxp)")

## ----kableEx------------------------------------------------------------------
knitr::kable(mtcars[1:5, 1:8],
             caption="(Parts of) the mtcars dataset")

