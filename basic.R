#basic

library(reshape2)
library(ggplot2)
library(doBy)
library(psych)
library(dplyr)
library(DMwR)
library(stringr)

#functions
dstats<-function(x,...){c(mean=mean(x,na.rm=T,...),sd=sd(x,na.rm=T,...))}
source('E:/project/e-smoking/data/e-smoking/histWithNormalCurve.R')

lay_out = function(...) {
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]],
                                           layout.pos.col = x[[i]][[3]]))
  }
}

#仕事仕事
source('E:/project/e-smoking/data/e-smoking/cleaning2.R', encoding = 'UTF-8')

