#!/usr/bin/Rscript

# Automatic package updating
#options("repos"="http://cran.csiro.au/")
library(utils)
update.packages(ask = FALSE)
my.packages = c("RColorBrewer","ggplot2")
to.download = which(!my.packages %in% rownames(installed.packages()))
if( length(to.download) > 0){
    install.packages(my.packages[to.download], clean=TRUE, dependencies=TRUE)
}
rm(my.packages, to.download)

# Scientific notation
options(scipen=12)

# My functions
`%ni%` <- Negate(`%in%`)
ct <- function(x){return(cat(x, sep="\n"))}
nl <-function (x) {return(cat(paste(shQuote(x, type="cmd"), collapse=", ")))}
sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

unnest <- function(x) {
  if(is.null(names(x))) {
    list(unname(unlist(x)))
  }
  else {
    c(list(all=unname(unlist(x))), do.call(c, lapply(x, unnest)))
  }
}


# libraries to load by default
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(reshape2)


# My ggplot2 custom themes
theme_greyDV <- function(base_size = 16, base_family = "Helvetica") {
    theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                            lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text =          element_text(size = rel(0.8)),
    strip.text =         element_text(size = rel(0.8)),

    axis.line =          element_blank(),
    axis.text.x =        element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y =        element_text(hjust = 1),
    axis.ticks =         element_line(colour = "grey50", size = 0.2),
    axis.title =         element_text(),
    axis.title.x =       element_text(vjust = 1),
    axis.title.y =       element_text(angle = 90),
    axis.ticks = element_line(colour = "black"), 
    axis.ticks.length =  unit(0.15, "cm"), 
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(colour = "grey80"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_rect(fill = NA, colour = "grey50"),
    panel.grid.major =   element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =   element_line(colour = "grey98", size = 0.5), 
    panel.margin =       unit(0.25, "lines"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    strip.background =   element_rect(fill = "grey80", colour = "grey50", size = 0.2),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),
    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
    complete = TRUE
  )
}

theme_black <- function(base_size = 12, base_family = "Helvetica") {
    theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                            lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text =          element_text(size = rel(0.8), colour = "white"),
    strip.text =         element_text(size = rel(0.8), colour = "white"),

    axis.line =          element_blank(),
    axis.text.x =        element_text(vjust = 1),
    axis.text.y =        element_text(hjust = 1),
    axis.ticks =         element_line(colour = "white", size = 0.2),
    axis.title =         element_text(colour = "white"),
    axis.title.x =       element_text(vjust = 1),
    axis.title.y =       element_text(angle = 90),
    axis.ticks.length =  unit(0.3, "lines"),
    axis.ticks.margin =  unit(0.5, "lines"),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "black", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8), colour = "white"),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0, colour = "white"),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   "vertical",
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = "black", colour = NA),
    panel.border =       element_rect(fill = NA, colour = "white"),
    panel.grid.major =   element_line(colour = "grey20", size = 0.2),
    panel.grid.minor =   element_line(colour = "grey5", size = 0.5),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   element_rect(fill = "grey30", colour = "grey10"),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),

    plot.background =    element_rect(colour = "black", fill = "black"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),

    complete = TRUE
  )
}
