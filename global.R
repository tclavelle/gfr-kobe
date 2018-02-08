################################################
##
## Global file for upside model Kobe plot app
##
## By: Tyler Clavelle
## Date: 05/27/2016
##
################################################

## Load packages and data ----
## Load packages and data ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(MASS)
library(knitr)
library(shiny)
library(plotly)
library(vembedr)

# read kobe data list object
load("data/KobeAppData.rdata")

# source ggkobe function
source('ggKobe.R')

# Vector of region ids and names

zone<-c('Arctic Sea','Northwest Atlantic','Northeast Atlantic','West Central Atlantic','Eastern Central Atlantic','Mediterranean and Black Sea',
        'Southwest Atlantic','Southeast Atlantic','Western Indian Ocean','Eastern Indian Ocean','Northwest Pacific','Northeast Pacific','Western Central Pacific','Eastern Central Pacific',
        'Southwest Pacific','Southeast Pacific','Atlantic Antarctic','Indian Ocean Antarctic','Pacific Antarctic')

codes<-c('18','21','27','31','34','37','41','47','51','57','61','67','71','77','81','87','48','58','88') # codes

names<-data.frame(zone,codes,stringsAsFactors=F) # df of names and corresponding codes for every FAO major region

# list of dot color choices to use
dotcolor<-list()
dotcolor[['Global']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName')
dotcolor[['Country']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName',"By Country"='Country')
dotcolor[['ISSCAAP']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName')
dotcolor[['Region']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName')

# assign colors to use for kobe plot
KobeColors<- colorRampPalette(c("powderblue", "white",'#FFFF99'))

RGBcols<-c('#A0A0A0','#FC6969','#0B610B')

# Scatter plot theme
scatterTheme <- theme_bw() + theme(axis.text=element_text(size=10),
                                   axis.title=element_text(size=10, vjust=.15),
                                   plot.margin=unit(c(.25,0.25,0.25,0.25), "lines"),
                                   legend.title = element_text(size=10),
                                   legend.text= element_text(size=10),
                                   plot.title = element_text(lineheight=.8, size=10),
                                   strip.text.x = element_text(size = 10))