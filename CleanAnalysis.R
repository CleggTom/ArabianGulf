##############
# Analysis of Life History Webs for 
# Clegg, Ali and Beckerman 2018: The Impact of Intraspecific Variation on Food Web Structure
##############
# Doi:
# Last edited 29.01.2017
# Copyright (c) 2017 the authors
##############
# Preparation ect.
# Clear workspace
rm(list = ls())

#Loading Packages
library(cheddar)
library(ggplot2)
library(tidyverse)
library(reshape2)

#Loading Life histort webs Data
LS_paths <- paste0("./Data/LS_Webs/",list.files("./Data/LS_Webs/"))
LS_webs <- vector("list", length(LS_paths))
for(i in 1:length(LS_paths)){
  LS_webs[[i]] <- LoadCommunity(LS_paths[i])
}

lapply(LS_webs, NumberOfNodes)

non_LS_paths <- paste0("./Data/Non_LS_Webs/",list.files("./Data/Non_LS_Webs/"))




Q1) How does LH influence Diversity-Complexity Relationships
