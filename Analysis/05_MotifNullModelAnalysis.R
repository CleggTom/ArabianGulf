##############
# Analysis of Life History Webs
# Script 4) Analysing null model motifs for:
# Clegg, Ali and Beckerman 2018: The Impact of Intraspecific Variation on Food Web Structure
# NOTE: This Script is very computationaly intensive and should be run on multiple
# cores when possible
##############
# Doi:
# Last edited 01.02.2017
# Copyright (c) 2017 the authors
##############
# Preparation ect.
# Clear workspace
rm(list = ls())
set.seed(1)

#Loading Packages
library(tidyverse)
library(cheddar)
library(igraph)

#Loading LS webs
LS_paths <- paste0("../Data/LS_Webs/",list.files("../Data/LS_Webs/"))
LS_webs <- vector("list", length(LS_paths))
for(i in 1:length(LS_paths)){
  LS_webs[[i]] <- LoadCommunity(LS_paths[i])
}

#Loading Motif Functions
source("./S1_MotifFunctions.R")
##############

#Load Null Model Data
LS_webs.nullModels <- readRDS("../Data/Motifs/Shuffled_Motifs.rds")

#Move through each web
for(i in 1:length(LS_webs.nullModels)){
  print("web: ",i)
  #define motif results dataFrame
  results <- matrix(0,nrow=1000,ncol=13)
  #move through each itteration
  for(j in 1:1000){
      print("itter: ",j)
    LS_webs.igraph <- graph_from_adjacency_matrix(LS_webs.nullModels[[i]][[j]])
    for(k in 1:13){
      print("motif: ",j)
      results[j,k] <- graph.count.subisomorphisms.vf2(LS_webs.igraph,subgraph3.graph[[k]])
    }
  }
  filename <- paste0("../Data/Motifs/",str_replace(LS_webs[[i]]$properties$title," ",""),"Motifs.csv")
  write.csv(results,filename)
}
