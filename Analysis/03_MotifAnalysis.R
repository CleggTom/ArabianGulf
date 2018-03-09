##############
# Analysis of Life History Webs
# Script 3) Generating motifs for the real webs for:
# Clegg, Ali and Beckerman 2018: The Impact of Intraspecific Variation on Food Web Structure
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
library(cheddar)
library(tidyverse)
library(reshape2)
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

#Convert communities to igraph
LS_webs.igraph <- lapply(LS_webs,ToIgraph)
#Get motifs
LS_webs.motif <- matrix(nrow=10,ncol=13)
row.names(LS_webs.motif) <- sapply(LS_webs,function(x) x$properties$title)
colnames(LS_webs.motif) <- paste0(c(rep("S",5),rep("D",8)),  c(1:5,1:8))
for(i in 1:10){
  print(paste(LS_webs[[i]]$properties$title))
  for(j in 1:13){
    print(paste('motif',j))
    LS_webs.motif[i,j] <- graph.count.subisomorphisms.vf2(LS_webs.igraph[[i]],subgraph3.graph[[j]])
  }
}

#write to csv
write.csv(LS_webs.motif,"../Data/Motifs/RealMotifs.csv")
