##############
# Analysis of Life History Webs
# Script 4) Generating null models for motif analysis for:
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
library(igraph)
library(tidyverse)

#Loading LS webs
LS_paths <- paste0("../Data/LS_Webs/",list.files("../Data/LS_Webs/"))
LS_webs <- vector("list", length(LS_paths))
for(i in 1:length(LS_paths)){
  LS_webs[[i]] <- LoadCommunity(LS_paths[i])
}

#Loading Motif Functions
source("./S1_MotifFunctions.R")
##############

LS_webs.Shuffled <- vector("list", length(LS_webs))

for(i in 1:length(LS_webs)){

  print("Motifing web: ")
  print(i)
  #Get numbers of links
  SingleLinks <- NSingleDouble(LS_webs[[i]])[1]
  DoubleLinks <- NSingleDouble(LS_webs[[i]])[2]
  CannibalLinks <- length(Cannibals(LS_webs[[i]]))

  S <- NumberOfNodes(LS_webs[[i]])

  Shuffled <- vector("list", 1000)
  for(j in 1:1000){

    print(j)

    mat <- matrix(0,S,S)
    #randomly assign single nodes
    Links <- combn(1:S,2)[,sample(1:ncol(combn(1:S,2)),SingleLinks+DoubleLinks,replace=F)]

    for(k in 1:(SingleLinks+DoubleLinks)){
      mat[Links[1,k],Links[2,k]] <- 1
    }

    #and double nodes
    Links <- Links[,sample(1:ncol(Links),DoubleLinks,replace=F)]
    for(k in 1:DoubleLinks){
      mat[Links[2,k],Links[1,k]] <- 1
    }

    #and cannibal nodes
    Links <- sample(1:S,CannibalLinks)
    for(k in 1:CannibalLinks){
      mat[Links[k],Links[k]] <- 1
    }

    Shuffled[[j]] <- mat
  }

LS_webs.Shuffled[[i]] <- Shuffled

}

saveRDS(LS_webs.Shuffled,"../Data/Motifs/Shuffled_Motifs.rds")
