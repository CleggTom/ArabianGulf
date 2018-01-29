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

#Loading LS webs
LS_paths <- paste0("./Data/LS_Webs/",list.files("./Data/LS_Webs/"))
LS_webs <- vector("list", length(LS_paths))
for(i in 1:length(LS_paths)){
  LS_webs[[i]] <- LoadCommunity(LS_paths[i])
}

#Loading non-LS webs
non_LS_paths <- paste0("./Data/Non_LS_Webs/",list.files("./Data/Non_LS_Webs/"))
non_LS_webs <- vector("list", length(non_LS_paths))
for(i in 1:length(non_LS_paths)){
  non_LS_webs[[i]] <- LoadCommunity(non_LS_paths[i])
}

##############
#Q1a) How does LH influence Diversity-Complexity Relationships

#Get DF with Nodes and Links
Nodes <- log(c(sapply(LS_webs,NumberOfNodes),sapply(non_LS_webs,NumberOfNodes)))
Links <- log(c(sapply(LS_webs,NumberOfTrophicLinks),sapply(non_LS_webs,NumberOfTrophicLinks)))
names <- c(sapply(LS_webs,function(x) x$properties$title),sapply(non_LS_webs,function(x) x$properties$title))
LS <- c(sapply(LS_webs,function(x) x$properties$LS),rep("other",length(non_LS_webs)))
Web <- c(gsub(" Lifestage| Taxanomic","",sapply(LS_webs,function(x) x$properties$title)),
         rep("other",length(non_LS_webs)))

NL_all <- data.frame(names = names,web = Web ,LS = LS, nodes = Nodes, links = Links)

#Regression
#Log(link) ~ log(Nodes)
reg_data <- NL_all[NL_all$LS != "LS",]
reg_data$nodes <- reg_data$nodes
reg_data$links <- reg_data$links

NL_model <- lm(links~nodes,data = reg_data)

reg_data$pred <- predict(NL_model)

#plotting
#defining colour vars
cbPalette <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
names(cbPalette) <- (unique(Web))

NvL <- ggplot(NL_all,aes(x = (nodes), y = (links), colour = web))+
        geom_point(size = 3)+
        geom_line(data = NL_all[NL_all$web != "other",])+
        scale_color_manual(values = cbPalette)+labs(x = "Log (nodes)", y = "Log (links)")+
        theme_classic() + 
        theme(legend.position = c(0.9,0.1),legend.justification = c(1,0),legend.title = element_blank())+
        geom_line(data = reg_data,aes(x=nodes,y=pred,group = NULL,colour = NULL))

#Q1b) How is this affected by LS overlap?

#get the overlap for each web

#define similarity
function



##############
