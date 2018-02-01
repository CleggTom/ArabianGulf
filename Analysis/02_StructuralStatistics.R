##############
# Analysis of Life History Webs
# Script 2) Structural Statistics and Niche model analysis for: 
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
library(gridExtra)

#Loading LS webs
LS_paths <- paste0("../Data/LS_Webs/",list.files("../Data/LS_Webs/"))
LS_webs <- vector("list", length(LS_paths))
for(i in 1:length(LS_paths)){
  LS_webs[[i]] <- LoadCommunity(LS_paths[i])
}
##############
#get web statistics

web_stats <- data.frame(title  = sapply(LS_webs, function(x) x$properties$title),
                        LS    = sapply(LS_webs, function(x) x$properties$LS),
                        S = sapply(LS_webs, NumberOfNodes),
                        L = sapply(LS_webs, NumberOfTrophicLinks),
                        basal = sapply(LS_webs, FractionBasalNodes),
                        inte = sapply(LS_webs, FractionIntermediateNodes),
                        top   = sapply(LS_webs, FractionTopLevelNodes),
                        gen   = sapply(LS_webs, function(x) mean(TrophicGenerality(x))),
                        gensd = sapply(LS_webs, function(x) sd(NormalisedTrophicGenerality(x))),
                        vun   = sapply(LS_webs, function(x) mean(TrophicVulnerability(x))),
                        vunsd = sapply(LS_webs, function(x) sd(NormalisedTrophicVulnerability(x))))

##############
# Get niche model statistics

# Define functions
niche_model <- function(S,C){
  #assign niche values
  n <- sort(runif(S))
  #assign ranges
  beta <- (1 - (2 * C)) / (2 * C)
  r <- n*(1 - (1 - runif(S))^(1/beta))
  r[1] <- 0
  #assign centers
  c <- r/2 + runif(S) * (n - r/2)
  #get final matrix
  mat <- matrix(0,S,S)
  for(i in 1:S){
    feeding <- (c[i] - r[i]/2) < n  &  (c[i] + r[i]/2) > n
    mat[feeding,i] <- 1
  }
  return(mat)
}

Stats_matrix <- function(mat){
  S = nrow(mat)
  L = sum(mat)
  
  basal = sum(colSums(mat) == 0)/S
  top   = sum(colSums(t(mat)) == 0)/S
  int   = 1 - basal - top
  gen   = mean(colSums(mat))
  vun   = mean(rowSums(mat))
  gensd = sd(colSums(mat)/(L/S))
  vunsd = sd(rowSums(mat)/(L/S))
  
  return(c(S,L,basal,int,top,gen,gensd,vun,vunsd))
}

# Get the web stats
stats <- vector(length = length(LS_webs),mode = "list")
for(n in 1:length(LS_webs)){
  print(web_stats$title[n])
  results <- matrix(0,nrow = 1000,ncol = 9)
  for(web in 1:1000){
    results[web,] <- Stats_matrix(niche_model(web_stats$S[n], web_stats$L[n]/web_stats$S[n]^2))
  }
  results <-  as.data.frame(results)
  colnames(results) <- c('S','L','basal','int','top','gen','gensd','vun','vunsd')
  results$title <- web_stats$title[n]
  stats[[n]] <- results
}

stats <- do.call("rbind", stats) %>% melt()
web_stats <- melt(web_stats)
StrucStats <- stats %>% 
  group_by(title,variable) %>%
  dplyr::summarise(model.median = median(value),
                   model.percent.95 = quantile(value,0.95),
                   model.percent.05 = quantile(value,0.05)) %>%
  merge(.,web_stats,by = c('variable','title'))     

#calculate ME
StrucStats$ME <- NA
#get the MEs
for(i in 1:nrow(StrucStats)){
  StrucStats$ME[i] <- (StrucStats$model.median[i] - StrucStats$value[i]) 
  if(StrucStats$ME[i] < 0){
    a <- StrucStats$ME[i] / (StrucStats$model.median[i] - StrucStats$model.percent.95[i])
  } else {
    a<- StrucStats$ME[i] / (StrucStats$model.median[i] - StrucStats$model.percent.05[i])
  }
  StrucStats$ME[i] <- abs(a)
}

ggplot(StrucStats,aes(x=title,y=ME,colour = title))+
  geom_point()+
  facet_grid(variable~LS,scales = 'free')+
  geom_hline(yintercept=1)
