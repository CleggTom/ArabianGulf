### Analysing webs together
library(cheddar)
library(plyr)
library(dplyr)
library(readr)
library(randomNames)
library(ggplot2)
library(reshape2)

setwd("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf")

## Weddell Sea
Weddell <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Weddell_Sea/Weddell.csv",)

links <- data.frame(consumer = Weddell$con.taxonomy, resource = Weddell$res.taxonomy)
nodes <- data.frame(node = unique(c(Weddell$con.taxonomy,Weddell$res.taxonomy))) 
props <- list(title = 'Weddell Sea')

Weddell <- Community(nodes = nodes,
                     trophic.links = links,
                     properties = props)
rm(links,nodes,props)
# Benguela 
data("Benguela")

#Barents Sea
Barents <- LoadCommunity("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Barents")

#Ythan
data("YthanEstuary")

#ChesapeakeBay
data("ChesapeakeBay")

#Caribbean
Jam <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Caribbean/jamaica.csv")

guildkey <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Caribbean/guildkey.csv")

guildkey <- guildkey[-755,]
n.guild <- unique(guildkey$`Guild Number`)

Matrix <- matrix(0,nrow = max(n.guild),ncol = max(n.guild))

jam.mat <- Matrix
for(i in n.guild){
  a <- subset(Jam$Prey,Jam$`Guild Number` == i)
  if(length(a) > 0){
    a <- strsplit(a,split = ',')[[1]]
    if(all(a != '.')){
      a <- as.numeric(gsub(x = a,' ',''))
      for(j in a){
        jam.mat[j,i] <- 1
      }
    }
  }
} 


names.jam <- randomNames(nrow(jam.mat))
colnames(jam.mat) <- row.names(jam.mat) <- names.jam

Jam.mat <- PredationMatrixToLinks(jam.mat)
Jam.nodes <- data.frame(node = names.jam)
Jam.prop <- list(title = 'jamaica')

jamaica <- Community(nodes = Jam.nodes, trophic.links = Jam.mat, Jam.prop)

rm(guildkey,Jam,Jam.mat,jam.mat,Jam.nodes,Matrix,a,i,j,n.guild,names.jam,Jam.prop)

Arab_webs <- LoadCollection("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/Arabian_webs_final")

Lafferty <- LoadCollection("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs_cheddar")

QuickPond <- LoadCollection("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/QuickPond_cheddar")

communities <- list(Barents,Benguela,ChesapeakeBay,jamaica,Weddell,YthanEstuary,
                    Arab_webs[[1]],Arab_webs[[2]],Arab_webs[[3]],
                    Lafferty[[1]],Lafferty[[2]],Lafferty[[3]],
                    Lafferty[[4]],Lafferty[[5]],Lafferty[[6]],
                    QuickPond[[1]],QuickPond[[2]])

rm(Arab_webs,Barents,Benguela,ChesapeakeBay,jamaica,Weddell,YthanEstuary,Lafferty,QuickPond)

print("Loaded communities")

