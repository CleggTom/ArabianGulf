### Analysing webs together


#Collecting webs

library(cheddar)
library(plyr)
library(dplyr)
library(readr)
library(randomNames)
library(ggplot2)
library(reshape2)

setwd("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf")

## Weddell Sea
Weddell <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Weddell_Sea/Weddell.csv")

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
    n.guild <- max(guildkey$`Guild Number`)
    
    Matrix <- matrix(0,nrow = n.guild,ncol = n.guild)
    
    jam.mat <- Matrix
    for(i in 1:n.guild){
      a <- strsplit(subset(Jam$Prey,Jam$`Guild Number` == i),split = ',')[[1]]
      if(a != '.'){
      a <- as.numeric(gsub(x = a,' ',''))
      for(j in a){
        jam.mat[j,i] <- 1
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
  
  
  
  communities <- list(Barents,Benguela,ChesapeakeBay,jamaica,Weddell,YthanEstuary,
                      Arab_webs[[1]],Arab_webs[[2]],Arab_webs[[3]])
  
  ##  S-L relationships
  web.stats <- data.frame(nodes = unlist(lapply(communities,NumberOfNodes)),
                links = unlist(lapply(communities,NumberOfTrophicLinks)),
                names = c("Barents","Benguela","Chesapeake Bay","Jamaica","Weddell","YthanEstuary",
                          "Arab Adult", "Arab Lifestage", "Arab Taxanomic"))
  
      #get null model link data
      arab_LS <- read_csv("~/Documents/Julia/Life_history_paper/arab_LS.csv")
      colnames(arab_LS) <- c("nodes","links")
      arab_LS$names <- 'Null model'
      
      #all together
      ggplot(web.stats,aes(x=log(nodes),y=log(links)))+
        geom_point()+
        geom_point(data=arab_LS,colour='red',alpha=0.01)
      
      #arab data
      arab.data <- data_frame(nodes = unlist(lapply(Arab_webs,NumberOfNodes)),
                              links = unlist(lapply(Arab_webs,NumberOfTrophicLinks)),
                              names = c("Arab Adult", "Arab Lifestage", "Arab Taxanomic"))
    
      #all together
      ggplot(arab.data,aes(x=log(nodes),y=log(links),colour=names))+
        geom_point()+
        geom_point(data=arab_LS,alpha=0.3)
        
      
      

##Density Distributions
    #find max links
    dataframe <- data.frame()
  for(i in 1:length(communities)){
   data   <-  data.frame(DD = cumsum(DegreeDistribution(communities[[i]])))
   data$n <- 1:nrow(data)
   data$propn <-  seq(0,1,length.out = nrow(data))
   data$web <- communities[[i]]$properties$title
    if(length(grep('Arab',communities[[i]]$properties$title))==1){
      data$arab <- "yes"
    } else {
      data$arab <- "no"
    }
   data$size <- NumberOfNodes(communities[[i]])
   dataframe <- rbind(dataframe,data)
  }
  
    
  dataframe_arab  <- dataframe[dataframe$arab == 'yes',]
    
  ggplot(dataframe,aes(y=DD,x=propn,colour=web,group=web))+
    geom_line()

# get nullmodel data
  Arab_distribution <- read_delim("~/Documents/Julia/Life_history_paper/Arab_distribution.txt",
                                  "\t", escape_double = FALSE, col_names = FALSE, 
                                   trim_ws = TRUE)
  
  Arab_distribution$x <- seq(0,1,length.out = nrow(Arab_distribution))

  Arab_distribution <- melt(Arab_distribution,id.vars = 'x')    

  ggplot(Arab_distribution,aes(x=x,y=value,group=variable))+
    geom_line(alpha = 0.1)+
    geom_line(data = dataframe_arab, aes(x= propn, y = DD, colour = web, group = NULL))
  

  