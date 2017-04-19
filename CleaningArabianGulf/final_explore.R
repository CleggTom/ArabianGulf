#########################
# exploring the new web #
#########################

library(cheddar)
library(plyr)
library(dplyr)
library(taxize)
library(ggplot2)
library(stringr)

#load orginal web
ArabianGulf<-LoadCommunity("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/ArabianGulfCopies")

#importing the files
links_final <- read.csv("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/links_final.csv" , stringsAsFactors = F)
nodes_final <- read.csv("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/nodes_final.csv" , stringsAsFactors = F)
prop_final  <- ArabianGulf$properties

#messing with nodes

  #do the category col
for(i in 1:nrow(nodes_final)){
  if(is.na(nodes_final$Kingdom[i])){
    next
  }
  if(nodes_final$Kingdom[i] == 'Animalia'){
    
    if(nodes_final$Phylum[i] == 'Chordata'){
      nodes_final$Category[i] <- 'Vert.Ecto'
    } else {
      nodes_final$Category[i] <- 'Invert'
    }
    
  } else {
    if(nodes_final$Kingdom[i] == 'Plantae' || nodes_final$Kingdom[i] == 'Chromista' ) {
    nodes_final$Category[i] <- 'Auto'
    } 
  }
}

#get resolved to column
nodes_final$resolved.to <- NA
nodes_final <- nodes_final[,c(1:12,ncol(nodes_final),13:(ncol(nodes_final)-1))]  
for(i in 1:nrow(nodes_final)){
  for(j in ncol(nodes_final):14){
    if(!is.na(nodes_final[i,j])){
      nodes_final$resolved.to[i] <- colnames(nodes_final)[j]
      break
    }
  }
  print(i)
}

#add species to the Sp. nodes
Sp.names <- nodes_final$Family[grep(' Sp.',nodes_final$node)]
Sp.num <- sapply(strsplit(nodes_final$node[grep(' Sp.',nodes_final$node)], ' '), '[', 3) 

nodes_final$Species[grep(' Sp.',nodes_final$node)] <- paste(Sp.names,Sp.num)

#get resolved to column again
nodes_final$resolved.to <- NA
nodes_final <- nodes_final[,c(1:12,ncol(nodes_final),13:(ncol(nodes_final)-1))]  
for(i in 1:nrow(nodes_final)){
  for(j in ncol(nodes_final):14){
    if(!is.na(nodes_final[i,j])){
      nodes_final$resolved.to[i] <- colnames(nodes_final)[j]
      break
    }
  }
  print(i)
}

  
  #get the lifestage lumper column

nodes_final$resolved <- NA
nodes_final$sp.no <- NA

j <- 0
for(i in 1:nrow(nodes_final)){
  a <- nodes_final[i,which(colnames(nodes_final) ==   nodes_final$resolved.to[i])]
  if(length(a)>0){
  nodes_final$resolved[i] <-a
  } else {
    nodes_final$resolved[i] <- paste('other',j)
    j <- j+1
  }
}

nodes_final$resolved <- as.factor(nodes_final$resolved)
nodes_final$sp.no <- as.numeric(nodes_final$resolved)

rm(a,i,j,Sp.names,Sp.num)

#messing with links
  
links_original <- ArabianGulf$trophic.links
links_final <- links_final[,-1]

links_final <- unique(links_final)

  #create community
ArabianGulf.new <- Community(nodes = nodes_final, properties =  prop_final, trophic.links = links_final)

SaveCommunity(ArabianGulf.new,
              "~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/foodweb_final")

#plot the kingdoms and categories

par(mfrow = c(1,2))

#plot by kingdom
colour.spec <- c(Animalia = 'blue', Chromista = 'black', Bacteria = 'yellow',
                 Plantae = 'green', Protozoa = 'purple', 'red')
PlotWebByLevel(ArabianGulf.new, colour.by = 'Kingdom' , colour.spec = colour.spec) 
legend("topright", legend=names(colour.spec), pch=19, col=colour.spec,cex = 0.3)

#plot by category
colour.spec <- c(Auto = 'green', Bact = 'purple', Invert = 'black',
                 Vert.Ecto = 'blue', Basal = 'red' , 'yellow')
PlotWebByLevel(ArabianGulf.new, colour.by = 'Category' , colour.spec = colour.spec) 
legend("topright", legend=names(colour.spec), pch=19, col=colour.spec,cex = 0.3)

#plot by resolved to. 
colour.spec <- rainbow(13)
names(colour.spec)<-unique(nodes_final$resolved.to)

PlotWebByLevel(Arab, colour.by = 'resolved.to' , colour.spec = colour.spec) 
legend("topright", legend=names(colour.spec), pch=19, col=colour.spec,cex = 0.3)


#plotting resolved to vs TL

nodes_final$TL <- PreyAveragedTrophicLevel(Arab)
unique(nodes_final$resolved.to)

resolved <- ordered(nodes_final$resolved.to,
                    levels = c('Variety','Subspecies',"Species",
                               'Genus','Subsection',"Section",
                               "Family",'Superorder', 'Infraclass','Subclass',
                               'Class','Phylum',''
                               ))

nodes_final$resolved.to <- resolved


ggplot(nodes_final,aes(x=resolved.to,y=TL,fill=resolved.to,colour = NULL ))+
  geom_violin()

#ploting mass vs TL
ggplot(nodes_final,aes(x=log10(M),y=TL,colour=Category))+
  geom_point()+
  geom_smooth(method = "lm" , se = F)

    #linear model
model <- lm(data = nodes_final, TL ~ log10(M):Category)

summary(model)

#trophic
PlotMDistribution(ArabianGulf.new)

par(mfrow = c(1,2))
PlotNPSDistribution(Arab,'NormalisedTrophicGenerality',main = 'Generality')
PlotNPSDistribution(Arab,'NormalisedTrophicVulnerability', main = 'vunerability')

par(mfrow = c(1,2))
PlotNPS(Arab, 'M', 'NormalisedTrophicVulnerability' , show.web = F)
PlotNPS(Arab, 'M', 'NormalisedTrophicGenerality' , show.web = F)

#lumping?
lump<-ArabianGulf.new$nodes$resolved

a<-LumpNodes(ArabianGulf.new , lump = lump, weight.by = NULL)

#how many specie have lifestage data?
length(unique(c((grep(',',a$nodes$Stage)),
  (grep(',',a$nodes$Sub.Stage)),
   (grep(',',a$nodes$Stage.Number)))))

#number of stage dist   
x<-ArabianGulf.new$nodes

x <- x %>% group_by(resolved) %>% summarise(n =n())

ggplot(x,aes(x=n))+
  geom_histogram(binwidth = 1)

#plot webs
PlotWebByLevel(ArabianGulf.new)
PlotWebByLevel(a)

#plot the distribution of links
PlotDegreeDistribution(ArabianGulf.new)
PlotDegreeDistribution(a)
