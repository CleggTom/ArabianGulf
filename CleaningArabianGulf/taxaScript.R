#########################
#taxanomic identity code#
#########################

#libraries
library(cheddar)
library(taxize)
library(plyr)
library(stringr)
library(dplyr)

#Set Wd
setwd("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw")

#Load data
ArabianGulf<-LoadCommunity("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/ArabianGulfCopies")

# get nodes file
arab.nodes<-ArabianGulf$nodes

# create i.d column
 #get resolved to

  for(i in 1:nrow(arab.nodes)){
    if(arab.nodes$Species[i] != ""){
      arab.nodes$resolved.to[i]<-"Species"
    } else {
    if(arab.nodes$Genus[i] != ""){
      arab.nodes$resolved.to[i]<-"Genus"
    } else {
        if(arab.nodes$Family[i] != ""){
        arab.nodes$resolved.to[i]<-"Family"
    }
    }
  }}

#trim whitespace 
arab.nodes$Family<-str_trim(arab.nodes$Family,side = 'both')
  arab.nodes$Genus<-str_trim(arab.nodes$Genus,side = 'both')
    arab.nodes$Species<-str_trim(arab.nodes$Species,side = 'both')

#get species as latin binomial
arab.nodes$Species<-paste(arab.nodes$Genus,arab.nodes$Species)
#get rid of double spaces
arab.nodes$Species<-sub("  ",replace=" ",x = arab.nodes$Species)

  #create column based in this
arab.nodes$resolved.to<-as.factor(arab.nodes$resolved.to)
for(i in 1:nrow(arab.nodes)){
  rank.to.resolve<-as.numeric(arab.nodes$resolved.to[i]) +3
  arab.nodes$taxa.to.id[i]<-arab.nodes[i,rank.to.resolve]
}
rm(rank.to.resolve)
rm(i)

#put into taxise
  #get worms ids
for(i in 1:nrow(arab.nodes)){
  arab.nodes$id[i] <- get_wormsid(arab.nodes$taxa.to.id[i])[[1]]
  }


  #re-add the worms names
for(i in 1:nrow(arab.nodes)){
  print(i)
  a <- classification(arab.nodes$id[i],db='worms')[[1]]
  if(!is.na(a)){
    arab.nodes$wormsName[i] <- as.character(a[nrow(a),1])
  } else {
    arab.nodes$wormsName[i] <- NA
  }
}

#export to check

#import the checked file
wormid_checked <- read_csv("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/wormid_checked.csv")

#add this back into the nodes file
arab.nodes<-cbind(arab.nodes,wormid_checked)

#export and check the other categories
write_csv(arab.nodes,'nodes_with_id.csv')

#manual completed all the categories
arab.nodes <- read_csv("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/nodes_with_id_checked.csv")

#remove wormsnames
arab.nodes$wormsNames <- NA

#cheking integrity of the ids 616 is weird

arab.nodes$id[616] <- '278777'



#getting the node names from the ids

for(i in 1:nrow(arab.nodes)){
  a <- classification(arab.nodes$id[i],db = 'worms')[[1]]
  
  if(!is.na(a)){
  a<-as.character(a[nrow(a),1])
  arab.nodes$wormsName[i] <- a
  } else {
    arab.nodes$wormsNames[i] <- NA
  }
  print(i)
}  


#creating new node names
arab.nodes$new.node <- NA

for(i in 1:nrow(arab.nodes)){
  newnode <- as.character(arab.nodes[i,c(12,6,7,8,9)])
  arab.nodes$new.node[i] <- paste(newnode[!is.na(newnode)],collapse = ' ')
}

  #adding the sp. number to duplicate names
write_csv(arab.nodes,'nodes_duplicates.csv')

#which need it?
a <- arab.nodes %>% group_by(new.node) %>% summarise(n = n())

a[which(a$n > 1),] #11 names and
sum(a[which(a$n > 1),]$a)

#checking
a <- read_csv("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/nodes_duplicates_done.csv")

a <- a %>% group_by(new.node) %>% summarise(n = n())

a[which(a$n > 1),] #all good !!
sum(a[which(a$n > 1),]$a)


arab.nodes <- read.csv("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/nodes_duplicates_done.csv")

arab.nodes<-arab.nodes[,1:14]
arab.nodes <- arab.nodes[,-13]  

#nodes done!!
write.csv(arab.nodes,'nodes_final.csv')

#integrate into the links
arab.links<-ArabianGulf$trophic.links

#new resources
  arab.links$new.resource <- NA
  arab.links$new.consumer <- NA
  
for(i in 1:nrow(arab.links)){
  #getting new tings
  new.resource <- as.character(arab.nodes$new.node[which(arab.nodes$node == arab.links$resource[i])])
  new.consumer <- as.character(arab.nodes$new.node[which(arab.nodes$node == arab.links$consumer[i])])
  
  #add to the links frame
  arab.links$new.resource[i] <- new.resource
  arab.links$new.consumer[i] <- new.consumer
  print(i)
}
  
  arab.links<-arab.links[,5:6]
  colnames(arab.links) <- c('resource','consumer')
  write.csv(arab.links,'links_final.csv')
  
  arab.links<-unique(arab.links)
  
  arab.prop<-ArabianGulf$properties

  arab.nodes<-arab.nodes[,-1]
  colnames(arab.nodes)[1] <- 'node'
  
  ArabianGulf.new<-Community(nodes = arab.nodes,trophic.links = arab.links, properties = arab.prop)  

  par(mfrow = c(1,2))
  PlotMCvMR(ArabianGulf.new) 
  PlotMCvMR(ArabianGulf) 
  
  par(mfrow = c(1,2))
  PlotNPS(ArabianGulf.new,'M','PreyAveragedTrophicLevel',show.web = F) 
  PlotNPS(ArabianGulf,'M','PreyAveragedTrophicLevel',show.web = F) 
  
  colour.spec <- c(Vert.Ecto = 'blue', Invert = 'black',
                   Auto = 'green', Bact = 'purple', 'red')
    
  par(mfrow = c(1,1))
  PlotWebByLevel(ArabianGulf.new, colour.by = 'Category' , colour.spec = colour.spec) 
  legend("topright", legend=names(colour.spec), col=colour.spec)

  
  