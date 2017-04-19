#Exporing Arabian Gulf Data

#Libraries
library(cheddar)
library(ggplot2)
library(dplyr)

#Set Wd
setwd("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw")

#Load data
ArabianGulf<-LoadCommunity("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFw/ArabianGulfCopies")

#Dissassemble
links<-ArabianGulf$trophic.links
nodes<-ArabianGulf$nodes
prop<-ArabianGulf$properties

#Looking at nodes
View(nodes)
colnames(nodes)

    sum(nodes$Species=='')
  a<-unique(nodes$Family)
  unique(a)

a<-subset(nodes,nodes$Family=='Bivalvia')

grep('Biva',nodes$node)

a<-unique(nodes$Category)

#

#problems
#basal?
basal.arab<-BasalNodes(ArabianGulf)
basal.arab<-data.frame(basal=BasalNodes(ArabianGulf))
basal.arab<-nodes[sum(nodes$node==basal.arab),]

basal.arab<- subset(nodes,nodes$node %in% basal.arab)

write.csv(basal.arab,file='basal.nodes.csv')

#eg Squilla (mantis shrimp genus)
a<-subset(nodes,nodes$Genus=='Squilla')
squilla.nodes<-a$node

a<-TrophicLinksForNodes(ArabianGulf,squilla.nodes) #are all resources only!

#Top
top.arab<-as.data.frame(TopLevelNodes(ArabianGulf))

#looking at Sillago sihama
a<-subset(nodes,nodes$Genus=='Sillago')
a<-a$node
sillago.links<-TrophicLinksForNodes(ArabianGulf,a)
sillago.links$c.r<-NA

index<-grep('Sillago',sillago.links$resource) #index of sillago as resource
for(i in 1:nrow(sillago.links)){
  if(sum(i==index) == 0){
   sillago.links$c.r[i]<-'C'
  } else {
    sillago.links$c.r[i]<-'R'  
    c<-sillago.links$consumer[i]
    sillago.links$consumer[i]<-sillago.links$resource[i]
    sillago.links$resource[i]<-c
  }
} #fliping all to single column

sillago.links<-sillago.links %>% 
  group_by(consumer,c.r) %>%
  summarise(n=n())

ggplot(sillago.links,aes(y=n,x=consumer,fill=c.r))+
  geom_bar(stat='identity',position = 'dodge')

#lookiing at vunerability/generalisms
#function to find 
nprey<-function(Community,Nodename){  
  #get the links for a given node
  links.node<-TrophicLinksForNodes(Community,Nodename)
  #find number where is pred
  n<-sum(links.node$consumer==Nodename)
  print(n)
} #n prey
npred<-function(Community,Nodename){  
  #get the links for a given node
  links.node<-TrophicLinksForNodes(Community,Nodename)
  #find number where is pred
  n<-sum(links.node$resource==Nodename)
  print(n)
} #n prey

#finding the number of in and out links per node
nodes.in.out<-nodes

nodes.in.out$prey<-NA
nodes.in.out$pred<-NA

for(i in 1:nrow(nodes)){
  nodes.in.out$prey[i]<-nprey(ArabianGulf,nodes.in.out$node[i])
  nodes.in.out$pred[i]<-npred(ArabianGulf,nodes.in.out$node[i])  
}

nodes.in.out$ratio<-nodes.in.out$pred/nodes.in.out$prey

ggplot(nodes.in.out,aes(x=prey,y=pred))+
  geom_point()+
  geom_abline()

#Leiognathus bindus A1  has 600 prey!!
a<- nodes.in.out[ nodes.in.out$node %in% links$resource[links$consumer=='Leiognathus bindus A1'],]
ggplot(a,aes(x=log(M)))+
  geom_dotplot(binwidth=.1)
#relative numbers
nodes.in.out$mean.prey<-mean(nodes.in.out$prey)
nodes.in.out$mean.pred<-mean(nodes.in.out$pred)

nodes.in.out$prey.rel<-nodes.in.out$prey/nodes.in.out$mean.prey
nodes.in.out$pred.rel<-nodes.in.out$pred/nodes.in.out$mean.pred

ggplot(nodes.in.out,aes(x=prey.rel,y=pred.rel))+
  geom_point()

qplot(x=log(pred),y=log(prey),data = nodes.in.out)

#ratio
nodes.in.out$ratio<-nodes.in.out$pred/nodes.in.out$prey

#trophic levels 
Arab.TL<-PreyAveragedTrophicLevel(ArabianGulf)
nodes$TL<-Arab.TL

links$prey.tl<-NA
links$pred.tl<-NA
for(i in 1:nrow(links)){
  links$prey.tl[i]<-nodes$TL[nodes$node==links$resource[i]]
  links$pred.tl[i]<-nodes$TL[nodes$node==links$consumer[i]]
  print(i)
  }

ggplot(links,aes(x=pred.tl,y=prey.tl))+
  geom_point()

