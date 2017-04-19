library(cheddar)
library(dplyr)


Arab.FW <- LoadCommunity("~/Documents/Rwork/4th year/LifeHistoryPaper/null_model/foodweb_final")

#getting colump to lump sexstages
nodes.arab <- Arab.FW$nodes
newresolved <- paste(nodes.arab$resolved,nodes.arab$Stage,nodes.arab$Sub.Stage,nodes.arab$Stage.Number)

#no sex community
nosex <- LumpNodes(Arab.FW,newresolved,weight.by = NULL)

newnodes <- nosex$nodes
newlinks <- nosex$trophic.links
newprops <- nosex$properties

#links with massess
nosexlinks <- TLPS(nosex,node.properties = "M")

#top nodes to sort
top.nodes <- newnodes[newnodes$node %in% TopLevelNodes(nosex),]
top.nodes <- top.nodes[top.nodes$Stage != "Adult",]

nosexlinks[nosexlinks$resource.M > 900,]

#getting Sillago sihama Juvenile 1 links - M 15
subset(newnodes,newnodes$resolved == 'Sillago sihama')

sill.links<-TrophicLinksForNodes(nosex,grep('Sillago sihama',newnodes$node))
sill.links[sill.links$resource == "Sillago sihama Juvenile  2",]
sill.links[sill.links$resource == "Sillago sihama Juvenile  3",]
    #stages 2 and 3 have the same predators, fair to assume 1 does too
    sill.to.add <- sill.links[sill.links$resource == "Sillago sihama Juvenile  3",]
    sill.to.add$resource <- "Sillago sihama Juvenile  1"

newlinks <- rbind(newlinks,sill.to.add)

#getting cynoglossus larvae 8 links
subset(newnodes,newnodes$resolved == 'Cynoglossus')
sill.links<-TrophicLinksForNodes(nosex,grep('Cynoglossus',newnodes$node))

a <- sill.links[sill.links$resource == "Cynoglossus Larvae  7",]$consumer
b <- sill.links[sill.links$resource == "Cynoglossus Larvae  9",]$consumer
 
identical(a,b)

  #again larvae 7 and 9 have same predsfair to assume 8 does too
cyno.to.add <- sill.links[sill.links$resource == "Cynoglossus Larvae  7",]
cyno.to.add$resource <- "Cynoglossus Larvae  8"

newlinks <- rbind(newlinks,cyno.to.add)


#basal nodes to be sorted
basal.nodes <- newnodes[newnodes$node %in% BasalNodes(nosex),]
basal.nodes <- basal.nodes[basal.nodes$Stage == "Adult",]
basal.nodes <- basal.nodes[basal.nodes$Category == "Invert",]

# 1) Calanopia minor - Done
 #other calanopia?
  subset(newnodes,newnodes$Genus == 'Calanopia')$node
 #get trophic links for these
  calanopia.links <-  TrophicLinksForNodes(nosex,subset(newnodes,newnodes$Genus == 'Calanopia')$node)
  
  a <- subset(calanopia.links,calanopia.links$resource == "Calanopia minor Adult")$consumer
  b <- subset(calanopia.links,calanopia.links$resource == "Calanopia elliptica Adult")$consumer
  
setdiff(a,b) # very small difference fair to add prey from Calanopia elliptica

call.links <- subset(calanopia.links,calanopia.links$consumer == "Calanopia elliptica Adult")
call.links$consumer <- "Calanopia minor Adult"

newlinks <- rbind(newlinks,call.links)

#2) Clytemnestra scutellata - Remove
subset(newnodes,newnodes$Family == 'Peltidiidae')$node #has no close relatives

  #looking at Order level with similar mass
  cope <- subset(newnodes,newnodes$Order == 'Harpacticoida') 
  cope$M # Clytemnestra scutellata has ~0.9 1 other with ~0.9 Microsetella Adult                                  Ectinosomatidae                       Microsetella         

  #similarities in predators
cope<-  TrophicLinksForNodes(nosex,c("Microsetella Adult","Clytemnestra scutellata Adult"))

a <- subset(cope,cope$resource == "Microsetella Adult")$consumer
b <- subset(cope,cope$resource == "Clytemnestra scutellata Adult")$consumer

length(intersect(a,b))/length(unique(c(a,b)))
#will remove node, not stong enough evidence 

#3) Penaeus semisulcatus Adult - Need to manualy fix
Penaeus <- newnodes[grep("Penaeus semisulcatus",newnodes$node),]
Penaeus <- TrophicLinksForNodes(nosex,grep("Penaeus semisulcatus Adult",newnodes$node))

a<-subset(Penaeus,Penaeus$resource == "Penaeus semisulcatus Adult")$consumer
b<-subset(Penaeus,Penaeus$resource == "Penaeus semisulcatus Adult  1")$consumer
c<-subset(Penaeus,Penaeus$resource == "Penaeus semisulcatus Adult  2")$consumer



# 4) Erugosquilla woodmasoni  - Remove
Erugosquilla <- newnodes[grep("Erugosquilla woodmasoni",newnodes$node),]

# 5) Gellius fibulatus - Remove
Gellius <- newnodes[grep("Gellius",newnodes$node),]

# 6) Halichondria glabrata - Remove
Halichondria <- newnodes[grep("Halichondria",newnodes$node),]

# 7) Haliclona Adult - Remove
Haliclona <- newnodes[grep("Haliclona",newnodes$node),]

# 8) Lingula - Remove
Lingula <- newnodes[grep("Lingula",newnodes$node),]

# 9) Ostracoda - Remove
Ostracoda <- newnodes[grep("Ostracoda",newnodes$node),]

# 10) Tethya aurantium - Remove
Tethya <- newnodes[grep("Tethya",newnodes$node),]

# 11) Pione vastifica - Remove
Pione <- newnodes[grep("Pione",newnodes$node),]

# 12) Terpios - Remove
Terpios <- newnodes[grep("Terpios",newnodes$node),]

# 13) Tedania anhelans - Remove
Tedania <- newnodes[grep("Tedania",newnodes$node),]

# 14) Pseudonitzschia - Remove
Pseudonitzschia <- newnodes[grep("Pseudonitzschia",newnodes$node),]

# 15) Acetes japonicus 
Acetes <- newnodes[grep("Acetes",newnodes$node),]

#remove the Squilla ect. 


Arab.after <- Community(nodes = newnodes,trophic.links = newlinks,properties = newprops)

#removing the nodes above
to.remove <- basal.nodes$node[2:length(basal.nodes$node)]

Arab.after <- RemoveNodes(Arab.after,remove = to.remove,title = "Arabian Gulf")

newnodes <- Arab.after$nodes
newlinks <- Arab.after$trophic.links
newprops <- Arab.after$properties

## Looking at other nodes
others <- newnodes[grep('other',newnodes$node),]
#load original web
Arab.FW <- LoadCommunity("~/Documents/Rwork/4th year/Arabian Gulf data/ArabianGulfFW/ArabianGulfCopies")

others.origin <- subset(Arab.FW$nodes, Arab.FW$nodes$Code.no. %in% others$Code.no.)

#clump detrital - Remove Cillates, ANF + HNF, Fillamentous Cyanobacteria
newnodes$resolved[grep('other',newnodes$node)] <- 'Detritus'


#NA nodes - Remove
n_a <- newnodes[grep('NA',newnodes$node),]

na_origin <- subset(Arab.FW$nodes, Arab.FW$nodes$Code.no. %in% n_a$Code.no.)

#removing
Arab.after <- Community(nodes = newnodes, trophic.links = newlinks , properties = newprops)

to.remove <- c(n_a$node, others$node[2:6])

Arab.after <- RemoveNodes(Arab.after,to.remove,title = 'Arabian Gulf')

#grouping detrital nodes
to.lump <- Arab.after$nodes$resolved
for(i in 1:length(to.lump)){
  if(to.lump[i] != "Detritus"){
    to.lump[i]<-Arab.after$nodes$node[i]
    
  }
}

Arab.after<-LumpNodes(Arab.after,to.lump,weight.by = NULL)

NumberOfNodes(Arab.after)
NumberOfTrophicLinks(Arab.after)


#final cleaning
nodes<-Arab.after$nodes
links<-Arab.after$trophic.links
props<-Arab.after$properties

unique(nodes$Stage)
nodes$Stage[nodes$Stage == "Adult,Egg"] <- "Detritus"
nodes$id[nodes$Stage == "Adult,Egg"] <- "Not_in_worms"

nodes$Sex <- NULL
nodes$sp.no <- NULL
nodes$Code.no. <- 1:nrow(nodes)
nodes$M <- NULL

Arab.after <- Community(nodes = nodes, trophic.links = links, properties = props)

SaveCommunity(Arab.after,"~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/ArabWebFinal")
