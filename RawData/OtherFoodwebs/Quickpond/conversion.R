###Quick conversion

Quick_Pond_Nodes <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Quickpond/Data\\Quick_Pond_Nodes.csv")
Quick_Pond_Links <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Quickpond/Data\\Quick_Pond_Links.csv")


Quick <- function(Links,Nodes){
  Trophic_links <- data.frame(consumer = Links$ConsumerNodeID,
                              resource = Links$ResourceNodeID)
  
  Nodes_frame   <- data.frame(node = paste('Species',Nodes$NodeID),
                              taxa.ID = paste('Species',Nodes$SpeciesID),
                              nodeID  = Nodes$NodeID,
                              Lifestyle = Nodes$'Lifestyle(species)',
                              name = Nodes$WorkingName)
  
  Nodes_frame <-as.data.frame(Nodes_frame %>% group_by(taxa.ID) %>% mutate(n.lifestages = n()))
  Nodes_frame$LS <- as.numeric(Nodes_frame$n.lifestages > 1)
  properties <- list(title = 'Quickpond Lifestage')
  
  for(i in 1:nrow(Trophic_links)){
    for(j in 1:2){
      Trophic_links[i,j] <- paste('Species',Trophic_links[i,j])
    }
  }                            
  
  return(Community(nodes = Nodes_frame,
                   trophic.links = Trophic_links,
                   properties = properties))
  
}


Quickpond <- Quick(Quick_Pond_Links,Quick_Pond_Nodes)
Quickpond.Taxa <- LumpNodes(Quickpond,Quickpond$nodes$taxa.ID,weight.by = NULL)

a <- CommunityCollection(list(Quickpond,Quickpond.Taxa))
SaveCollection(a,"~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/QuickPond_cheddar")
