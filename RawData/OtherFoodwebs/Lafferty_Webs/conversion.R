####### Converting Lafferty's WEbs
library(cheddar)

BSQweb_Links <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs/CSV/BSQweb_Links.csv")
BSQweb_Nodes <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs/CSV/BSQweb_Nodes.csv")

CSMweb_Links <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs/CSV/CSMweb_Links.csv")
CSMweb_Nodes <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs/CSV/CSMweb_Nodes.csv")

EPBweb_Links <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs/CSV/EPBweb_Links.csv")
EPBweb_Nodes <- read_csv("~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs/CSV/EPBweb_Nodes.csv")


Lafferty_cheddar <- function(Links,Nodes){
  Trophic_links <- data.frame(consumer = Links$ConsumerNodeID,
                              resource = Links$ResourceNodeID)
  
  Nodes_frame   <- data.frame(node = paste('Species',Nodes$NodeID),
                              taxa.ID = paste('Species',Nodes$SpeciesID),
                              nodeID  = Nodes$NodeID,
                              Lifestyle = Nodes$'Lifestyle(species)')
  
  Nodes_frame <-as.data.frame(Nodes_frame %>% group_by(taxa.ID) %>% mutate(n.lifestages = n()))
  Nodes_frame$LS <- as.numeric(Nodes_frame$n.lifestages > 1)
  properties <- list(title = Nodes$System[1])
  
  for(i in 1:nrow(Trophic_links)){
    for(j in 1:2){
      Trophic_links[i,j] <- paste('Species',Trophic_links[i,j])
    }
  }                            

 return(Community(nodes = Nodes_frame,
            trophic.links = Trophic_links,
            properties = properties))
  
  }

BSQ_Lifestage <- Lafferty_cheddar(BSQweb_Links,BSQweb_Nodes)
BSQ_Taxanomic <- LumpNodes(BSQ_Lifestage,BSQ_Lifestage$nodes$taxa.ID,weight.by = NULL,title = 'BSQ taxa')
CSM_Lifestage<- Lafferty_cheddar(CSMweb_Links,CSMweb_Nodes)
CSM_Taxanomic <- LumpNodes(CSM_Lifestage,CSM_Lifestage$nodes$taxa.ID,weight.by = NULL,title = 'CSM Taxanomic')
EPB_Lifestage <- Lafferty_cheddar(EPBweb_Links,EPBweb_Nodes)
EPB_Taxanomic <- LumpNodes(EPB_Lifestage,EPB_Lifestage$nodes$taxa.ID,weight.by = NULL,title = 'EPB Taxanomic')


mat <- PredationMatrix(BSQ_Taxanomic)
write.csv(x = unname(mat),"/Users/Tom/Documents/Julia/Life_history_paper/BSQ_mat.csv")

#removing parasites
#BSQ <- RemoveNodes(BSQ,remove = which(BSQ$nodes$Lifestyle == "infectious"),method = 'cascade',title = 'BSQ')
#BSQ.Lifestage <- LumpNodes(BSQ,BSQ$nodes$taxa.ID,weight.by = NULL,title = 'BSQ Lifestage')
#CSM <- RemoveNodes(CSM,remove = which(CSM$nodes$Lifestyle == "infectious"),method = 'cascade',title = 'CSM')
#CSM.Lifestage <- LumpNodes(CSM,CSM$nodes$taxa.ID,weight.by = NULL,title = 'CSM Lifestage')
#EPB <- RemoveNodes(EPB,remove = which(EPB$nodes$Lifestyle == "infectious"),method = 'cascade',title = 'EPB')
#EPB.Lifestage <- LumpNodes(EPB,EPB$nodes$taxa.ID,weight.by = NULL,title = 'EPB Lifestage')

#PlotWebByLevel(BSQ,colour.by = 'LS')
#PlotWebByLevel(BSQ,colour.by = 'Lifestyle')


a <- BSQ_Lifestage$nodes %>% group_by(taxa.ID) %>% summarise(n = n())
hist(a$n)
fitdistr(a$n,'exponential')

Cali.Est <- CommunityCollection(list(BSQ_Lifestage,BSQ_Taxanomic,
                                     CSM_Lifestage,CSM_Taxanomic,
                                     EPB_Lifestage,EPB_Taxanomic))




SaveCollection(Cali.Est,"~/Documents/Rwork/4th year/LifeHistoryPaper/foodwebs/Lafferty_Webs_cheddar")
