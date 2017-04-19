##Cheddarising the null model

library(cheddar)
library(randomNames)


##For the null models
community_list <- list()
for(i in 1:1000){
  title = paste('matrix',i,sep = "")
  mat <- read_delim(paste("~/Documents/Julia/Life_history_paper/Lh_mats/",title,".txt",
                          sep=""), 
                        "\t", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
  mat <- as.matrix(mat)
  names <- make.names(rep('species',nrow(mat)),unique = T)
  rownames(mat) <- colnames(mat) <- names
  links <- PredationMatrixToLinks(mat)
  nodes <- data.frame(node=names)
  props <- list(title = paste(title))
  
  com <- Community(nodes = nodes, trophic.links = links, properties = props)
  community_list[[i]] <- com
  print(i)
}
degree_list <- list()
for(i in 1:1000){
  print(i)
  degree_list[[i]] <- DegreeDistribution(community_list[[i]])
  degree_list[[i]] <- data.frame(degree = cumsum(degree_list[[i]]),
                        K = (1:length(degree_list[[i]]))/LinkageDensity(community_list[[i]]),
                        N = i)
}

df <- bind_rows(degree_list)

ggplot(df,aes(y=degree,x=K,group=N))+
  geom_line()

ggplot(rbind(df,df1),aes(y=degree,x=K,group=N))+
  geom_line()

function(){
NN1<-unlist(lapply(X = community_list, FUN = NumberOfNodes))
NTL1<-unlist(lapply(X = community_list, FUN = NumberOfTrophicLinks)) 
LD1<-unlist(lapply(X = community_list, FUN = LinkageDensity))
DC1<-unlist(lapply(X = community_list, FUN = DirectedConnectance))
FB1<-unlist(lapply(X = community_list, FUN = FractionBasalNodes))
FI1<-unlist(lapply(X = community_list, FUN = FractionIntermediateNodes))
FT1<-unlist(lapply(X = community_list, FUN = FractionTopLevelNodes))
TG1<-unlist(lapply(X = community_list, FUN = function(x) mean(TrophicGenerality(x))))
TG1.SD<-unlist(lapply(X = community_list, FUN = function(x) sd(NormalisedTrophicGenerality(x))))
TV1<-unlist(lapply(X = community_list, FUN = function(x) mean(TrophicVulnerability(x))))
TV1.SD<-unlist(lapply(X = community_list, FUN = function(x) sd(NormalisedTrophicVulnerability(x))))
Title <- unlist(lapply(X = community_list, FUN = function(x) return(x$properties$title)))
}



