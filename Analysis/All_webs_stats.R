#analysisng the webs

source("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/Structure_stats/All_webs_function.R")
library(ggraph)


NN1<-unlist(lapply(X = communities, FUN = NumberOfNodes))
NTL1<-unlist(lapply(X = communities, FUN = NumberOfTrophicLinks)) 
LD1<-unlist(lapply(X = communities, FUN = LinkageDensity))
DC1<-unlist(lapply(X = communities, FUN = DirectedConnectance))
FB1<-unlist(lapply(X = communities, FUN = FractionBasalNodes))
FI1<-unlist(lapply(X = communities, FUN = FractionIntermediateNodes))
FT1<-unlist(lapply(X = communities, FUN = FractionTopLevelNodes))
TG1<-unlist(lapply(X = communities, FUN = function(x) mean(TrophicGenerality(x))))
TG1.SD<-unlist(lapply(X = communities, FUN = function(x) sd(NormalisedTrophicGenerality(x))))
TV1<-unlist(lapply(X = communities, FUN = function(x) mean(TrophicVulnerability(x))))
TV1.SD<-unlist(lapply(X = communities, FUN = function(x) sd(NormalisedTrophicVulnerability(x))))
Title <- unlist(lapply(X = communities, FUN = function(x) return(x$properties$title)))
Arabian <- c(rep('N',6),rep('Y',3))

sum.dat <- data.frame(nodes = NN1 , links = NTL1 , Linkage_Density = LD1, Connectance = DC1,
                      Fraction_basal = FB1 ,Fraction_intermediate = FI1 , Fraction_Top = FT1,
                      Trophic_Generality = TG1 , Trophic_Generality_SD = TG1.SD,
                      Trophic_Vunerability = TV1 , Trophic_Vunerability_SD = TV1.SD,
                      Title = Title , Arabian=Arabian) 

degree_list_coms <- list()
for(i in 1:9){
  degree_list_coms[[i]] <-DegreeDistribution(communities[[i]])
  degree_list_coms[[i]] <- data.frame(degree = cumsum(degree_list_coms[[i]]),
                                 K = (1:length(degree_list_coms[[i]]))/LinkageDensity(communities[[i]]),
                                 N = communities[[i]]$properties$title)

}

df1 <- bind_rows(degree_list_coms)

ggplot(df,aes(x=K,y=degree,colour=N))+
  geom_line()
