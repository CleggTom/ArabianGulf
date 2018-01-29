# !diagnostics off

### Full Analysis

#load packages
library(cheddar)
library(Basingstoke)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(wesanderson)

#colour blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#load data + functions 
source("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/Structure_stats/All_webs_function.R",print.eval = F)

#function that gets stats for a list of communities
get_webstats <- function(com = communities){
  NN1<-unlist(lapply(X = com, FUN = NumberOfNodes))
  NTL1<-unlist(lapply(X = com, FUN = NumberOfTrophicLinks)) 
  LD1<-unlist(lapply(X = com, FUN = LinkageDensity))
  DC1<-unlist(lapply(X = com, FUN = DirectedConnectance))
  FB1<-unlist(lapply(X = com, FUN = FractionBasalNodes))
  FI1<-unlist(lapply(X = com, FUN = FractionIntermediateNodes))
  FT1<-unlist(lapply(X = com, FUN = FractionTopLevelNodes))
  TG1<-unlist(lapply(X = com, FUN = function(x) mean(TrophicGenerality(x))))
  TG1.SD<-unlist(lapply(X = com, FUN = function(x) sd(NormalisedTrophicGenerality(x))))
  TV1<-unlist(lapply(X = com, FUN = function(x) mean(TrophicVulnerability(x))))
  TV1.SD<-unlist(lapply(X = com, FUN = function(x) sd(NormalisedTrophicVulnerability(x))))
  print('ts')
  Title <- unlist(lapply(X = com, FUN = function(x) return(x$properties$title)))
  
  sum.dat <- data.frame(nodes = NN1 , links = NTL1 , Linkage_Density = LD1, Connectance = DC1,
                        Fraction_basal = FB1 ,Fraction_intermediate = FI1 , Fraction_Top = FT1,
                        Trophic_Generality = TG1 , Trophic_Generality_SD = TG1.SD,
                        Trophic_Vunerability = TV1 , Trophic_Vunerability_SD = TV1.SD,
                        Title = Title) 
  return(sum.dat)
}

Real_webstats <- get_webstats(communities)

Real_webstats$group <- c(1,2,3,4,5,6,7,7,7,8,8,9,9,10,10,11,11)

#######
# Q1) How does LH influence Diversity-Complexity Relationships
######

#N lifestages
#####
#Arab
sum((table(communities[[8]]$nodes$resolved)) > 1)/
  length((table(communities[[8]]$nodes$resolved))) # 0.09448819
#cali
sum(table(communities[[10]]$nodes$taxa.ID) > 1)/
  length(unique(communities[[10]]$nodes$taxa.ID)) #0.3488372 BSQ
sum(table(communities[[12]]$nodes$taxa.ID) > 1)/
  length(unique(communities[[12]]$nodes$taxa.ID)) #0.3192771 CSM
sum(table(communities[[14]]$nodes$taxa.ID) > 1)/
  length(unique(communities[[14]]$nodes$taxa.ID)) #0.3627907 EPB
#quick
sum(table(communities[[16]]$nodes$taxa.ID) > 1)/
  length(unique(communities[[16]]$nodes$taxa.ID)) #0.5079365

######

Prop.ls <- c(0.09448819,0.3488372,0.3192771,0.3627907,0.5079365) #goes in numerical order

#Plotting all S v L
ggplot(Real_webstats,aes(x=log(nodes),y=log(links),colour=Title,group=group))+
  geom_point()+
  geom_line()

#just the Lifestage webs
ggplot(Real_webstats[8:15,],aes(x=log(nodes),y=log(links),colour=Title,group=group))+
  geom_point()+
  geom_line()+
  geom_abline(slope = 1.5,intercept = -1.0902)+
  geom_abline(slope = 2,intercept = -1.0902)


taxaWebstats <- Real_webstats[c(1:6,8,10,12,14,16),]
taxaWebstats$nodes <- log(taxaWebstats$nodes)
taxaWebstats$links <- log(taxaWebstats$links)

lifeWebstats <- Real_webstats[c(1:6,9,11,13,15,17),]
lifeWebstats$nodes <- log(lifeWebstats$nodes)
lifeWebstats$links <- log(lifeWebstats$links)

#Regression 
SL.taxa.model <- lm((links)~(nodes),taxaWebstats)
SL.life.model <- lm((links)~(nodes),lifeWebstats)

summary(SL.taxa.model)
summary(SL.life.model)

taxa.model.line <- data.frame(nodes = (seq(min(taxaWebstats$nodes),max(taxaWebstats$nodes),length.out = 100)),links = NA)
taxa.model.line$links <- predict(SL.taxa.model,taxa.model.line)

life.model.line <- data.frame(nodes = (seq(min(lifeWebstats$nodes),max(lifeWebstats$nodes),length.out = 100)),links = NA)
life.model.line$links <- predict(SL.life.model,life.model.line)

webstats.data <- Real_webstats  
webstats.data$nodes <- log(webstats.data$nodes)
webstats.data$links <- log(webstats.data$links )

taxa.model.line$Regression <- "Taxanomic"
life.model.line$Regression <- "Lifestage"
SL.line.data <- rbind(taxa.model.line)

#Fig 1) 
webstats.data <- webstats.data[-7,]
webstats.data$colouring <- c(rep("Other",6),rep(c("Arabian Gulf","BSQ","CSM","EPB","Quickpond"),each = 2))
webstats.data$colouring <- factor(webstats.data$colouring,levels = c("Arabian Gulf","BSQ","CSM","EPB","Quickpond","Other"))

cVect <- c("Arabian Gulf" = cbPalette[7],"BSQ" = cbPalette[2],"CSM" = cbPalette[3],
             "EPB" = cbPalette[4],"Quickpond" = cbPalette[5],"Other" = cbPalette[6])


DvC.plot <- ggplot(webstats.data,aes(x = (nodes), y = (links),colour=colouring,group=group))+
  geom_point(size = 3)+
  geom_line()+
  geom_line(data = SL.line.data,aes( group = NULL,colour=NULL))+
  labs(x = "Log (nodes)", y = "Log (links)")+
  theme_classic() + 
  theme(legend.position = c(0.9,0.1),legend.justification = c(1,0),legend.title = element_blank())+
  scale_color_manual(values = cVect)
  



##Is The change along the SL relationship due to changes in overlap?
# at lower overlap there is a smaller diet ( ??) and therefore a 
#smaller increase in Links

#Arab
Nodes <-communities[[8]]$nodes
Nodes <- Nodes[(Nodes %>% group_by(resolved) %>% dplyr::mutate(n=n()))$n > 1,]
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$resolved))){
  a <- subset(Nodes,Nodes$resolved == unique(Nodes$resolved)[i])
  resources <- list()
  N <- nrow(a)
  for(j in 1:N){
    L <- TrophicLinksForNodes(communities[[8]],a$node[j])
    L <- subset(L$resource,L$consumer == a$node[j])
    resources[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(resources) > 1){
      one <-  resources[[comb[1,j]]]
      two <-  resources[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) #0.4478262
sd(overlaps)

#Cali 0.02
######
Nodes <-communities[[10]]$nodes[communities[[10]]$nodes$LS == 1,]
#Remove Basal
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  resources <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[10]],a$node[j])
    L <- subset(L$resource,L$consumer == a$node[j])
    resources[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(resources) > 1){
      one <-  resources[[comb[1,j]]]
      two <-  resources[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.02141534
sd(overlaps)
#Cali
Nodes <-communities[[12]]$nodes[communities[[12]]$nodes$LS == 1,]
#Remove Basal
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  resources <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[12]],a$node[j])
    L <- subset(L$resource,L$consumer == a$node[j])
    resources[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(resources) > 1){
      one <-  resources[[comb[1,j]]]
      two <-  resources[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.05024191

#Cali
Nodes <-communities[[14]]$nodes[communities[[14]]$nodes$LS == 1,]
#Remove Basal
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  resources <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[14]],a$node[j])
    L <- subset(L$resource,L$consumer == a$node[j])
    resources[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(resources) > 1){
      one <-  resources[[comb[1,j]]]
      two <-  resources[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.01937459
sd
#Quick
Nodes <-communities[[16]]$nodes[communities[[16]]$nodes$LS == 1,]
#Remove Basal
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  resources <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[16]],a$node[j])
    L <- subset(L$resource,L$consumer == a$node[j])
    resources[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(resources) > 1){
      one <-  resources[[comb[1,j]]]
      two <-  resources[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.2693295

#What about consumer overlap?

#Arab
Nodes <-communities[[8]]$nodes
Nodes <- Nodes[(Nodes %>% group_by(resolved) %>% dplyr::mutate(n=n()))$n > 1,]
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$resolved))){
  a <- subset(Nodes,Nodes$resolved == unique(Nodes$resolved)[i])
  consumers <- list()
  N <- nrow(a)
  for(j in 1:N){
    L <- TrophicLinksForNodes(communities[[8]],a$node[j])
    L <- subset(L$consumer,L$resource == a$node[j])
    consumers[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(consumers) > 1){
      one <-  consumers[[comb[1,j]]]
      two <-  consumers[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.6109928
sd(overlaps)
#Cali 0.03
######
Nodes <-communities[[10]]$nodes[communities[[10]]$nodes$LS == 1,]
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  consumers <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[10]],a$node[j])
    L <- subset(L$consumer,L$resource == a$node[j])
    consumers[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(consumers) > 1){
      one <-  consumers[[comb[1,j]]]
      two <-  consumers[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.04418759

Nodes <-communities[[12]]$nodes[communities[[12]]$nodes$LS == 1,]
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  consumers <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[12]],a$node[j])
    L <- subset(L$consumer,L$resource == a$node[j])
    consumers[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(consumers) > 1){
      one <-  consumers[[comb[1,j]]]
      two <-  consumers[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.03629438

Nodes <-communities[[14]]$nodes[communities[[14]]$nodes$LS == 1,]
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  consumers <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[14]],a$node[j])
    L <- subset(L$consumer,L$resource == a$node[j])
    consumers[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(consumers) > 1){
      one <-  consumers[[comb[1,j]]]
      two <-  consumers[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.03103297
######
#Quick
Nodes <-communities[[16]]$nodes[communities[[16]]$nodes$LS == 1,]
#for each taxa
overlaps <- vector()
for(i in 1:length(unique(Nodes$taxa.ID))){
  a <- subset(Nodes,Nodes$taxa.ID == unique(Nodes$taxa.ID)[i])
  consumers <- list()
  N <- a$n.lifestages[1]
  for(j in 1:length(a$node)){
    L <- TrophicLinksForNodes(communities[[16]],a$node[j])
    L <- subset(L$consumer,L$resource == a$node[j])
    consumers[[j]] <- L
  }
  comb <- combn(1:N,2)
  result <- vector()
  for(j in 1:ncol(comb)){
    if(length(consumers) > 1){
      one <-  consumers[[comb[1,j]]]
      two <-  consumers[[comb[2,j]]]
      
      if(length(one) < 1 || length(two) < 1){
        result[j] <- 0
      } else {
        result[j] <- length(intersect(one,two)) / length(unique(union(one,two)))
      }} else {result[j] <- NA} 
  }
  overlaps[i] <- mean(result,na.rm = T)
}

mean_se(overlaps) # 0.3897602

#What is the relationship between slope on the log(S)-log(L) and the overlap
overlap.data <- Real_webstats[c(8:17),]
overlap.data$slope <- NA
for(i in unique(overlap.data$group)){
  a <- diff(log(overlap.data[overlap.data$group == i , ]$links))
  b <- diff(log(overlap.data[overlap.data$group == i , ]$nodes))
  overlap.data[overlap.data$group == i , ]$slope = a/b
}

overlap.data$res_overlap <- rep(c(0.4478262,0.02141534,0.05024191,0.01937459,0.2693295),each=2)
overlap.data$con_overlap <- rep(c(0.6109928,0.04418759,0.03629438,0.03103297,0.3897602),each=2)
overlap.data$prop.LS <- rep(Prop.ls,each=2)

ggplot(overlap.data,aes(x=res_overlap,y=slope))+
  geom_point()

ggplot(overlap.data,aes(x=con_overlap,y=slope))+
  geom_point()

##Fig 1 
overlap.data$colouring <- c(rep(c("Arabian Gulf","BSQ","CSM","EPB","Quickpond"),each = 2))
overlap.data$colouring <- factor(overlap.data$colouring,levels = c("Arabian Gulf","BSQ","CSM","EPB","Quickpond"))

overlap.data <- melt(overlap.data,measure.vars = c("res_overlap","con_overlap"))
levels(overlap.data$variable) <- c("Resource","Consumer")

OvS.plot <- ggplot(overlap.data,aes(x = value, y = slope, colour = colouring))+
  geom_point(size = 3)+
  labs(x = "Overlap", y = "ΔL / ΔS")+
  theme_classic() + 
  theme(legend.position =  "none",strip.background = element_blank())+
  facet_wrap(~variable,nrow=2, scales = 'free')+
  xlim(0,0.7)+
  scale_color_manual(values = cVect)

source("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/Structure_stats/Multiplot.R")

multiplot(DvC.plot,OvS.plot,cols = 2)

#Cor tests
#resource
  cor.test(overlap.data$slope[overlap.data$variable == "Resource"],
      overlap.data$value[overlap.data$variable == "Resource"])
#consumer
  cor.test(overlap.data$slope[overlap.data$variable == "Consumer"],
      overlap.data$value[overlap.data$variable == "Consumer"])
########
# Q2) Changes in Webstats vs niche models
########

#get lifestage webs
LS.webs <- Real_webstats[8:17,]

######        
#generate niche models
niche.webs <- data.frame()
set.seed(1001)
for(n in 1:nrow(LS.webs)){
  N <- LS.webs$nodes[n]
  C <- LS.webs$Connectance[n]
  webs <- NicheModelLinks(n = 100, S = N,C = C)
  
  for(i in 1:length(webs)){
    a <- webs[[i]]
    mat <- matrix(0,nrow=N,ncol=N)
    print(paste(n,i))
    for(j in 1:nrow(a)){
      mat[a[j,1],a[j,2]] <- 1
    }
    nodes <- N
    links <- sum(mat)
    Linkage_Density <- links/nodes
    Connectance <- links/nodes^2
    
    Fraction_basal <- sum(colSums(mat) == 0)/nodes
    Fraction_Top   <- sum(colSums(t(mat)) == 0)/nodes
    Fraction_intermediate <- 1-Fraction_basal-Fraction_Top
    
    TG <- colSums(mat)
    Trophic_Generality <- mean(TG)
    Trophic_Generality_SD <- sd(TG/Linkage_Density)
    
    TV <- colSums(t(mat))
    Trophic_Vunerability <- mean(TV)
    Trophic_Vunerability_SD <- sd(TV/Linkage_Density)
    
    sum.dat <- data.frame(nodes, links , Linkage_Density, Connectance,
                          Fraction_basal ,Fraction_intermediate , Fraction_Top ,
                          Trophic_Generality , Trophic_Generality_SD ,
                          Trophic_Vunerability , Trophic_Vunerability_SD,
                          title = as.character(LS.webs$Title[n])) 
    niche.webs <-  rbind(niche.webs,sum.dat)
  }
}

#plot results
plotting <- melt(niche.webs,'title')

Real_webstats_melted <- Real_webstats[8:17,]
Real_webstats_melted <- melt(Real_webstats_melted,'Title')

ggplot(plotting,aes(x=title,y=value,fill=title))+
  geom_violin()+
  facet_wrap(~variable,scale='free')+
  geom_point(data = Real_webstats_melted,aes(x=Title,y=value,fill=NULL))+
  theme(axis.text.x = element_blank())

#Calculating ME
## Normalised difference between the median and real value
## (median - real)/abs(median-(95% or 5%))
#Getting the medians and 
ME.data <- melt(niche.webs,id.vars = 'title')
ME.data <- ME.data %>% group_by(title,variable) %>% dplyr::summarise(model.median     = median(value),
                                                                     model.percent.95 = quantile(value,0.95),
                                                                     model.percent.05 = quantile(value,0.05))
colnames(ME.data)[1] <- "Title"
#Add real data
ME.merged <- merge(ME.data,Real_webstats_melted,by = c("Title","variable"))
ME.merged$ME <- NA
#get the MEs
for(i in 1:nrow(ME.data)){
  ME.merged$ME[i] <- (ME.merged$model.median[i] - ME.merged$value[i]) 
  if(ME.merged$ME[i] < 0){
    a <- ME.merged$ME[i] / (ME.merged$model.median[i] - ME.merged$model.percent.95[i])
  } else {
    a<- ME.merged$ME[i] / (ME.merged$model.median[i] - ME.merged$model.percent.05[i])
  }
  ME.merged$ME[i] <- abs(a)
}

#Fig 3 Structural stats
#remove the basic stats
toKeep <- unique(plotting$variable)[-1:-4]
plotting <- plotting[plotting$variable %in% toKeep,]
ME.merged <- ME.merged[ME.merged$variable %in% toKeep,]

#Add column with the web and version to plotting a ME data
plotting$web <- NA
plotting$type <- NA
ME.merged$web <- NA
ME.merged$type <- NA

web <- c("Arab","BSQ","CSM","EPB","Quick")
for(i in 1:nrow(plotting)){
  plotting$web[i] <- web[ceiling(which(unique(plotting$title) %in% plotting$title[i])/2)]
  if(length(grep('axa',plotting$title[i])) > 0 || length(grep('lump',plotting$title[i])) > 0 ){
    plotting$type[i] <- "Taxanomic"
  }else{
    plotting$type[i] <- "LifeStage"
  }
}

for(i in 1:nrow(ME.merged)){
  ME.merged$web[i] <- web[ceiling(which(unique(ME.merged$Title) %in% ME.merged$Title[i])/2)]
  if(length(grep('axa',ME.merged$Title[i])) > 0 || length(grep('lump',ME.merged$Title[i])) > 0 ){
    ME.merged$type[i] <- "Taxanomic"
  }else{
    ME.merged$type[i] <- "LifeStage"
  }
}

#Is significant?
ME.merged$sig <- NA
for(i in 1:length(ME.merged$ME)){
  ME.merged$sig[i] <- (ME.merged$ME[i]) > 1
}


ggplot(plotting,aes(x=web,y=value,fill=type,colour=type))+
  geom_violin(position = 'identity' ,alpha = 0.5)+
  facet_wrap(~variable, scales = 'free_y',nrow=2)+
  theme_light()+
  geom_point(data = ME.merged,aes(y=value), position = 'identity')

ggplot(ME.merged,aes(x=web,y=ME,colour=type))+
  geom_point(position = 'identity' ,alpha = 0.5)+
  facet_wrap(~variable, scales = 'free_y',nrow=2)+
  theme_light()+
  geom_hline(yintercept = 1)

ME.diff <- ME.merged

ME.diff$ME <- 1 - abs(ME.diff$ME)

ggplot(ME.diff,aes(x=web,y=ME,colour=type))+
  geom_point(position = 'identity' ,alpha = 0.5)+
  facet_wrap(~variable, scales = 'free_y',nrow=2)+
  theme_light()


### Presentation Fig
ggplot(ME.merged,aes(x=Title,y=variable,fill=sig))+
  geom_tile()

ME.merged$variable <- as.character(ME.merged$variable)

a <- ME.merged %>% group_by(variable,type) %>% mutate(sig = sum(value))

#########
##Motifs##
#########
library(igraph)
source("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/Motif_functions.R")
communities.LH <- communities[8:17]

communities.igraph <- lapply(communities.LH,ToIgraph)

communities.motif <- matrix(nrow=10,ncol=13)
for(i in 1:10){
  print(paste(communities.LH[[i]]$properties$title))
  for(j in 1:13){
    print(paste('motif',j))
    communities.motif[i,j] <- graph.count.subisomorphisms.vf2(communities.igraph[[i]],subgraph3.graph[[j]])
  }
}

communities.motif <- as.data.frame(communities.motif)

communities.motif$n <- 1:10

communities.motif <- melt(communities.motif,'n')



#getting random web motifs
motifs <- list()
for(i in 1:10){motifs[[i]] <- data.frame(matrix(ncol=13,nrow=1000))}

for(i in 1:1000){
  name <- paste0("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/data/","motifs",i,".csv")
  motifbatch <- read.csv(name,row.names = 1)
  for(j in 1:10){
    motifs[[j]][i,] <- (motifbatch[j,])
  }
}


#add numbers to id
for(i in 1:10){
  motifs[[i]] <- as.data.frame(cbind(motifs[[i]],cbind(i,1:1000)))
  colnames(motifs[[i]]) <- 1:15
  motifs[[i]]<- melt(motifs[[i]],id.vars = c("14",'15'))
}

#Add 
motifs <- bind_rows(motifs)
colnames(motifs) <- c("Web","Num","Motif","Value")

motifs <- motifs %>% dplyr::group_by(Motif,Web) %>% dplyr::summarise(mean = mean(Value), sd = sd(Value)) %>% ungroup()

communities.motif$variable <- as.numeric(as.factor(communities.motif$variable))

colnames(communities.motif) <- c("Web","Motif","Value")

motifs <- merge(communities.motif,motifs,by = c("Web","Motif"))
motifs <- as.data.frame(motifs)

#adding plotting groups
motifs$Title <- as.character(Real_webstats[8:17,]$Title)[motifs$Web]
motifs$LS <- rep(c("Life stage", "non-Life stage"),5)[motifs$Web]
motifs$group <- c("Arab","BSQ","CSM","EPB","Quickpond")[rep(1:5,each = 2)[motifs$Web]]

#Z score
motifs$Zscore <- (motifs$Value - motifs$mean)/motifs$sd

motifs$Zscore[is.nan(motifs$Zscore)] <- 0
#squared measure
motifs$Zscore2 <- motifs$Zscore^2
motifs <- motifs %>% dplyr::group_by(Title) %>% dplyr::mutate(Norm = sqrt(sum(Zscore2)))

motifs <- data.frame(motifs)
motifs$value <- motifs$Zscore/motifs$Norm

motifNames <- paste(c(rep('S',5),rep('D',8)),c(1:5,1:8),sep = '')

motifs$Motif <- factor(motifNames[motifs$Motif],levels = motifNames)

ggplot(motifs,aes(x=Motif,y=value,group=Web,colour=LS))+
  geom_line()+
  geom_point(show.legend = F)+
  theme_classic()+
  ylab("z-Score Profile")+xlab("")+
  theme(
        legend.background = element_blank(),legend.title = element_blank())+
  ylim(-1,1) +
  geom_hline(yintercept = 0, linetype = 2)+
  facet_wrap(~group,ncol = 1)

#uncentered correlatin coefficent
r <- vector()
for(i in 1:length(unique(motifs$group))){
 a <- subset(motifs,motifs$group == unique(motifs$group)[i])
 r.temp <- 0
 for(j in 1:13){
   b <- subset(a,a$Motif == levels(a$Motif)[j])$value
   r.temp <- r.temp + (b[1] * b[2])
 }
 r[[i]] <- r.temp
}

names(r) <- unique(motifs$group)

r

#ratio of the zScore norms
d <- vector()
for(i in 1:length(unique(motifs$group))){
  a <- subset(motifs,motifs$group == unique(motifs$group)[i])
  b.ls <- unique(a$Norm[a$LS == "Life stage"])
  b.tax <- unique(a$Norm[a$LS == "non-Life stage"])
  
    d[[i]] <- b.tax/b.ls
}

names(d) <- unique(motifs$group)

d

#plot
motifStats <- data.frame(measure = c(rep('r',5),rep('d',5)),
                       value = c(r,d),
                       title = c(rep(names(r),2)))

rPlot <- ggplot(motifStats[motifStats$measure == 'r',],aes(x=value,y=title))+
  geom_point()+
  geom_vline(xintercept = 0, linetype = 2)+
  theme_classic()+
  xlim(c(-1,1))+
  xlab("Uncentred Correlation Coefficent (r)") + ylab("")

dPlot <- ggplot(motifStats[motifStats$measure == 'd',],aes(x=value,y=title))+
  geom_point()+
  geom_vline(xintercept = 1, linetype = 2)+
  theme_classic()+
  xlim(c(0,2))+
  xlab("Ratio of z-score Norms") + ylab("")+
  theme(axis.text.y = element_blank())

multiplot(rPlot,dPlot,cols = 2)

overlap.data$r <- rep(r,each=2)
overlap.data$d <- rep(d,each=2)

ggplot(overlap.data,aes(x = slope , y = r ))+
  geom_point()

ggplot(overlap.data,aes(x=slope,y=d))+
  geom_point()



#######################
# Degree Distribution #
#######################

#export to network 3d
source("~/Documents/Rwork/4th year/LifeHistoryPaper/Arabian_Gulf/network3D.R")

for(i in 1:length(communities[8:17])){
  title <- gsub(x = communities[8:17][[i]]$properties$title,pattern = ' ',replacement = "")
  directory <- paste("~/Documents/Rwork/4th year/LifeHistoryPaper/Network3d/",title,sep = "")
  dir.create(directory)
  ExportToNetwork3D(communities[8:17][[i]],dir = directory)
}

LS.coms <- communities[8:17]

InDegreeDistribution(LS.coms[[2]])

log(sd(InDegree(LS.coms[[1]]))/sd(InDegree(LS.coms[[2]])))

for(i in 8:17){
 mat <- PredationMatrix(communities[[i]])
 name <- gsub(names$Title[i-7],pattern = " ",replacement = "")
 dir <- paste("~/Desktop/motifs/Sent/mats/",name,".csv",sep = "")
 write.csv(mat,dir)
}


