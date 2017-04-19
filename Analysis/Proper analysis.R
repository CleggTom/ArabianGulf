### Full Analysis

#load packages
library(cheddar)
library(Basingstoke)
library(ggplot2)
library(tidyverse)
library(reshape2)


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
        #qucik
        sum(table(communities[[16]]$nodes$taxa.ID) > 1)/
          length(unique(communities[[16]]$nodes$taxa.ID)) #0.5079365
        #cali
        sum(table(communities[[14]]$nodes$taxa.ID) > 1)/
          length(unique(communities[[14]]$nodes$taxa.ID)) #0.3627907
        sum(table(communities[[12]]$nodes$taxa.ID) > 1)/
          length(unique(communities[[12]]$nodes$taxa.ID)) #0.3192771
        sum(table(communities[[10]]$nodes$taxa.ID) > 1)/
          length(unique(communities[[10]]$nodes$taxa.ID)) #0.3488372
        ######
        
        
#Plotting all S v L
ggplot(Real_webstats,aes(x=log(nodes),y=log(links),colour=Title,group=group))+
  geom_point()+
  geom_line()
#just the Lifestage webs
ggplot(Real_webstats[8:15,],aes(x=log(nodes),y=log(links),colour=Title,group=group))+
  geom_point()+
  geom_line()

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
SL.line.data <- rbind(taxa.model.line,life.model.line)

#Fig 1) 
ggplot(webstats.data[-7,],aes(x = nodes, y = links))+
  geom_point()+
  geom_line(data = SL.line.data, aes(linetype=Regression,group=Regression))+
  theme_bw()+
  labs(x = "Log (nodes)", y = "Log (links)")+
  theme(panel.grid = element_blank())



##Is The change along the SL relationship due to changes in overlap?
    # at lower overlap there is a smaller diet ( ??) and therefore a 
    #smaller increase in Links
    
        #Arab
        Nodes <-communities[[8]]$nodes
        Nodes <- Nodes[(Nodes %>% group_by(resolved) %>% mutate(n=n()))$n > 1,]
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
        Nodes <- Nodes[(Nodes %>% group_by(resolved) %>% mutate(n=n()))$n > 1,]
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
        
        ##COuld be a contender\
        
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

        ggplot(overlap.data,aes(x=res_overlap,y=slope))+
          geom_point()
        
        ggplot(overlap.data,aes(x=con_overlap,y=slope))+
          geom_point()
        
        #Fig 2
        overlap.fig <- melt(overlap.data,id.vars = 1:14)
        
        ggplot(overlap.fig,aes(y = slope, x = value))+
          geom_point()+
          facet_wrap(~variable)+
          theme_bw()+
          theme(panel.grid = element_blank())
   
    #Testing the actual null model webs for overlap 
      overlap_index <- read.csv("/Users/Tom/Documents/Julia/Life_history_paper/Cali_Overlap_index.csv")
      Null_overlap_data <- data.frame()
        for(i in 1:10){
          for(j in 1:100){
           print(paste('Web:',j,' Level:',i/10))
            mat <- read.csv(paste("/Users/Tom/Documents/Julia/Life_history_paper/LH_mats_2/matrix_",i,j,'.csv',sep=""))
            colnames(mat) <- rownames(mat) <- paste('Species',1:nrow(mat))
            Links <- PredationMatrixToLinks(mat)
            
            Nodes <- data.frame(node = paste('Species',1:nrow(mat)))
            index <- overlap_index[,((i-1)* 100)+j]
            if(TRUE){
            Nodes$resolved <- as.character(Nodes$node)
            for(k in 1:nrow(Nodes)){
              Nodes$resolved[k] <- paste('lumped',index[k])
              }
            
            Prop <- list(title = paste('Web',i/100,j))
            
            com <- Community(nodes = Nodes, properties = Prop, trophic.links = Links)
            
            #for each taxa
            N <-com$nodes
            N <- N %>% group_by(resolved) %>% mutate(n=n())
            N <- subset(N,N$n > 1)
            
            overlaps <- vector()
            for(y in 1:length(unique(N$resolved))){
              a <- subset(N,N$resolved == unique(N$resolved)[y])
              consumers <- list()
              Number <- nrow(a)
              for(x in 1:length(a$node)){
                L <- TrophicLinksForNodes(com,a$node[x])
                L <- subset(L$consumer,L$resource == a$node[x])
                consumers[[x]] <- L
              }
              comb <- combn(1:Number,2)
              result <- vector()
              for(x in 1:ncol(comb)){
                if(length(consumers) > 1){
                  one <-  consumers[[comb[1,x]]]
                  two <-  consumers[[comb[2,x]]]
                  
                  if(length(one) < 1 || length(two) < 1){
                    result[x] <- 0
                  } else {
                    result[x] <- length(intersect(one,two)) / length(unique(union(one,two)))
                  }} else {result[x] <- NA} 
              }
              overlaps[y] <- mean(result,na.rm = T)
            }
            
            con <- data.frame(mean_se(overlaps),overlap = 'con',overlap = i/10, web = paste('web',j),
                              S=NumberOfNodes(com),L=NumberOfTrophicLinks(com))

            overlaps <- vector()
            for(y in 1:length(unique(N$resolved))){
              a <- subset(N,N$resolved == unique(N$resolved)[y])
              consumers <- list()
              Number <- nrow(a)
              for(x in 1:length(a$node)){
                L <- TrophicLinksForNodes(com,a$node[x])
                L <- subset(L$resource,L$consumer == a$node[x])
                consumers[[x]] <- L
              }
              comb <- combn(1:Number,2)
              result <- vector()
              for(x in 1:ncol(comb)){
                if(length(consumers) > 1){
                  one <-  consumers[[comb[1,x]]]
                  two <-  consumers[[comb[2,x]]]
                  
                  if(length(one) < 1 || length(two) < 1){
                    result[x] <- 0
                  } else {
                    result[x] <- length(intersect(one,two)) / length(unique(union(one,two)))
                  }} else {result[x] <- NA} 
              }
              overlaps[y] <- mean(result,na.rm = T)
            }
            
            res <- data.frame(mean_se(overlaps),overlap = 'res',overlap = i/10, web = paste('web',j),
                              S=NumberOfNodes(com),L=NumberOfTrophicLinks(com))
            
            Null_overlap_data <- rbind(Null_overlap_data,con,res)
            
            }
          }
        }
        
      Null_overlap_data$slope <-  (log(Null_overlap_data$L) - log(Real_webstats$links[11]))/
                                    (log(Null_overlap_data$S) - log(Real_webstats$nodes[11]))
        
 
      
  #What about the links between lifestages
    #these links 'dissapear' when we aggregate webs
    #Potentialy a large amount of complexity is lost = decreases in C
        
        #Arab Web
        arab_links <- communities[[9]]$trophic.links
      arab_links$r.LH <- NA
      arab_links$c.LH <- NA
      
      arab_nodes <- communities[[9]]$nodes
      arab_nodes$n <- (communities[[8]]$nodes %>% group_by(resolved) %>% summarise(n=n()))$n
      
      for(i in 1:nrow(arab_links)){
        print(i)
        if(subset(arab_nodes$n ,arab_nodes$node == arab_links[i,]$consumer)>1){
          arab_links$c.LH[i] <- 1
        } else {
          arab_links$c.LH[i] <- 0
        }
        if(subset(arab_nodes$n ,arab_nodes$node == arab_links[i,]$resource)>1){
          arab_links$r.LH[i] <- 1
        } else {
          arab_links$r.LH[i] <- 0
        }
        
      }
      
      sum((arab_links$r.LH + arab_links$c.LH) == 2)/nrow(communities[[8]]$trophic.links)
      
      #Cali
      
      Cali_links <- communities[[11]]$trophic.links
      Cali_links$r.LH <- NA
      Cali_links$c.LH <- NA
      
      Cali_nodes <- communities[[11]]$nodes
      
      for(i in 1:nrow(Cali_links)){
        Cali_links[i,3] <- subset(Cali_nodes$LS,Cali_nodes$node == Cali_links[i,1]) 
        Cali_links[i,4] <- subset(Cali_nodes$LS,Cali_nodes$node == Cali_links[i,2]) 
      }
      
      sum((Cali_links$r.LH+Cali_links$c.LH)==2)/nrow(communities[[11]]$trophic.links)
      
    par(mfrow=c(1,2))  
    PlotWebByLevel(communities[[10]],colour.by = 'LS')
    PlotWebByLevel(communities[[11]],colour.by = 'LS')
    
### Doesnt look like it is the amount of links between lifestages
    
    #what about the generality/vunerability of the lifestage nodes
    
    Cali_Gen <- NormalisedTrophicGenerality(communities[[10]])
    Cali_Gen <- subset(Cali_Gen,communities[[10]]$nodes$LS==1)
    mean(Cali_Gen)
    
    Arab_Gen <- NormalisedTrophicGenerality(communities[[8]])
      N <- communities[[8]]$nodes %>% group_by(resolved) %>% mutate(n =n())
    Arab_Gen <- subset(Arab_Gen,N$n > 1)
    mean(Arab_Gen)
    
    Cali_Vun <- NormalisedTrophicVulnerability(communities[[10]])
    Cali_Vun <- subset(Cali_Vun,communities[[10]]$nodes$LS==1)
    mean(Cali_Vun)
    
    Arab_Vun <- NormalisedTrophicVulnerability(communities[[8]])
    N <- communities[[8]]$nodes %>% group_by(resolved) %>% mutate(n =n())
    Arab_Vun <- subset(Arab_Vun,N$n > 1)
    mean(Arab_Vun)
    
    #Could be a cause
      #The cali web lifestage nodes have less links
        #Aggregating results in less lost links
        #Is equivelent to altering the overlap...
    
   ###Cali overlap
        Cali_LS_var_overlap <- read_csv("~/Documents/Julia/Life_history_paper/Cali_LS_var_overlap.csv")
        Cali_LS_var_overlap2 <- read_csv("~/Documents/Julia/Life_history_paper/Cali_LS_var_overlap2.csv")
        
        ggplot(Cali_LS_var_overlap,aes(x = log(S), y =log(C)))+
          geom_point(colour='red')+
          geom_point(data = Cali_LS_var_overlap2,colour='blue')+
          geom_point(data = Real_webstats, aes(x=log(nodes),y=log(links)))
    
   
    
########
# Q2) Changes in Webstats vs niche models
########

#get lifestage webs
LS.webs <- Real_webstats[8:17,]

######        
#generate niche models
niche.webs <- data.frame()
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

  ME.diff$ME <- 1 - ME.diff$ME

  ggplot(ME.diff,aes(x=web,y=ME,colour=type))+
    geom_point(position = 'identity' ,alpha = 0.5)+
    facet_wrap(~variable, scales = 'free_y',nrow=2)+
    theme_light()
  
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


### Niche motifs
random.motifs <- list()
for(i in 1:10){
  links <- NumberOfTrophicLinks(communities.LH[[i]])
  nodes <- NumberOfNodes(communities.LH[[i]])
  
  motif <- matrix(nrow = 1000, ncol = 13)
  #generate random
  for(j in 1:1000){
   mat <-  matrix(0,nrow = nodes,
                    ncol = nodes)
   
   mat[sample(1:nodes^2, size = links)] <- 1
    
   graph <- graph_from_adjacency_matrix(mat)
    for(k in 1:13){
      print(paste(communities.LH[[i]]$properties$title, ' Web:',j,' Motif:',k))
      motif[j,k] <-  graph.count.subisomorphisms.vf2(graph,subgraph3.graph[[k]])
   }
   motif <- as.data.frame(motif)
  }
motif$n <- i  
random.motifs[[i]] <- motif
  }

a <- bind_rows(random.motifs)
a <- melt(a,'n')


a <- a %>% group_by(n,variable) %>% summarise(mean = mean(value), sd = sd(value))

motif.data <- merge(communities.motif,a)

motif.data$z_score <- (motif.data$value - motif.data$mean)/motif.data$sd

motif.data <- motif.data %>% group_by(n) %>% mutate(norm = sqrt(sum(z_score^2)))

motif.data$z_score <- motif.data$z_score/motif.data$norm

motif.data$n <- as.factor(motif.data$n)
levels(motif.data$n) <- unlist(lapply(X = communities.LH, FUN = function(x){return(x$properties$title)}))

ggplot(motif.data,aes(x=variable,y=z_score,group=n,colour=n))+
  geom_point()+
  geom_line()+
  ylim(-1,1)

communities.motif <- communities.motif %>% group_by(n) %>% mutate(total = sum(value))
communities.motif$prop <- communities.motif$value/communities.motif$total
communities.motif$LS <- rep(c('LS','Not LS'),65)

communities.motif$n <- as.factor(communities.motif$n)
levels(communities.motif$n) <- unlist(lapply(X = communities.LH, FUN = function(x){return(x$properties$title)}))

ggplot(communities.motif,aes(x=variable,y=prop,colour=LS,group=n,shape=n))+
  geom_point()+
  geom_line()

motif.data <- merge(communities.motif[,c(1,2,6)],motif.data)

ggplot(motif.data,aes(x=variable,y=z_score,group=n,colour=LS))+
  geom_point()+
  geom_line()+
  ylim(-1,1)

motif.data$group <- rep(1:5,each = 13*2)

motif.data <- motif.data %>% group_by(group,variable) %>% mutate(total = value - lag(value),totall = sum(value))

motif.data$totall <- motif.data$total / motif.data$totall

ggplot(motif.data,aes(x=variable,y=totall,colour=n,group=n))+
  geom_point()+
  geom_line()

#######################
# Degree Distribution #
#######################

library(fitdistrplus)

#cumsum
degree_list_coms <- list()
for(i in 8:17){
degree <- Degree(communities[[i]]) / LinkageDensity(communities[[i]])
degree.dist <- cumsum(DegreeDistribution(communities[[i]]))


degree_list_coms[[i]] <- data.frame(degree.dist = degree.dist,
                                    title = communities[[i]]$properties$title,
                                    LS    = if(i > 7){ if(i %% 2 == 0){'LS'} else {'Taxa'}} else {'Non-LS'},
                                    K = (1:length(degree.dist)) / LinkageDensity(communities[[i]]) )

}

df <- bind_rows(degree_list_coms)

ggplot(df,aes(x=(K),y=-((degree.dist)),colour=LS,group=title))+
  geom_line()


#PDF

degree_list_coms <- list(df = list(), dist = list())
for(i in 8:17){
  degree <- unname(Degree(communities[[i]]) / LinkageDensity(communities[[i]]))
  
  degree_list_coms$df[[i]] <- data.frame(degree = degree,
                       title = communities[[i]]$properties$title,
                       LS    = if(i > 7){ if(i %% 2 == 0){'LS'} else {'Taxa'}} else {'Non-LS'},
                       group = Real_webstats$group[i])
  
  degree_list_coms$dist[[i]] <- fitdist(degree, distr = "weibull", method = "mle", lower = c(0, 0))
  }

degree_list_coms$dist


df <- bind_rows(degree_list_coms$df)

ggplot(df,aes(x=degree))+
  geom_histogram(binwidth = 1)+
  facet_grid(LS~group,scales = 'free')

split.df <- split(df,df$N)

fitdistr(split.df[[1]]$cum.degree, densfun = 'weibull')
