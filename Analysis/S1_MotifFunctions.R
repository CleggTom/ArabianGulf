##############
# Analysis of Life History Webs
# Script S1) Motif Helper Functions:
# Clegg, Ali and Beckerman 2018: The Impact of Intraspecific Variation on Food Web Structure
##############
# Doi:
# Last edited 01.02.2017
# Copyright (c) 2017 the authors
##############
#Function to convert to igraph object
ToIgraph <- function(community, weight=NULL)
{
  if(is.null(TLPS(community)))
  {
    stop('The community has no trophic links')
  }
  else
  {
    tlps <- TLPS(community, link.properties=weight)
    if(!is.null(weight))
    {
      tlps$weight <- tlps[,weight]
    }
    return (graph.data.frame(tlps,
                             vertices=NPS(community),
                             directed=TRUE))
  }
}


#objects to calculate Motifs

# Here are the adjacency matrices for each of the 13 subgraphs again
s1<-matrix(c(0,1,0,0,0,1,0,0,0),nrow=3,ncol=3)
s2<-matrix(c(0,1,1,0,0,1,0,0,0),nrow=3,ncol=3)
s3<-matrix(c(0,1,0,0,0,1,1,0,0),nrow=3,ncol=3)
s4<-matrix(c(0,0,1,0,0,1,0,0,0),nrow=3,ncol=3)
s5<-matrix(c(0,1,1,0,0,0,0,0,0),nrow=3,ncol=3)
d2<-matrix(c(0,1,1,1,0,1,0,0,0),nrow=3,ncol=3)
d1<-matrix(c(0,1,1,0,0,1,0,1,0),nrow=3,ncol=3)
d3<-matrix(c(0,0,1,1,0,0,1,0,0),nrow=3,ncol=3)
d4<-matrix(c(0,0,0,1,0,1,0,1,0),nrow=3,ncol=3)
d5<-matrix(c(0,1,1,0,0,1,1,0,0),nrow=3,ncol=3)
d6<-matrix(c(0,1,1,1,0,1,1,1,0),nrow=3,ncol=3)
d7<-matrix(c(0,1,1,1,0,1,1,0,0),nrow=3,ncol=3)
d8<-matrix(c(0,1,1,1,0,0,1,0,0),nrow=3,ncol=3)

# Turn them into a convenient list
subgraph3.mat<-list(s1,s2,s3,s4,s5,d1,d2,d3,d4,d5,d6,d7,d8)
# And then into a list of graph objects
subgraph3.graph<-lapply(subgraph3.mat,graph.adjacency)

rm(s1,s2,s3,s4,s5,d1,d2,d3,d4,d5,d6,d7,d8,subgraph3.mat)
print("i")
#Function to get the number of Single and Double links
NSingleDouble <- function(community){
  links <- community$trophic.links

  #remove canibals
  links = links[links$resource != links$consumer,]

  #find double links
  links.reversed <- data.frame(resource = links$consumer,
                               consumer = links$resource)

  doubles <- nrow(plyr::match_df(links,links.reversed))
  singles <- nrow(links) - doubles

  return(c(singles,doubles/2))
}
