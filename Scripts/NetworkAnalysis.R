#Script for running network analysis of thesis funders in R
#Author: Nick Matthews
#Date: 03/08/18

#Load in packages
library(ggplot2)
library(ggraph)
library(tidygraph)
library(dplyr)
library(igraph)
library(RColorBrewer)

#Set date
date = format(Sys.Date(), "%d%m%Y")

#Load ethosdf
load("Data/SponsorinfoDFWithAnnotations.RData")
#Load in unique sponsors
load("Data/SponsorMatches.RData")
head(sponsorMatchesDF)

#Extract just the most common
sponsors <- sponsorMatchesDF[!duplicated(sponsorMatchesDF$Ref),]
indexCutoff <- 1
sponsors <- sponsors[sponsors$TotalInIndex > indexCutoff & sponsors$SponsorClass != "none",c("SponsorID","Ref","TotalInIndex","SponsorClass")]

#Set Network IDs
ID <- 1:nrow(sponsors)
sponsors <- cbind(ID,sponsors)
save(sponsors,file=paste0("Data/SponsorsIndexCutoff_",indexCutoff,".RData"))
#Initate matrix
netMat <- matrix(data = 0, nrow = nrow(sponsors), ncol = nrow(sponsors))
#Extract combinations
recordSpons <- ethosdf[,paste0("sponsor",1:max(ethosdf$NoOfSponsor))]
#Simplify to a list
recordSponsList <- as.list(as.data.frame(t(recordSpons),stringsAsFactors = FALSE))
#Remove any "-1"
recordSponsList <- sapply(recordSponsList, function(x) x[x != "-1"])
#Remove any only one sponsor in length
recordSponsList <- recordSponsList[unlist(lapply(recordSponsList,length))>1]

#Now calculate edge weights - takes a while
for(ii in 1:nrow(sponsors)) {
  cat("Node",ii,"\n")
  #And for every other 
  for(jj in 1:nrow(sponsors)) {
    if(ii != jj & jj < ii) {
      netMat[ii,jj] <- sum(unlist(lapply(recordSponsList,function(x) 
        if(sponsors$Ref[ii] %in% as.character(x) & sponsors$Ref[jj] %in% as.character(x)) {return(TRUE)} else {return(FALSE)})))
    } else {
      break
    }
  }
}

#Save resultant netMat matrix
save(netMat,file=paste0("Data/CommonSponsorsNet_indexCut",indexCutoff,".RData"))
#Create initial graph object
netGraph <- graph_from_adjacency_matrix(netMat,mode="undirected",diag=FALSE,weighted = TRUE)

#Set names
V(netGraph)$name <- sponsors$Ref
V(netGraph)$type <- sponsors$SponsorClass
V(netGraph)$n <- sponsors$TotalInIndex
#Remove all conflicted nodes
netGraph <- delete.vertices(netGraph,V(netGraph)$type =="conflicted")
sponsors <- sponsors[sponsors$SponsorClass != "conflicted",]
#Set plotting properties
V(netGraph)$size <- log(V(netGraph)$n)
E(netGraph)$width <- log(E(netGraph)$weight)

#Set specialised type definition
type <- recode(V(netGraph)$type,"UKGov"="UK Government","business"="Business","charity"="Charity")
V(netGraph)$type2 <- factor(recode(V(netGraph)$type,"UKGov"="UK Government","business"="Business","charity"="Charity"),
                            levels = c("UK Government","Business","Charity"))

#Make hive plot
ggraph(netGraph,'hive',axis='type2') +
  geom_edge_hive(aes(alpha=log(as.numeric(weight))),width=1.5,show.legend = FALSE) + 
  geom_axis_hive(aes(colour=as.character(type2)),size=5,label=FALSE) +
  scale_colour_manual(name="",values=brewer.pal(3,"Set1"),labels=c("UK Government","Business","Charity")) +
  scale_edge_alpha(range=c(0.2,1)) +
  theme(panel.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_text(size=25, face="bold"),
        legend.text = element_text(size=25))
ggsave(paste0("Plots/NetworkHivePlot.pdf"),height=8,width=12,dpi=500)

#Network with just edges of at least 5, or any with no degree
netSig <- delete_edges(netGraph, E(netGraph)[weight<5])
netSig <- delete.vertices(netSig,degree(netSig)==0)

#####Plotting graph#####
#Manually set the names
V(netSig)$name2 <- c('Airbus','Astrazeneca','AWE','BAE Systems','BBSRC',
                      'British Heart Foundation','Cancer Research UK','CCFE','DSTL','ESRC',
                      'EPSRC','GSK','Johnson Matthey','MRC','NERC','National Grid','NIHR',
                      'ORSAS','Pfizer','QinetiQ','Research Council UK','Rolls-Royce',
                      'Royal Society','STFC','Severn Trent Water','Syngenta','TWI',
                      'Unilever','Wellcome Trust')
V(netSig)$type2 <- factor(V(netSig)$type,levels = c("UKGov","business","charity"))
#Make plot
ggraph(netSig) +
  geom_edge_link(aes(alpha=log(weight)),width=1.6,show.legend = FALSE) +
  geom_node_point(aes(colour=as.character(type2),size=n)) +
  geom_node_text(aes(label=name2),repel=TRUE,family="sans",fontface="bold.italic",size=5) +
  scale_colour_manual(name="",values=brewer.pal(3,"Set1"),
                      labels=c("UK Government","Business","Charity"),
                      guide=guide_legend(keywidth=0.4,keyheight=0.4,default.unit="inch",override.aes = list(size=8))) +
  scale_size(range=c(2,8),guide="none") +
  scale_edge_alpha(range=c(0.3,0.7)) +
  theme(panel.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        legend.title = element_text(size=25, face="bold"),
        legend.text = element_text(size=25))
ggsave(paste0("Plots/SelectNetworkPlot.pdf"),height=8,width=12,dpi=500)

#Compute louvain communities and redo
cluster <- cluster_louvain(netSig)
V(netSig)$cluster <- cluster$membership
ggraph(netSig) +
  geom_edge_link(aes(alpha=log(weight)),width=1.6,show.legend = FALSE) +
  geom_node_point(aes(colour=as.character(cluster),size=n)) +
  geom_node_text(aes(label=name2),repel=TRUE,family="sans",fontface="bold.italic",size=5) +
  scale_colour_manual(name="Communities (Louvain)",values=brewer.pal(4,"Set2"),
                      labels=c("Pharma & Biotech\nCompanies","Medical Research Charities","NERC & ESRC","Engineering &\nManufacturing Companies"),
                      guide=guide_legend(keywidth=0.7,keyheight=0.7,default.unit="inch",override.aes = list(size=18))) +
  scale_size(range=c(2,8),guide="none") +
  scale_edge_alpha(range=c(0.3,0.7)) +
  theme(panel.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        legend.title = element_text(size=25, face="bold"),
        legend.text = element_text(size=25))
ggsave(paste0("Plots/SelectNetworkPlotCommunities.pdf"),height=8,width=12,dpi=500)

