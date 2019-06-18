#Script for analysing annotated dataset and plotting the results
#Author: Nick Matthews
#Data: 31/07/18

#Load libraries
library(ggplot2)
library(RColorBrewer)
library(stringi)
library(scales)
library(dplyr)
library(ngram)
library(tm)

date = format(Sys.Date(), "%d%m%Y")

#Publication plots
source("Scripts/PublicationPlots.R")


#####Load in data####
load("Data/SponsorinfoDFWithAnnotations.RData")

#####Summary of totals#####
#With extrapolation
funderData <- data.frame(sponsor = c("Charitable", "UK Government", "Business"),
                          count = c(sum(ethosdf$CharitySponsor,na.rm = TRUE),
                                    sum(ethosdf$UKGovSponsor,na.rm = TRUE),
                                    sum(ethosdf$BusSponsor,na.rm = TRUE)),
                          sensitivity = c(0.821,0.966,0.725),
                          accuracy = c(0.913,1,0.983))
funderData$expectedAccuracy <- funderData$count * funderData$accuracy
funderData$extrap <- funderData$count/funderData$sensitivity
funderData$estimate <- funderData$count/funderData$sensitivity*funderData$accuracy
funderData$PercentThesis <- funderData$estimate/nrow(ethosdf)*100

write.csv(funderData,file = paste0("Plots/FunderTable.csv"))


#####Pie charts of sponsorship frequency####
#Charitable
charit <- data.frame(table(ethosdf$subjectClass[ethosdf$CharitySponsor == 1]))
charit <- mutate(charit, Var1 = factor(c("Arts & Humanities","Medical","Science","Technology &\nManagement")),
                 cumulative = cumsum(Freq),
                 midpoint = sum(Freq) - (cumulative - Freq/2),
                 label = paste0(round(Freq/sum(Freq)*100),"%"))
ggplot(data=charit) +
  geom_bar(aes(x="",y=Freq, fill = Var1), stat = "identity") +
  coord_polar(theta="y") +
  scale_fill_Publication() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1.2,y = midpoint,label=label), size=16) +
  labs(title = "") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_text(size=35, face="bold"),
        legend.text = element_text(size=30)) +
  guides(fill=guide_legend(
    title="Subject",
    keywidth=0.8,
    keyheight=0.8,
    default.unit="inch"))
ggsave(paste0("Plots/CharitablePieChart.pdf"),width = 12,height=8,dpi=500)

#For government
UKGov <- data.frame(table(ethosdf$subjectClass[ethosdf$UKGovSponsor == 1]))
UKGov <- mutate(UKGov, Var1 = factor(c("Arts & Humanities","Medical","Science","Technology &\nManagement")),
                cumulative = cumsum(Freq),
                midpoint = sum(Freq) - (cumulative - Freq/2),
                label = paste0(round(Freq/sum(Freq)*100),"%"))
ggplot(data=UKGov) +
  geom_bar(aes(x="",y=Freq, fill = Var1), stat = "identity") +
  coord_polar(theta="y") +
  scale_fill_Publication() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1.2,y = midpoint,label=label), size=16) +
  labs(title = "") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_text(size=35, face="bold"),
        legend.text = element_text(size=30)) +
  guides(fill=guide_legend(
    title="Subject",
    keywidth=0.8,
    keyheight=0.8,
    default.unit="inch"))
ggsave(paste0("Plots/UKGovPieChart.pdf"),width = 12,height=8,dpi=500)

#For Business
Bus <- data.frame(table(ethosdf$subjectClass[ethosdf$BusSponsor == 1]))
Bus <- mutate(Bus, Var1 = factor(c("Arts & Humanities","Medical","Science","Technology &\nManagement")),
              cumulative = cumsum(Freq),
              midpoint = sum(Freq) - (cumulative - Freq/2),
              label = paste0(round(Freq/sum(Freq)*100),"%"))
ggplot(data=Bus) +
  geom_bar(aes(x="",y=Freq, fill = Var1), stat = "identity") +
  coord_polar(theta="y") +
  scale_fill_Publication() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1.2,y = midpoint,label=label), size=16) +
  labs(title = "") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_text(size=35, face="bold"),
        legend.text = element_text(size=30)) +
  guides(fill=guide_legend(
    title="Subject",
    keywidth=0.8,
    keyheight=0.8,
    default.unit="inch"))
ggsave(paste0("Plots/BusinessPieChart.pdf"),width = 12,height=8,dpi=500)

#For AMRC
AMRC <- data.frame(table(ethosdf$subjectClass[ethosdf$AMRCSponsor == 1]))
AMRC <- mutate(AMRC, Var1 = factor(c("Arts & Humanities","Medical","Science","Technology &\nManagement")),
               cumulative = cumsum(Freq),
               midpoint = sum(Freq) - (cumulative - Freq/2),
               label = paste0(round(Freq/sum(Freq)*100),"%"))
#Remove very small labels
AMRC$label[AMRC$Freq/sum(AMRC$Freq)*100 < 5] <- ""
#plot
ggplot(data=AMRC) +
  geom_bar(aes(x="",y=Freq, fill = Var1), stat = "identity") +
  coord_polar(theta="y") +
  scale_fill_Publication() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1.2,y = midpoint,label=label), size=16) +
  labs(title = "") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_text(size=35, face="bold"),
        legend.text = element_text(size=30)) +
  guides(fill=guide_legend(
    title="Subject",
    keywidth=0.8,
    keyheight=0.8,
    default.unit="inch"))
ggsave(paste0("Plots/AMRCPieChart.pdf"),width = 12,height=8,dpi=500)
#For nonAMRC Charitable
ethosdf$CharitySponsorNotAMRC <- rep(FALSE,nrow(ethosdf))
ethosdf$CharitySponsorNotAMRC[ethosdf$AMRCSponsor == 0 & ethosdf$CharitySponsor == 1] <- TRUE
nonAMRC <- data.frame(table(ethosdf$subjectClass[ethosdf$CharitySponsorNotAMRC == TRUE]))
nonAMRC <- mutate(nonAMRC, Var1 = factor(c("Arts & Humanities","Medical","Science","Technology &\nManagement")),
                  cumulative = cumsum(Freq),
                  midpoint = sum(Freq) - (cumulative - Freq/2),
                  label = paste0(round(Freq/sum(Freq)*100),"%"))
ggplot(data=nonAMRC) +
  geom_bar(aes(x="",y=Freq, fill = Var1), stat = "identity") +
  coord_polar(theta="y") +
  scale_fill_Publication() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1.2,y = midpoint,label=label), size=16) +
  labs(title = "") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_text(size=35, face="bold"),
        legend.text = element_text(size=30)) +
  guides(fill=guide_legend(
    title="Subject",
    keywidth=0.8,
    keyheight=0.8,
    default.unit="inch"))
ggsave(paste0("Plots/NonAMRCPieChart.pdf"),width = 12,height=8,dpi=500)

#And then export the data table of the pie charts
subjectSponsorTable <- rbind(table(ethosdf$subjectClass),
                             table(ethosdf$subjectClass[ethosdf$UKGovSponsor == 1]),
                             table(ethosdf$subjectClass[ethosdf$BusSponsor == 1]),
                             table(ethosdf$subjectClass[ethosdf$CharitySponsor == 1]),
                             table(ethosdf$subjectClass[ethosdf$AMRCSponsor == 1]),
                             table(ethosdf$subjectClass[ethosdf$CharitySponsorNotAMRC == 1]))
row.names(subjectSponsorTable) <- c("All","UK Gov", "Business","Charitable","AMRC","non-AMRC")
write.csv(subjectSponsorTable,file = paste0("Plots/SubjectSponsorTable.csv"))


#####Co-Sponsoring#####
#Summary table of cosponsoring
summariesCoSpon <- data.frame("GovCharity" = rep(0,4),"GovBus" = rep(0,4),"BusCharity" = rep(0,4),"AllThree" = rep(0,4),
                         subject = c("Arts & Humanities","Medical","Science","Technology & Management"))
subjectClasses <- c("Arts & Humanities","Medical","Science","Technology & Management")
for(i in 1:4) {
  summariesCoSpon$GovCharity[i] <- sum(ethosdf$subjectClass == subjectClasses[i] & ethosdf$UKGovSponsor==TRUE & ethosdf$CharitySponsor==TRUE)
  summariesCoSpon$GovBus[i] <- sum(ethosdf$subjectClass == subjectClasses[i] & ethosdf$UKGovSponsor==TRUE & ethosdf$BusSponsor==TRUE)
  summariesCoSpon$BusCharity[i] <- sum(ethosdf$subjectClass == subjectClasses[i] & ethosdf$BusSponsor==TRUE & ethosdf$CharitySponsor==TRUE)
  summariesCoSpon$AllThree[i] <- sum(ethosdf$subjectClass == subjectClasses[i] & ethosdf$UKGovSponsor==TRUE & ethosdf$CharitySponsor==TRUE & ethosdf$BusSponsor==TRUE)
  summariesCoSpon[i,1:4] <- summariesCoSpon[i,1:4]/sum(ethosdf$subjectClass == subjectClasses[i])
}

write.csv(summariesCoSpon,file = paste0("Plots/CoSponsorTable.csv"))


#Cosponsoring table 2
summariesCoSpon2 <- data.frame("NoSponsors" = rep(0,3),"UKGov" = rep(0,3),"Charity" = rep(0,3),"Business" = rep(0,3),
                               subject = c("UK Government","Charity","Business"))
sponsorsTypes <- c("UKGovSponsor","CharitySponsor","BusSponsor")
sponsorsTypes2 <- c("UKGov","charity","business")
for(i in 1:3) {
  summariesCoSpon2$NoSponsors[i] <- mean(ethosdf$NoOfSponsor[ethosdf[,sponsorsTypes[i]]==TRUE])
  summariesCoSpon2$UKGov[i] <- sum(ethosdf[,sponsorsTypes[i]] == TRUE & ethosdf$UKGovSponsor==TRUE)
  summariesCoSpon2$Charity[i] <- sum(ethosdf[,sponsorsTypes[i]] == TRUE & ethosdf$CharitySponsor==TRUE)
  summariesCoSpon2$Business[i] <- sum(ethosdf[,sponsorsTypes[i]] == TRUE & ethosdf$BusSponsor==TRUE)
  #Replace diagnol with internal co-sponsoring
  summariesCoSpon2[i,i+1] <- sum(rowSums(ethosdf[,paste0("sponsorClass",1:max(ethosdf$NoOfSponsor))] == sponsorsTypes2[i])>1)
  summariesCoSpon2[i,2:4] <- summariesCoSpon2[i,2:4]/sum(ethosdf[,sponsorsTypes[i]] == TRUE)*100
}

write.csv(summariesCoSpon2,file = paste0("Plots/CoSponsorTable2.csv"))

#####Chi squared tests for subjects and plots of residuals#####
#Run tests
testCharity <- chisq.test(table(as.character(ethosdf$CharitySponsor),as.character(ethosdf$subjectClass)))
testBusiness <- chisq.test(table(as.character(ethosdf$BusSponsor),as.character(ethosdf$subjectClass)))
testUKGov <- chisq.test(table(as.character(ethosdf$UKGovSponsor),as.character(ethosdf$subjectClass)))
#Extract residuals for plotting - charitable
resid <- testCharity$residuals
resid <- data.frame(resid)
colnames(resid) <- c("Sponsor", "Subject", "Residuals")
resid$Sponsor <- as.character(resid$Sponsor)
resid$Sponsor[resid$Sponsor == FALSE] <- "Non-Charitable"
resid$Sponsor[resid$Sponsor == TRUE] <- "Charitable"
#Plot
ggplot(data = resid) +
  geom_bar(aes(x=Subject, y = Residuals, fill = Sponsor), stat="identity",position = "dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Arts &\nHumanities","Medical","Science","Technology &\nManagement")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Subject", y = "Residuals",title = "Charitable Funding Effect on Subject") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_blank(),
        legend.text = element_text(size=25)) +
  guides(fill=guide_legend(
    keywidth=0.6,
    keyheight=0.6,
    default.unit="inch")) +
  theme(panel.spacing = unit(1, "lines"),strip.text = element_text(size=38,face="bold"),
        panel.grid.major = element_line(color = "gray75"),
        axis.text.x = element_text(size=25))
ggsave(paste0("Plots/CharitableResiduals.pdf"),width = 14, height = 7.5,dpi=500)

#Extract residuals for plotting - business
resid <- testBusiness$residuals
resid <- data.frame(resid)
colnames(resid) <- c("Sponsor", "Subject", "Residuals")
resid$Sponsor <- as.character(resid$Sponsor)
resid$Sponsor[resid$Sponsor == FALSE] <- "Non-Business"
resid$Sponsor[resid$Sponsor == TRUE] <- "Business"
#Plot
ggplot(data = resid) +
  geom_bar(aes(x=Subject, y = Residuals, fill = Sponsor), stat="identity",position = "dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Arts &\nHumanities","Medical","Science","Technology &\nManagement")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Subject", y = "Residuals",title = "Business Funding Effect on Subject") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_blank(),
        legend.text = element_text(size=25)) +
  guides(fill=guide_legend(
    keywidth=0.6,
    keyheight=0.6,
    default.unit="inch")) +
  theme(panel.spacing = unit(1, "lines"),strip.text = element_text(size=38,face="bold"),
        panel.grid.major = element_line(color = "gray75"),
        axis.text.x = element_text(size=25))
ggsave(paste0("Plots/BusinessResiduals.pdf"),width = 14, height = 7.5,dpi=500)

#Extract residuals for plotting - UK Government
resid <- testUKGov$residuals
resid <- data.frame(resid)
colnames(resid) <- c("Sponsor", "Subject", "Residuals")
resid$Sponsor <- as.character(resid$Sponsor)
resid$Sponsor[resid$Sponsor == FALSE] <- "Non-UK Government"
resid$Sponsor[resid$Sponsor == TRUE] <- "UK Government"
resid$Sponsor <- factor(resid$Sponsor,levels = c("UK Government","Non-UK Government"))
#Plot
ggplot(data = resid) +
  geom_bar(aes(x=Subject, y = Residuals, fill = Sponsor), stat="identity",position = "dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Arts &\nHumanities","Medical","Science","Technology &\nManagement")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Subject", y = "Residuals",title = "UK Government Funding Effect on Subject") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_blank(),
        legend.text = element_text(size=25)) +
  guides(fill=guide_legend(
    keywidth=0.6,
    keyheight=0.6,
    default.unit="inch")) +
  theme(panel.spacing = unit(1, "lines"),strip.text = element_text(size=38,face="bold"),
        panel.grid.major = element_line(color = "gray75"),
        axis.text.x = element_text(size=25))
ggsave(paste0("Plots/UKGovResiduals.pdf"),width = 14, height = 7.5,dpi=500)

#Not AMRC funded
ethosdf$CharitySponsorNotAMRC <- rep("FALSE",nrow(ethosdf))
ethosdf$CharitySponsorNotAMRC[ethosdf$AMRCSponsor == 0 & ethosdf$CharitySponsor == 1] <- TRUE
testNonAMRC <- chisq.test(table(as.character(ethosdf$CharitySponsorNotAMRC),as.character(ethosdf$subjectClass)))
#Extract residuals for plotting
resid <- testNonAMRC$residuals
resid <- data.frame(resid)
colnames(resid) <- c("Sponsor", "Subject", "Residuals")
resid$Sponsor <- as.character(resid$Sponsor)
resid$Sponsor[resid$Sponsor == FALSE] <- "Non-Charitable\n(Plus AMRC)"
resid$Sponsor[resid$Sponsor == TRUE] <- "Charitable\n(Non-AMRC)"
#Plot
ggplot(data = resid) +
  geom_bar(aes(x=Subject, y = Residuals, fill = Sponsor), stat="identity",position = "dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Arts &\nHumanities","Medical","Science","Technology &\nManagement")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Subject", y = "Residuals",title = "Non-AMRC Effect on Subject") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_blank(),
        legend.text = element_text(size=25)) +
  guides(fill=guide_legend(
    keywidth=0.6,
    keyheight=0.8,
    default.unit="inch")) +
  theme(panel.spacing = unit(1, "lines"),strip.text = element_text(size=38,face="bold"),
        panel.grid.major = element_line(color = "gray75"),
        axis.text.x = element_text(size=25))
ggsave(paste0("Plots/NonAMRCResiduals.pdf"),width = 14, height = 7.5,dpi=500)

#AMRC funded
test <- chisq.test(table(as.character(ethosdf$AMRCSponsor),as.character(ethosdf$subjectClass)))
#p-value < 2.2e-16
#Extract residuals for plotting
resid <- test$residuals
resid <- data.frame(resid)
colnames(resid) <- c("Sponsor", "Subject", "Residuals")
resid$Sponsor <- as.character(resid$Sponsor)
resid$Sponsor[resid$Sponsor == FALSE] <- "Non-AMRC"
resid$Sponsor[resid$Sponsor == TRUE] <- "AMRC"
#Plot
ggplot(data = resid) +
  geom_bar(aes(x=Subject, y = Residuals, fill = Sponsor), stat="identity",position = "dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Arts &\nHumanities","Medical","Science","Technology &\nManagement")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Subject", y = "Residuals",title = "AMRC Funding Effect on Subject") +
  theme(axis.title = element_text(size=35),
        axis.text.y = element_text(size=35),
        plot.title = element_text(hjust = 0.5, size = 45),
        legend.title = element_blank(),
        legend.text = element_text(size=25)) +
  guides(fill=guide_legend(
    keywidth=0.6,
    keyheight=0.6,
    default.unit="inch")) +
  theme(panel.spacing = unit(1, "lines"),strip.text = element_text(size=38,face="bold"),
        panel.grid.major = element_line(color = "gray75"),
        axis.text.x = element_text(size=25))
ggsave(paste0("Plots/AMRCResiduals.pdf"),width = 14, height = 7.5,dpi=500)









