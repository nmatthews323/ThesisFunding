#This script takes a dataframe of ethos outputs and searches for terms in their sponsor info
#Author: Nick Matthews

#Load libraries
library(ggplot2)
library(RColorBrewer)
library(stringi)
library(scales)
library(dplyr)
library(ngram)
library(tm)

#Load in date
date = format(Sys.Date(), "%d%m%Y")

#Load in source code
source("Scripts/ThesisFundingSourceCode.R")

#####Read in and process#####
#Load in the ethos data containing the necessary info
ethosdf <- read.csv("Data/RecordsWithSponsorInfoEtc.csv",stringsAsFactors = FALSE)

#####Do DDC classifications#####
#Normal DDC
ethosdf$DDC <- as.numeric(ethosdf$subject2)
ethosdf <- subjectClassify(ethosdf,type = "bespoke")


#####Cleaning and processing sponsors#####
#Extract all the sponsors
Sponsors <- trimws(unlist(strsplit(ethosdf$sponsor,"\\,|\\, |\\ , |\\;|\\; |\\ ; |\\:|\\: |\\ : ")),which = "both")
#Clean and process the sponsors
cleanSponsors <- wordClean(Sponsors)

#Specify set of false abbreviations and ones to add
removeSet <- c("mexico","firm","u.s.","tetfund","csc","london","airbus", "arup", "brazil", 
               "brunei", "chile", "egypt", "iraq", "jordan", "libya","london", "mexico", 
               "pfizer", "shell", "syria","qinetiq","usa","taiwan","nigeria","england")

addSet <- c("bbsrc","epsrc","ahrc","esrc","stfc","mrc","nerc")

#Extract abbreviations
abbreviations <- extractAbbreviations(Sponsors,removeSet,addSet)


#Once abbreviations done now do fuzzy matching of unique dataset to match together sponsors which are the same
#Only for those that are long and have more than one word
problemTerms <- c("research council", "school eee", "se controls","ge healthcare","uk government","e. engineering","e engineering",
                  "mas foundation", "e. engineering ltd","e engineering ltd", "e. technologies","e technologies", "national heart", "science technology",
                  "physical sciences research council","welcome trust","wates foundation",
                  "national environment research council","national environmental research council","natural environment research counci",
                  "university jos","graduate school", "innovateuk","school eese","one north east",
                  "graduate school","higher education","ministry education","biomedical research centre",
                  "research fellowship","school engineering","city hospital","national trust","medical school","rac foundation",
                  "food rural affairs","engineering technology","innovation skills","school health","school english","school history",
                  "national heart lung institute","scotland ltd"
                  )
#Special merge for epsrc misspellings
epsrcmerge <- list(epsrc=c("epsrc","eprsc","esprc","eprc","epsrs"))

#Now merge together sponsor matches into consistent df
sponsorMatchesDF <- sponsorProcess(Sponsors,abbreviations,problemTerms,manualmerges = epsrcmerge)

#Load in and process all the search terms, give short terms \\b bounding so have to be word in itself
#Set search terms for government sources
GovSearchTermsFile <- read.csv("Data/SearchTerms/UKGovSearchTerms.csv",header = TRUE)
GovSearchTerms <- wordClean(GovSearchTermsFile$Term)
GovSearchTerms[nchar(GovSearchTerms) <= 6] <- paste0("\\b",GovSearchTerms[nchar(GovSearchTerms) <= 6],"\\b")

#Set search terms for business sources
BusSearchTermsFile <- read.csv("Data/SearchTerms/BusinessSearchTerms.csv",header = TRUE)
BusSearchTerms <- wordClean(BusSearchTermsFile$Term)
BusSearchTerms[nchar(BusSearchTerms) <= 6] <- paste0("\\b",BusSearchTerms[nchar(BusSearchTerms) <= 6],"\\b")

#Set search terms for charity sources
CharitySearchTermsFile <- read.csv("Data/SearchTerms/CharitySearchTerms.csv",header = TRUE)
CharitySearchTerms <- wordClean(CharitySearchTermsFile$Term)
CharitySearchTerms[nchar(CharitySearchTerms) <= 6] <- paste0("\\b",CharitySearchTerms[nchar(CharitySearchTerms) <= 6],"\\b")
CharitySearchTermsGen <- CharitySearchTerms[CharitySearchTermsFile$Type == "General"]
CharitySearchTermsSpec <- CharitySearchTerms[CharitySearchTermsFile$Type != "General"]
CharitySearchTermsAMRC <- CharitySearchTerms[CharitySearchTermsFile$Type == "AMRC"]


#Now do the assignments, by fuzzy matching or straight matching depending on the length
#If do fuzzy matching for very short search terms it's far too noisy
#UKGov
sponsorMatchesDF$UKGovSponsorObs <- rep(FALSE, nrow(sponsorMatchesDF))
sponsorMatchesDF$UKGovSponsorRef <- rep(FALSE, nrow(sponsorMatchesDF))
#Do matches for ref
sponsorMatchesFuzzy <- lapply(GovSearchTerms[nchar(GovSearchTerms) > 10],
                                             function(x) agrep(x,sponsorMatchesDF$Ref, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(GovSearchTerms[nchar(GovSearchTerms) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Ref, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$UKGovSponsorRef[sponsorMatches] <- TRUE
#Do matches for obs
sponsorMatchesFuzzy <- lapply(GovSearchTerms[nchar(GovSearchTerms) > 10],
                              function(x) agrep(x,sponsorMatchesDF$Obs, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(GovSearchTerms[nchar(GovSearchTerms) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Obs, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$UKGovSponsorObs[sponsorMatches] <- TRUE

#Business
sponsorMatchesDF$BusSponsorObs <- rep(FALSE, nrow(sponsorMatchesDF))
sponsorMatchesDF$BusSponsorRef <- rep(FALSE, nrow(sponsorMatchesDF))
#Do matches for ref
sponsorMatchesFuzzy <- lapply(BusSearchTerms[nchar(BusSearchTerms) > 10],
                              function(x) agrep(x,sponsorMatchesDF$Ref, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(BusSearchTerms[nchar(BusSearchTerms) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Ref, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$BusSponsorRef[sponsorMatches] <- TRUE
#Do matches for obs
sponsorMatchesFuzzy <- lapply(BusSearchTerms[nchar(BusSearchTerms) > 10],
                              function(x) agrep(x,sponsorMatchesDF$Obs, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(BusSearchTerms[nchar(BusSearchTerms) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Obs, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$BusSponsorObs[sponsorMatches] <- TRUE


#Charities specific
sponsorMatchesDF$CharitySponsorObs <- rep(FALSE, nrow(sponsorMatchesDF))
sponsorMatchesDF$CharitySponsorRef <- rep(FALSE, nrow(sponsorMatchesDF))
#Do matches for Ref
sponsorMatchesFuzzy <- lapply(CharitySearchTermsSpec[nchar(CharitySearchTermsSpec)],
                              function(x) agrep(x,sponsorMatchesDF$Ref, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(CharitySearchTermsSpec[nchar(CharitySearchTermsSpec) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Ref, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$CharitySponsorRef[sponsorMatches] <- TRUE
#Do matches for obs
sponsorMatchesFuzzy <- lapply(CharitySearchTermsSpec[nchar(CharitySearchTermsSpec) > 10],
                              function(x) agrep(x,sponsorMatchesDF$Obs, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(CharitySearchTermsSpec[nchar(CharitySearchTermsSpec) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Obs, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$CharitySponsorObs[sponsorMatches] <- TRUE
#Charities General -  three exclusion terms included
#Do matches for ref
sponsorMatchesFuzzy <- lapply(CharitySearchTermsGen[nchar(CharitySearchTermsGen) > 10],
                              function(x) setdiff(agrep(x,sponsorMatchesDF$Ref, ignore.case = TRUE,max.distance = 0.05),
                                                  grep("\\bNHS\\b|national science foundation|greek state scholarship", sponsorMatchesDF$Ref, ignore.case = TRUE)))
sponsorMatchesExact <- lapply(CharitySearchTermsGen[nchar(CharitySearchTermsGen) <= 10],
                              function(x) setdiff(grep(x,sponsorMatchesDF$Ref, ignore.case = TRUE),
                                                  grep("\\bNHS\\b|national science foundation|greek state scholarship", sponsorMatchesDF$Ref, ignore.case = TRUE)))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$CharitySponsorRef[sponsorMatches] <- TRUE
#Do matches for obs
sponsorMatchesFuzzy <- lapply(CharitySearchTermsGen[nchar(CharitySearchTermsGen) > 10],
                              function(x) setdiff(agrep(x,sponsorMatchesDF$Obs, ignore.case = TRUE,max.distance = 0.05),
                                                  grep("\\bNHS\\b|national science foundation|greek state scholarship", sponsorMatchesDF$Ref, ignore.case = TRUE)))
sponsorMatchesExact <- lapply(CharitySearchTermsGen[nchar(CharitySearchTermsGen) <= 10],
                              function(x) setdiff(grep(x,sponsorMatchesDF$Obs, ignore.case = TRUE),
                                                  grep("\\bNHS\\b|national science foundation|greek state scholarship", sponsorMatchesDF$Ref, ignore.case = TRUE)))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$CharitySponsorObs[sponsorMatches] <- TRUE

#AMRC
sponsorMatchesDF$AMRCSponsorObs <- rep(FALSE, nrow(sponsorMatchesDF))
sponsorMatchesDF$AMRCSponsorRef <- rep(FALSE, nrow(sponsorMatchesDF))
#Do matches for ref
sponsorMatchesFuzzy <- lapply(CharitySearchTermsAMRC[nchar(CharitySearchTermsAMRC) > 10],
                              function(x) agrep(x,sponsorMatchesDF$Ref, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(CharitySearchTermsAMRC[nchar(CharitySearchTermsAMRC) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Ref, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$AMRCSponsorRef[sponsorMatches] <- TRUE
#Do matches for obs
sponsorMatchesFuzzy <- lapply(CharitySearchTermsAMRC[nchar(CharitySearchTermsAMRC) > 10],
                              function(x) agrep(x,sponsorMatchesDF$Obs, ignore.case = TRUE,max.distance = 0.05))
sponsorMatchesExact <- lapply(CharitySearchTermsAMRC[nchar(CharitySearchTermsAMRC) <= 10],
                              function(x) grep(x,sponsorMatchesDF$Obs, ignore.case = TRUE))
sponsorMatches <- unique(c(unlist(sponsorMatchesFuzzy),unlist(sponsorMatchesExact)))
sponsorMatchesDF$AMRCSponsorObs[sponsorMatches] <- TRUE

#Now determine sponsor class
sponsorMatchesDF$SponsorClass <- rep("none",nrow(sponsorMatchesDF))
#If clear from the ref assign that ref
sponsorMatchesDF$SponsorClass[sponsorMatchesDF$CharitySponsorRef == TRUE & 
                                 !sponsorMatchesDF$BusSponsorRef == TRUE &
                                 !sponsorMatchesDF$UKGovSponsorRef == TRUE] <- "charity"
sponsorMatchesDF$SponsorClass[!sponsorMatchesDF$CharitySponsorRef == TRUE & 
                                  sponsorMatchesDF$BusSponsorRef == TRUE &
                                  !sponsorMatchesDF$UKGovSponsorRef == TRUE] <- "business"
sponsorMatchesDF$SponsorClass[!sponsorMatchesDF$CharitySponsorRef == TRUE & 
                                  !sponsorMatchesDF$BusSponsorRef == TRUE &
                                  sponsorMatchesDF$UKGovSponsorRef == TRUE] <- "UKGov"

#If more than half of Obs are there then assign as true for all
forTest <- unique(sponsorMatchesDF$Ref[sponsorMatchesDF$SponsorClass == "none"])
for(ii in 1:length(forTest)) {
  tempdf <- sponsorMatchesDF[sponsorMatchesDF$Ref == forTest[ii],]
  charityCounts <- sum(tempdf$CharitySponsorObs)
  busCounts <- sum(tempdf$BusSponsorObs)
  UKGovCounts <- sum(tempdf$UKGovSponsorObs)
  if(charityCounts >= 0.5*nrow(tempdf) & busCounts < 0.5*nrow(tempdf) & UKGovCounts < 0.5*nrow(tempdf)) {
    sponsorMatchesDF$SponsorClass[sponsorMatchesDF$Ref == forTest[ii]] <- "charity"
  } else if(charityCounts < 0.5*nrow(tempdf) & busCounts >= 0.5*nrow(tempdf) & UKGovCounts < 0.5*nrow(tempdf)) {
    sponsorMatchesDF$SponsorClass[sponsorMatchesDF$Ref == forTest[ii]] <- "business"
  } else if(charityCounts < 0.5*nrow(tempdf) & busCounts < 0.5*nrow(tempdf) & UKGovCounts >= 0.5*nrow(tempdf)) {
    sponsorMatchesDF$SponsorClass[sponsorMatchesDF$Ref == forTest[ii]] <- "UKGov"
  }
}

#Add in AMRC sponsors
sponsorMatchesDF$AMRCSponsor <- rep(FALSE,nrow(sponsorMatchesDF))
sponsorMatchesDF$AMRCSponsor[sponsorMatchesDF$AMRCSponsorRef == TRUE & sponsorMatchesDF$SponsorClass == "charity"] <- TRUE

#If sponsor Ref for more than one we say conflicted
sponsorMatchesDF$SponsorClass[apply(sponsorMatchesDF[c('CharitySponsorRef','BusSponsorRef','UKGovSponsorRef')],1,sum) > 1] <- "conflicted"

#Load in and correct some spellings
refCorrection <- read.csv(paste0("Data/RefCorrections.csv"),stringsAsFactors = FALSE)
sponsorMatchesDF$Ref <- plyr::mapvalues(sponsorMatchesDF$Ref,refCorrection$Orginal,refCorrection$Correction)

#####Sponsor assignment####
#Create sponsor conversion vector
sponsorConversion <- sponsorMatchesDF$Ref
names(sponsorConversion) <- sponsorMatchesDF$Obs
default <- c("-1")
names(default) <- c("-1")
sponsorConversion <- c(sponsorConversion,default)
#And sponsor class conversions
sponsorClassConversion <- sponsorMatchesDF$SponsorClass
names(sponsorClassConversion) <- sponsorMatchesDF$Obs
default <- c("-1")
names(default) <- c("-1")
sponsorClassConversion <- c(sponsorClassConversion,default)
#Add AMRC class conversion
sponsorAMRCConversion <- sponsorMatchesDF$AMRCSponsor
names(sponsorAMRCConversion) <- sponsorMatchesDF$Obs
default <- c("-1")
names(default) <- c("-1")
sponsorAMRCConversion <- c(sponsorAMRCConversion,default)
#Need to split sponsors and assign columns
ethosdf$NoOfSponsor <- unlist(lapply(strsplit(ethosdf$sponsor,"\\,|\\, |\\ , |\\;|\\; |\\ ; |\\:|\\: |\\ : "),length))
sponsorSets <- strsplit(ethosdf$sponsor,"\\,|\\, |\\ , |\\;|\\; |\\ ; |\\:|\\: |\\ : ")

#Assigns each sponsor in dataframe
for(ii in 1:max(ethosdf$NoOfSponsor)) {
  #Set up new temp vector of sponsor names, -1 if absent or invalid
  temp <- rep("-1",nrow(ethosdf))
  temp[ethosdf$NoOfSponsor >= ii] <- wordClean(unlist(lapply(sponsorSets[ethosdf$NoOfSponsor >= ii], "[[", ii)),
                                                     removeNumeric = FALSE,minLength = 0)
  temp[nchar(temp) < 2 | is.na(temp)] <- "-1"
  #Determine sponsor ref and class
  tempRef <- sponsorConversion[temp]
  tempSponsorClass <- sponsorClassConversion[temp]
  tempAMRCClass <- sponsorAMRCConversion[temp]
  #Create new columns
  ethosdf[paste0("sponsor",ii,"Raw")] <- temp
  ethosdf[paste0("sponsor",ii)] <- tempRef
  ethosdf[paste0("sponsorClass",ii)] <- tempSponsorClass
  ethosdf[paste0("AMRCSponsor",ii)] <- tempAMRCClass
}

#Combine all sponsors and write to csv file
allsponsors <- cbind(ethosdf$sponsor1,ethosdf$sponsor2,ethosdf$sponsor3,ethosdf$sponsor4,
                     ethosdf$sponsor5,ethosdf$sponsor6,ethosdf$sponsor7,ethosdf$sponsor8,
                     ethosdf$sponsor9,ethosdf$sponsor10,ethosdf$sponsor11,ethosdf$sponsor12,
                     ethosdf$sponsor13,ethosdf$sponsor14,ethosdf$sponsor15)
allsponsors <- allsponsors[allsponsors != "-1"]
write.csv(as.data.frame(table(allsponsors)),file= "Data/SponsorsIndexed.csv", row.names = FALSE)

#Can now add total number of sponsor information
sponsorMatchesDF$TotalInIndex <- unlist(lapply(sponsorMatchesDF$Ref, function(x) sum(allsponsors == x,na.rm=TRUE)))
#Save sponsor match information for network analysis
write.csv(sponsorMatchesDF,file="Data/SponsorMatches.csv", row.names = FALSE)
save(sponsorMatchesDF,file="Data/SponsorMatches.RData")

#Calculate ones that have at least one sponsor of a certain class
#UK government sponsors
ethosdf$UKGovSponsor <- rep(FALSE,nrow(ethosdf))
ethosdf$UKGovSponsor[rowSums(ethosdf[,paste0("sponsorClass",1:max(ethosdf$NoOfSponsor))] == "UKGov") > 0] <- TRUE
#Charity sponsors
ethosdf$CharitySponsor <- rep(FALSE,nrow(ethosdf))
ethosdf$CharitySponsor[rowSums(ethosdf[,paste0("sponsorClass",1:max(ethosdf$NoOfSponsor))] == "charity") > 0] <- TRUE
#Business sponsors
ethosdf$BusSponsor <- rep(FALSE,nrow(ethosdf))
ethosdf$BusSponsor[rowSums(ethosdf[,paste0("sponsorClass",1:max(ethosdf$NoOfSponsor))] == "business") > 0] <- TRUE
#AMRC Sponsors
ethosdf$AMRCSponsor <- rep(FALSE,nrow(ethosdf))
ethosdf$AMRCSponsor[rowSums(ethosdf[,paste0("AMRCSponsor",1:max(ethosdf$NoOfSponsor))]==TRUE) > 0] <- TRUE

#Remove any which now don't have a sponsor
ethosdf <- ethosdf[ethosdf$sponsor1 != -1,]

#Export final dataframe
write.csv(ethosdf,file= "Data/SponsorinfoDFWithAnnotations.csv", row.names = FALSE)
save(ethosdf,file= "Data/SponsorinfoDFWithAnnotations.RData")
