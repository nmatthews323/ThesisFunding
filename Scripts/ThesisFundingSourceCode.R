#Source functions for thesis funding project
#Author: Nick Matthews
#Date: 23/02/19



#Wordclean function - this function using tm functions to clean up text
wordClean <- function(wordset, needUnique = FALSE,removeBracketed = TRUE, stripHTML = TRUE, replaceAmp = TRUE,
                      lowercase = TRUE,minLength=2,stopwords = TRUE,removeNumeric = FALSE) {
  require(tm)
  
  #Strip HTML tags
  if(stripHTML == TRUE) {
    wordset <- gsub("<.*?>", "", wordset)
  }
  #Strip out anything after a bracket - do this twice
  if(removeBracketed == TRUE) {
    wordset <- sapply(strsplit(wordset,"\\(|\\["), function(x) x[1])
    #Remove any left with brackets
    wordset <- gsub("\\(|\\)|\\[|\\]","",wordset)
  }
  #Replace & with and
  if(replaceAmp == TRUE) {
    wordset <- gsub("&","and",wordset)
  }
  #All lowercase
  if(lowercase == TRUE) {
    wordset <- tolower(wordset)
  }
  #Trim whitespace
  wordset <- trimws(wordset)
  #Remove stopwords
  if(stopwords == TRUE) {
    wordset <- gsub(paste0("\\<",stopwords(),"\\>", collapse = "|"), "", wordset)
  }
  #Remove punctuation
  wordset <- removePunctuation(wordset,preserve_intra_word_dashes =TRUE)
  #Remove multiple spaces
  wordset <- gsub("\\s+", " ", wordset)
  #Need to trim whitespace again
  wordset <- trimws(wordset)
  #Remove any shorter than certain length (default 2 or less characters)
  wordset <- wordset[nchar(wordset)>=minLength]
  #Remove purely numeric
  if(removeNumeric == TRUE) {
    wordset <- removeNumbers(wordset)
  }
  #Unique, if required
  if(needUnique == TRUE) {
    wordset = unique(wordset)
  }
  wordset
}

#Function classifies DDC assignments into discrete subject classes
subjectClassify <- function(df,type="DDC",DDCcol="DDC") {
  #Ensure DDC classification is in numeric form
  DDC <- as.numeric(df[,DDCcol])
  #Do assignment
  if(type == "DDC") {
    df$subjectClass <- rep("none", length(DDC))
    df$subjectClass[DDC >= 0 & DDC < 100] <- "Computer science, information & general works"
    df$subjectClass[DDC >= 100 & DDC < 200] <- "Philosophy & psychology"
    df$subjectClass[DDC >= 200 & DDC < 300] <- "Religion"
    df$subjectClass[DDC >= 300 & DDC < 400] <- "Social sciences"
    df$subjectClass[DDC >= 400 & DDC < 500] <- "Language"
    df$subjectClass[DDC >= 500 & DDC < 600] <- "Science"
    df$subjectClass[DDC >= 600 & DDC < 700] <- "Technology"
    df$subjectClass[DDC >= 700 & DDC < 800] <- "Arts & recreation"
    df$subjectClass[DDC >= 800 & DDC < 900] <- "Literature"
    df$subjectClass[DDC >= 900 & DDC < 1000] <- "History & geography"
  } else if(type == "bespoke") {
    df$subjectClass <- rep("none", length(DDC))
    df$subjectClass[DDC >= 0 & DDC < 100] <- "Technology & Management"
    df$subjectClass[DDC >= 100 & DDC < 200] <- "Arts & Humanities"
    df$subjectClass[DDC >= 200 & DDC < 300] <- "Arts & Humanities"
    df$subjectClass[DDC >= 300 & DDC < 400] <- "Arts & Humanities"
    df$subjectClass[DDC >= 400 & DDC < 500] <- "Arts & Humanities"
    df$subjectClass[DDC >= 500 & DDC < 600] <- "Science"
    df$subjectClass[DDC >= 600 & DDC < 700] <- "Technology & Management"
    df$subjectClass[DDC >= 700 & DDC < 800] <- "Arts & Humanities"
    df$subjectClass[DDC >= 800 & DDC < 900] <- "Arts & Humanities"
    df$subjectClass[DDC >= 900 & DDC < 1000] <- "Arts & Humanities"
    df$subjectClass[DDC >= 610 & DDC < 620] <- "Medical"
  } else {
    break("No recognised classification schema")
  }
  return(df) 
}

extractAbbreviations <- function(Sponsors, removeSet=c(""),addSet=c()) {
  require(ngram)
  #Extract all bracketted words as candidate abbreviations
  bracketSet <- sapply(strsplit(Sponsors,"\\(|\\["), function(x) x[2])
  bracketSet <- bracketSet[!is.na(bracketSet)]
  bracketSet <- gsub("\\)|\\]","",bracketSet)
  bracketSet <- wordClean(bracketSet)
  #Remove any that aren't between 4 and 10 letters in length
  bracketSet <- bracketSet[lapply(bracketSet,wordcount)==1 & nchar(bracketSet) > 2 & nchar(bracketSet) < 8]
  #Create summary table
  bracketSetTable <- as.data.frame(table(bracketSet))
  #Extract most common
  commonBracketSet <- as.character(bracketSetTable$bracketSet[bracketSetTable$Freq >10])
  
  #Extract single word sponsors of 3-9 characters
  ExistingSponsorAbbrev <- cleanSponsors[lapply(cleanSponsors,wordcount)==1 & nchar(cleanSponsors) > 2 & nchar(cleanSponsors) < 8]
  existingAbbrev <- as.data.frame(table(ExistingSponsorAbbrev))
  commonExistingAbrev <- as.character(existingAbbrev$ExistingSponsorAbbrev[existingAbbrev$Freq >5])
  
  #Merge the two
  abbreviations <- unique(c(commonBracketSet,commonExistingAbrev))
  
  #Remove set of false abbreviations
  abbreviations <- abbreviations[!abbreviations %in% removeSet]
  #And add any necessary
  abbreviations <- unique(c(abbreviations, addSet))
  #Return abbreviations
  return(abbreviations)
  
}

sponsorProcess <- function(Sponsors, abbreviations,problemTerms,manualmerges) {
  #Extract unique sponsors
  UniqueSponsors <- wordClean(Sponsors,needUnique = TRUE)
  #Sort by length
  UniqueSponsors <- UniqueSponsors[order(nchar(UniqueSponsors))]
  #Extract candidates for merging
  candidates <- nchar(UniqueSponsors) > 10 & lapply(UniqueSponsors,wordcount) > 1 & !(UniqueSponsors %in% problemTerms)
  UniqueSponsorsCandidates <- UniqueSponsors[candidates]
  sponsorMatches <- lapply(UniqueSponsorsCandidates, function(x) agrep(x,UniqueSponsors, ignore.case = TRUE,max.distance = 0.05))
  #Make dataframe with list items
  sponsorMatchesDF <- data.frame(Index = unlist(sponsorMatches),
                                 Obs = UniqueSponsors[unlist(sponsorMatches)],
                                 Ref = rep(UniqueSponsors[unlist(lapply(sponsorMatches, "[[", 1))],sapply(sponsorMatches, length)),
                                 stringsAsFactors = FALSE)
  uniqueSponsorMatches <- sponsorMatchesDF[!duplicated(sponsorMatchesDF$Index),]
  #Now add in all the other sponsors not merged
  allIndices <- 1:length(UniqueSponsors)
  notIncluded <- allIndices[!(1:length(UniqueSponsors) %in% uniqueSponsorMatches$Index)]
  sponsorMatchesDF2 <- data.frame(Index = notIncluded,
                                  Obs = UniqueSponsors[notIncluded],
                                  Ref = UniqueSponsors[notIncluded],
                                  stringsAsFactors = FALSE)
  #Complete reference dataframe through a merge
  sponsorMatchesComb <- rbind(sponsorMatchesDF2,uniqueSponsorMatches)
  sponsorMatchesComb <- sponsorMatchesComb[!is.na(sponsorMatchesComb$Obs),]
  #Add in abbreviations column
  sponsorMatchesComb$Abbreviation <- rep("none",nrow(sponsorMatchesComb))
  sponsorMatchesComb$AbbreviationOriginal <- rep("none",nrow(sponsorMatchesComb))
  sponsorMatchesComb$existingAbbrev <- rep("none",nrow(sponsorMatchesComb))
  sponsorMatchesComb$existingAbbrevOriginal <- rep("none",nrow(sponsorMatchesComb))
  #abbreviate selected matches
  forAbbreviation <- lapply(sponsorMatchesComb$Ref,wordcount) > 2 & lapply(sponsorMatchesComb$Ref,wordcount) < 8
  forAbbreviationOriginal <- lapply(sponsorMatchesComb$Obs,wordcount) > 2 & lapply(sponsorMatchesComb$Obs,wordcount) < 8
  existingAbbrevs <- lapply(sponsorMatchesComb$Ref,wordcount)==1 & nchar(sponsorMatchesComb$Ref) > 2 & nchar(sponsorMatchesComb$Ref) < 8
  existingAbbrevs[is.na(existingAbbrevs)] <- FALSE
  existingAbbrevsOriginal <- lapply(sponsorMatchesComb$Obs,wordcount)==1 & nchar(sponsorMatchesComb$Obs) > 2 & nchar(sponsorMatchesComb$Obs) < 8 
  existingAbbrevsOriginal[is.na(existingAbbrevsOriginal)] <- FALSE
  #Extract common abbreviations
  sponsorMatchesComb$Abbreviation[forAbbreviation] <- abbreviate(sponsorMatchesComb$Ref[forAbbreviation],strict = TRUE,minlength = 3)
  sponsorMatchesComb$AbbreviationOriginal[forAbbreviationOriginal] <- abbreviate(sponsorMatchesComb$Obs[forAbbreviationOriginal],strict = TRUE,minlength = 3)
  sponsorMatchesComb$existingAbbrev[existingAbbrevs] <- sponsorMatchesComb$Ref[existingAbbrevs]
  sponsorMatchesComb$existingAbbrevOriginal[existingAbbrevsOriginal] <- sponsorMatchesComb$Obs[existingAbbrevsOriginal]
  
  #Now test if match to abbreviations fed into function
  sponsorMatchesComb$abbrevMatch <- rep("none",nrow(sponsorMatchesComb))
  for(abbrev in abbreviations) {
    #Assign matches
    sponsorMatchesComb$abbrevMatch[sponsorMatchesComb$Abbreviation == abbrev |
                                     sponsorMatchesComb$AbbreviationOriginal == abbrev |
                                     sponsorMatchesComb$existingAbbrev == abbrev |
                                     sponsorMatchesComb$existingAbbrevOriginal == abbrev] <- abbrev
  }
  
  #Special merges for misspellings (e.g. epsrc)
  for(ii in 1:length(manualmerges)){
    sponsorMatchesComb$abbrevMatch[sponsorMatchesComb$abbrevMatch %in% manualmerges[[ii]]] <- names(manualmerges)[ii]
  }
  #Edit entries where abbreviations have been found and edit to new entry
  sponsorMatchesComb$RefNew <- sponsorMatchesComb$Ref
  for(abbrev in unique(sponsorMatchesComb$abbrevMatch[sponsorMatchesComb$abbrevMatch != "none"])) {
    abbrevMatch <- sponsorMatchesComb$abbrevMatch == abbrev
    sponsorMatchesComb$RefNew[abbrevMatch] <- sponsorMatchesComb$Ref[abbrevMatch][which.max(nchar(sponsorMatchesComb$Ref[abbrevMatch]))]
  }
  
  #Clean up table
  sponsorMatchesComb$Ref <- sponsorMatchesComb$RefNew
  sponsorMatchesFinal <- sponsorMatchesComb[,c("Index","Obs","Ref")]
  #Add in ID column
  sponsorMatchesFinal <- sponsorMatchesFinal[order(sponsorMatchesFinal$Ref),]
  sponsorMatchesFinal$SponsorID <- rep.int(1:length(unique(sponsorMatchesFinal$Ref)), as.numeric(table(sponsorMatchesFinal$Ref)))
  
  #Return output
  return(sponsorMatchesFinal)
}

