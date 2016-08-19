#Setting a working directory. Working directory is folder where all data files are located
setwd("C:/Users/SANTOSH/Desktop/Loan Data Management")

# Extracting the excel files
AdjustedData<- read.csv("C:/Users/SANTOSH/Desktop/Loan Data Management/Adjusted Data.csv", header = TRUE)
LeadSyndicates<- read.csv("C:/Users/SANTOSH/Desktop/Loan Data Management/Lead Syndicates 93-2015.csv", header = TRUE)
syndicateraw<- read.csv("C:/Users/SANTOSH/Desktop/Loan Data Management/syndicateraw.csv", header = TRUE)

# This is numcov calculation
df2<- data.frame(syndicateraw$CovenantDescription)
names(df2)<- c("x")
df2[] <- lapply(df2, as.character)
df2$x[df2$x == "-"]<- ""
numcov <- data.frame(
  # iterate over columns
  lapply(df2, function(y){
    # iterate over rows
    unname(sapply(y, function(x){
      # split at linebreak and get vector length
      length(strsplit(x, "\n")[[1]])
    }))
  })
)

# Formating Data and Correcting Data, particularly dates "andate"
LeadAgent<- as.character(syndicateraw$LeadAgents)
names(LeadAgent)<- c("LeadAgent")
yrs <- basename(as.character(syndicateraw$andate))
yrs <- ifelse(nchar(yrs) == 2, format(as.Date(yrs, format = "%y"), "%Y"), yrs)
Year<- data.frame(yrs)
names(Year)<- c("Year")
LeadAgentData <- cbind(Year, LeadAgent)

#Formating LeadSyndicates data
names(LeadSyndicates)<- c("Rank", "Arrangers", "YearAmt", "Mkt.Share", "NumberofIssues")
i1 <- LeadSyndicates$Rank == ''
newLeadSyndicates <- transform(LeadSyndicates, Year = YearAmt[i1][cumsum(i1)])[!i1, ]
row.names(newLeadSyndicates) <- NULL
LeadSyndicateData <- subset(newLeadSyndicates, Rank != "Rank")
LeadSyndicateData <- LeadSyndicateData[1:558, ]
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
LeadSyndicateData$Year <-  trim(LeadSyndicateData$Year)
LeadSyndicateData <- subset(LeadSyndicateData, Year != "2016")

#Data Formatting, Fuzzy matching and calculation of leadrep
LeadAgentData$LeadAgent<- as.character(LeadAgentData$LeadAgent)
LeadAgentList<- strsplit(LeadAgentData$LeadAgent, "/")
LeadAgentData$LeadAgent<- sapply(LeadAgentList, tolower)
LeadSyndicateData$Arrangers<- sapply(LeadSyndicateData$Arrangers, tolower)
LeadSyndicateData[] <- lapply(LeadSyndicateData, as.character)
ind <- sapply(LeadAgentData, is.factor)
LeadAgentData[ind] <- lapply(LeadAgentData[ind], as.character)

#This is fuzzy matching
library(stringdist)
rankrep <- vector("list", length(LeadAgentData$LeadAgent))
MktShareList <- vector("list", length(LeadAgentData$LeadAgent))
IssuesList <- vector("list", length(LeadAgentData$LeadAgent))

for (i in 1:length(LeadAgentData$LeadAgent)) {
  
  for (j in 1:length(LeadSyndicateData$Arrangers)) {
    
    for (k in 1:length(LeadAgentList[[i]])) {
      
      if ((ain(LeadSyndicateData$Arrangers[j], LeadAgentData$LeadAgent[[i]][k], maxDist=0.17, 
               method="jw")) == "TRUE" & (LeadAgentData$Year[[i]] == LeadSyndicateData$Year[j]) &
            (as.numeric(gsub("[*]","",LeadSyndicateData$Rank[j])) <= 10)){             
        
        rankrep[[i]][k]<- LeadSyndicateData$Rank[j] 
        MktShareList[[i]][k]<- LeadSyndicateData$Mkt.Share[j]
        IssuesList[[i]][k]<- LeadSyndicateData$NumberofIssues[j]
        
      }
    }
    
  }
}

# Calculation of leadrep5, leadrep10, leadrepMktShare etc..
leadrepResult<- data.frame(cbind(rankrep, MktShareList, IssuesList))

ListMktShare <- lapply(leadrepResult$MktShareList, function(x) x[!is.na(x)])
ListofIssues <- lapply(leadrepResult$IssuesList, function(x) x[!is.na(x)])
ListofRank <- lapply(leadrepResult$rankrep, function(x) x[!is.na(x)])

RankList <-  lapply(ListofRank, as.numeric)
MktShareList <- lapply(ListMktShare, as.numeric)
IssuesList <- lapply(ListofIssues, as.numeric)

leadrep5 <- matrix(ncol=6, nrow=length(LeadAgentData$LeadAgent))
for (i in 1:length(LeadAgentData$LeadAgent)) {
  leadrep5[i, 1] <- length(which(ListofRank[[i]] <= 5))
  leadrep5[i, 2] <- sum(MktShareList[[i]][which(RankList[[i]] <= 5)])
  leadrep5[i, 3] <- sum(IssuesList[[i]][which(RankList[[i]] <= 5)])
  leadrep5[i, 4] <- length(which(RankList[[i]] <= 10))
  leadrep5[i, 5] <- sum(MktShareList[[i]][which(RankList[[i]] <= 10)])
  leadrep5[i, 6] <- sum(IssuesList[[i]][which(RankList[[i]] <= 10)])
}

LeadRep <- data.frame(leadrep5)
names(LeadRep) <- c("leadrep5", "leadrep5mktshrare", "leadrep5num", 
                    "leadrep10", "leadrep10mktshrare", "leadrep10num")

names(numcov) <- c("numcov")
LoanDataManagement <- cbind(numcov, LeadRep)

write.csv(LoanDataManagement, 'LoanDataManagement.csv')

