######## Script to analyse the CBS Downloads for 2023 in the DANS Data Station SSH
####### INSTALL PACKAGES
install.packages("stringr") 

######## LOAD PACKAGES
library (tidyverse)
library (here)
library (skimr)
library(dplyr)
library(ggpubr)
library(stringr)
library(data.table)


##############################
# GET THE FILES 
##############################

#Dataset containing a list of datasets and their DOIs for which we want to know the download numbers
file <- "InputData.csv" 
CBSDatasets <- read.csv(file)
rm(file)

#Guestbook export containing all the actions (requests adn downloads/views etc) on our Data Station
file <- "Guestbook.csv" 
RawData <- read.csv(file)
rm(file)

##### CLEAN THE DATA
#Remove anything we know is from DANS (as we are curators of the data, these should not be counted)
Data <- RawData[!grepl('@dans.knaw.nl', RawData$Email),]

#Select the right year
DownloadEnView.AllYrs <- subset(Data, (Data$Type!='AccessRequest')) #Note that this now inlcudes downloads, explores and alls kinds of views
DownloadEnView.2023 <- DownloadEnView.AllYrs[grepl('2023', DownloadEnView.AllYrs$Date),] #to only select 2023 from the data

#Create an estimation of the downloads per dataset
Input <-DownloadEnView.2023

templist<-list()
j<-1
for (i in 1:(nrow(Input)-1))
{
  #if the next row is different, we are at the end of one dataset and need to store this row
  if (Input$Dataset.PID[i] != Input$Dataset.PID [i+1] | Input$Date [i] != Input$Date [i+1] | Input$User.Name [i] !=Input$User.Name [i+1]) {
    templist[[j]] <-Input[i,]  
    j<-j+1 }
  #specifically checking for the last dataset if it is different from the last listed in templist
  if(i==(nrow(Input)-1)) {
    if(Input$Dataset.PID[i+1] != templist[[j-1]]$Dataset.PID | Input$Date [i+1] != templist[[j-1]]$Date | Input$User.Name [i+1] !=templist[[j-1]]$User.Name){
      templist[[j]] <-Input[i+1,]
    } 
  }
}

Datasets.DV.2023<-do.call(rbind, templist)
#remove the columns with file information as it is may be confusing for this part. 
drop <- c("File.Name","File.Id", "File.PID")
Datasets.DV.2023 = Datasets.DV.2023[,!(names(Datasets.DV.2023) %in% drop)]

#Create an overview where we exclude downloads of the same Dataset from the same user (so we get an indication of unique users)
Datasets.UNIEKDV.2023 <-Datasets.DV.2023
#because I need to keep the rows were a user did not login (e.g. empty "email"), I want to ensure these receive a unique number (otherwise all empty email rows would be counted as one user)
Datasets.UNIEKDV.2023$unNum <- seq.int(nrow(Datasets.UNIEKDV.2023))

#Now I remove this number for all registered users (e.g. not empty "email"), so we set the value to 0 for all rows where the Email is not empty)
Datasets.UNIEKDV.2023$unNum[which(Datasets.UNIEKDV.2023$Email!="")] <- 0

#Now we can create a unique list based on rows having the same PID and the same Email and the same number (0), if the email is empty the number will be different. 
Datasets.UNIEKDV.2023 = unique(setDT(Datasets.UNIEKDV.2023), by = c("Dataset.PID", "Email", "unNum"));

#Create an overview of the number of downloads per DOI
DownloadPerDOI<-Datasets.DV.2023 %>% count(Dataset.PID)
UNIEKDownloadPerDOI <-Datasets.UNIEKDV.2023 %>% count(Dataset.PID) #Unieke gebruikers --> LET OP: ALLEEN JUISTE INFORMATIE VOOR RESTRICTED ACCESS, OPEN ACCESS GEBRUIKERS HOEVEN NIET IN TE LOGGEN!

#Sanity check - the two overviews should have the same lengths (same number of datasets)
if (nrow(DownloadPerDOI)!=nrow(UNIEKDownloadPerDOI)) {
  stop("Downloads Per DOI and UNIEK Downloads Per DOI dont seem to have the same lentgh. That should not happen, please check.")
}


#For each DOI in the CBS List, look up the number in DownloadPerDOI for that DOI
for (i in 1:nrow(CBSDatasets)){
 
  if (!is.null(CBSDatasets$DOI[i])){
    
  testDOI<-CBSDatasets$DOI[i]
  testDOI <- str_replace(testDOI, "https://doi.org/","doi:")
  
  if (!is.na(match(testDOI, DownloadPerDOI$Dataset.PID))){
    p=match(testDOI, DownloadPerDOI$Dataset.PID)
    
    #check that the DOI for UNIEKDownloadPerDOI at place p is the same
    q=match(testDOI, UNIEKDownloadPerDOI$Dataset.PID)
    if (q!=p){stop("The DOIs in UNIEKDownloadPerDOI and DownloadPerDOI are not listed in the same order. That should not happen, please check.")
      }
    rm(q)
    
    CBSDatasets$Count[i] <- DownloadPerDOI$n[p] #Write down the DOI
    CBSDatasets$CountUniek[i] <- UNIEKDownloadPerDOI$n[p] #Write down the DOI
  } else {
      CBSDatasets$Count[i]= 0
      CBSDatasets$CountUniek[i]=0
    }
    
  }
}

#save the file 
file <- "CBS-Datasets_Output.csv"
write.csv(CBSDatasets, file)
  
