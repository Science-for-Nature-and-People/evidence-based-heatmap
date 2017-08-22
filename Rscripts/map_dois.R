############################################################################################
### Get the DOIs and full citations for peer reviewed litterature                        ###
### by the SNAPP working Group Evidence Based - https://www.nceas.ucsb.edu/portal/12696  ###
### Last updated: Oct 6, 2015                                                            ###
### Author: Julien Brun (ORCID: 0000-0002-7751-6238), sciomp@nceas.ucsb.edu             ###
### Copyrights: NCEAS, UCSB                                                              ###
############################################################################################

# Clear environment
rm(list = ls())

#Set the working directory
# setwd("/Users/brun/GitHubnceas/SNAPP/evidence-based-heatmap")
setwd("/Users/justin/Desktop/evidence")

##### Source the created function to ping the Crossref API and match a title with a doi ####
source("get_dois.R")

data_review <- "Sam_data/evidence_based_5_13_16.RData"


####### MAIN #######


#load the data to get the titles
load(data_review)

#get the unqiue titles
titles = unique(data.biblio$Title)
nb.titles <- length(titles)

#initilaize the dataframe to store the results
dois <- data.frame(Dois = character(),
                   Title.formatted = character(),
                   Fullcitation = character(),
                   Title.reviewer = character(),
                   Title.identical = logical(),
                   stringsAsFactors=FALSE)

#loop throught the titles
for (i in 1:nb.titles ){
  title = titles[i]
  #year = data.biblio[data.biblio$Titl3==title,data.biblio$]
  print(title)
  #Query the API
  gotback <- getdoi(title)
  #Store the results
  newRow <- c(gotback,title,identical(gotback[2],title))
  #print(newRow)
  dois[i,] <- newRow
  print(dois)
}


#write a csv
write.table(dois, file = "dois_last_newgetdois.csv", sep=";",col.names = TRUE, row.names =F)
#write.csv(dois, file = "dois_last.csv", row.names =F,fileEncoding = "latin1")
#test <- read.table(file = "dois_last.csv", sep="\t",col.names = TRUE, fileEncoding = "latin1")
#### END ####