##################################################################################
###  Function to spit out bibtex from conservation interventions/outcomes      ###
###  Last updated: May 10, 2016                                                ###
###  Authors: Ian McCullough and Julien Brun (ORCID: 0000-0002-7751-6238)      ###
###  Contributors: Justin Kroes and Kendall Miller                             ###
###  Contact: scicomp@nceas.ucsb.edu                                           ###
##################################################################################


#### Packages #### 
library(readr)
library(RefManageR)
library(dplyr)
library(stringr)
library(xtable)
library(bibtex)


#### CONSTANTS ####

# Working directory
setwd("/Users/brun/GitHubnceas/SNAPP/evidence-based-heatmap")

# Heatmap URL
SERVER_URL ="https://www.nceas.ucsb.edu/~brun/heatmap/"

# Input files with all the bibliographic information
data_final <- read_csv("map_doi_20160716.csv")


#### FUNCTIONS ####

#' Title
#'
#' @param dfl data frame
#' @param filename character
#' @param pagetitle character
#'
#' @return write html file
#' @export
#'
#' @examples htmlpage(my)dataframe, "my_biblio", "data skills")
htmlpage <- function(dfl,filename, pagetitle){
  # Write the html header
  html.head <- paste("<head>" ,
                     pagetitle,
                     '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">',
                     '<link rel="stylesheet" type="text/css" href="mystyle.css"/>', # hardwired css file name
                     "</head>",sep='\n')
  # Export the results as html table
  html.table <- print(xtable(select(dfl,References)), 
                      type="html", html.table.attributes="",
                      sanitize.text.function=function(x){x} # Solve the formatting issue with < >
                      )
  # Button to download file
  refdow <- paste0('<a href=', 
                   shQuote(paste0("biblio/",filename,".bib")),
                   ' download=',shQuote(paste0("biblio/",filename,".bib")),
                   '><button>Download</button></a>')
  # Create the htm body
  html.body <- paste("<body>",refdow, html.table ,"</body>",sep='\n')
  
  # Write the html file
  filehtml.name <- paste0(filename,".html")
  write(paste(html.head,html.body,sep='\n'),file=filehtml.name)
}



#' Title
#'
#' @param data.file data frame
#'
#' @return
#' @export
#'
#' @examples doiFunction(data.final)
doiFunction = function(data.file) {
  #data.file = data frame containing intervention, outcome data
  #DOI = data file with DOI references
  
  ### Join intervention and outcome data
  common_field = intersect(names(data.interv), names(data.outcome)) # allows function to identify common field name from the two data frames
  common_field = common_field[1] # takes and uses first common column name...in case there are multiple
  joined = full_join(data.outcome, data.interv, by=common_field) # full join retains all values and rows
  names(joined)
  out_names = colnames(dplyr::select(joined, contains('outcome', ignore.case=T))) # returns character vector with column names that contain word outcome
  out_names = grep('^Outcome$', x=out_names, ignore.case=F, value=F) #returns character string of only 'Outcome' column name
  joined$Outcome = as.factor(joined$Outcome) # convert outcomes to factor from character
  joined$Int_type = as.factor(joined$Int_type) # convert intervention types to factor from character
  interventions = levels(joined$Int_type)
  outcomes = levels(joined$Outcome)
  
  # extract only the columns we want
  aidintout <- joined %>% select(aid, Outcome, Int_type)
  
  # Create a unique list
  aidintout = unique(aidintout, incomparables = F)
  
  ### Join bibliographic data to intervention/outcome data
  DOI_title_join <- data.file %>% select(aid, Title, Pub_year, DOI ,Fullcitation)
  DOI_title_join <- unique(DOI_title_join)
  # create full reference column
  DOI_title_join$References = paste('<a href =',shQuote(paste0("http://dx.doi.org/",DOI_title_join$DOI)),'>',DOI_title_join$Fullcitation,'</a>')
  
  ### Join prior table with outcomes/interventions to table with DOIs
  common_field3 = intersect(names(aidintout), names(DOI_title_join))
  common_field3 = common_field3[1]
  aidintout2 = left_join(aidintout, DOI_title_join, by=common_field3)
  # aidintout2 = DOI_title_join
  
  # select only desired columns
  #doi_cols = grep(pattern='doi', x= names(aidintout), value= T, ignore.case=T)
  aidintout2 = dplyr::select(aidintout2, aid, Outcome, Int_type, Title, Pub_year, DOI, References, Fullcitation)
  aidintout2 = unique(aidintout2, incomparables = F)
  aidintout2 = na.omit(aidintout2)
  
  ### Assign numeric IDs to interventions and outcomes
  aidintout2$Outcome = as.factor(aidintout2$Outcome)
  aidintout2$Int_type = as.factor(aidintout2$Int_type)
  aidintout2$Outcome_num = as.numeric(aidintout2$Outcome)
  aidintout2$Intervention_num = as.numeric(aidintout2$Int_type)
  aidintout2$Int_out = paste(aidintout2$Outcome_num, aidintout2$Intervention_num,
                             sep=",", collapse=NULL)
  aidintout2$Int_out = as.factor(aidintout2$Int_out)
  aidintout2$Int_out_num = as.numeric(aidintout2$Int_out)
  aidintout3 = aidintout2[!(is.na(aidintout2$DOI) | aidintout2$DOI==""), ] ##NEED to be removed
  
  out_num = length(levels(aidintout3$Outcome)) # assign each outcome level a number
  hccol = seq(1,out_num,1) # create string of numbers from 1 to out_num by 1
  colLabel = levels(aidintout3$Outcome)
  df_out = data.frame(hccol, colLabel)
  int_num = length(levels(aidintout3$Int_type))
  hcrow = seq(1,int_num,1)
  rowLabel = levels(aidintout3$Int_type)
  df_int = data.frame(hcrow, rowLabel)
  
  ### create html pages for each intervention/outcome combination
  for (nout in hccol) {
    for(nint in hcrow) {
      # Create the dataframe for a specific combination of outcomes and interventions
      df <- filter(aidintout3, Outcome_num==nout & Intervention_num==nint) %>%
        arrange(desc(Pub_year)) %>%
        unique %>%
        dplyr::select(Outcome,Int_type,Outcome_num,Intervention_num,References,DOI,Int_out_num)
      
      # Create the filename
      filename.ref = paste0("biblio_",as.character(unique(df$Outcome_num)),as.character(unique(df$Intervention_num)))
      
      # Create the bibtex
      doi_chr = df$DOI 
      tryCatch(biblio.data <- GetBibEntryWithDOI(doi = doi_chr, temp.file = tempfile('doi.bib'), delete.file = T),
               error = function(err) {
                 print(err)
                 biblio.data <- NULL
                 return(biblio.data)
               }
      )
      
      
      ## Export the results as bibtex
      # Check if the DOI API returned results
      if(!is.null(biblio.data)) {
        write.bib(biblio.data,file=paste0(filename.ref,".bib"))
      }else{
        print(paste0("there was no references retuned for ",filename.ref))
      }
      
      ## Create the html page and save it
      # create the page title
      title = paste("<title>",unique(df$Int_type),"--",unique(df$Outcome),"</title>", sep=" ")
      htmlpage(df,filename.ref, title)
      
    }
  }
  
  #### Group the Biblio table by intervention-outcomes combination ####
  
  # Group the intervention - outcome combination and collapse them into one row and count the number of occurence
  x <- group_by(aidintout3, Outcome_num, Intervention_num) %>% 
    # summarise(DOI_link=paste(DOI_link,collapse=''),count=n_distinct(aid)) ## Uncomment to list the titles
    summarise(count=n_distinct(aid))
  # summarise(count=n_distinct(aid))
  
  # concatenate outcome and intervention columns into new column
  x$out_int = paste(x$Outcome_num, x$Intervention_num, sep=',')
  
  
  #### Create a data frame with a row for every possible intervention/outcome combination ####
  outcome_col = rep(seq(1,out_num),int_num)
  intervention_col = rep(seq(1,int_num),out_num)
  big_frame = data.frame(outcome_col=sort(outcome_col), intervention_col=intervention_col)
  #big_frame$countB = rep(0,length(big_frame))
  big_frame$out_int = paste(big_frame$outcome_col, big_frame$intervention_col, sep=',')
  
  big_join = full_join(big_frame,x, by='out_int')
  
  # replace NAs with zeros (meaning zero papers at that outcome/intervention intersection)
  big_join$count[is.na(big_join$count)] = 0
  
  #Build the link to the html reference page
  big_join$count_link <-paste('<a href =',
                              shQuote(paste0(SERVER_URL,
                                             "biblio_",
                                             as.character(big_join$outcome_col),
                                             as.character(big_join$intervention_col),
                                             ".html")),
                              'target="_blank">',
                              paste(big_join$count, "References",sep=" "),
                              '</a>')
  big_join$count_link[big_join$count==0] = "No References"
  
  # Rename the columns
  names(big_join)[names(big_join)=="outcome_col"] <- "Outcome_number"
  names(big_join)[names(big_join)=="intervention_col"] <- "Intervention_number"
  
  # Sort the data by outcome and intervention and select the needed fields
  big_join_clean<- big_join[with(big_join, order(Outcome_number, Intervention_number)), ] %>% select(out_int,Outcome_number,Intervention_number,count,count_link)
  
  # Write the file
  write.table(big_join_clean, file='join_to_the_world_Last.tsv', sep='\t', quote=F, row.names=F, fileEncoding = "UTF-8")
  
  print("All done")
}


#### MAIN ####

# Run the function
doiFunction(data.file = data.final)
 