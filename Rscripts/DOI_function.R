## Create function to spit out bibtex from conservation interventions/outcomes
# Date: 12-8-15
# Author: Ian McCullough, immccull@gmail.com

### THIS IS NOT FINISHED ###
library(readr)
library(RefManageR)
library(dplyr)
library(stringr)
library(xtable)
library(bibtex)


# Ian's working directory
#setwd("~/git_redmine/snap_interns/EvidenceBased")
# setwd("/Users/brun/redmine_git/snap_interns/EvidenceBased")
setwd("/Users/brun/GitHubnceas/SNAPP/evidence-based-heatmap")

#### Constant ####
# SERVER_URL ="https://www.nceas.ucsb.edu/~mccullough/"
SERVER_URL ="https://www.nceas.ucsb.edu/~brun/heatmap/"


# This function could work (for example)
# DOI = read.csv('dois5.csv')
# # data = load('evidence_based4.RData')
# data = load('Sam_data/evidence_based_5_13_16.RData')

# data_final = readRDS("/Users/brun/GitHubnceas/SNAPP/visualizations/beta_tool/data/map_data_final.rds")
data_final <- read_csv("map_doi_20160716.csv")
# data_final <- read.table("test.csv", sep = ";", stringsAsFactors = F, header=T, fileEncoding = "macroman")
# data_final <- all_data_last
#### FUNCTIONS ####
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
  # refdow <- paste0('<form method="get" action=',shQuote(paste0("biblio/",filename,".bib")),'><button type="submit">Download</button></form>')
  refdow <- paste0('<a href=',shQuote(paste0("biblio/",filename,".bib")),' download=',shQuote(paste0("biblio/",filename,".bib")),'><button>Download</button></a>')
  #<a href="biblio/biblio_64.bib" download="biblio_64.bib"><button>Download</button></a>
  #<a href='biblio/biblio_1019.bib' download='biblio_1019.bib'><button>Download</button></a>
  # Create the htm body
  html.body <- paste("<body>",refdow, html.table ,"</body>",sep='\n')
  
  # Write the html file
  filehtml.name <- paste0(filename,".html")
  write(paste(html.head,html.body,sep='\n'),file=filehtml.name)
}



doiFunction = function(data.file, DOI) {
  #data.file = data frame containing intervention, outcome data
  #DOI = data file with DOI references
  
  ### Join intervention and outcome data
#   common_field = intersect(names(data.interv), names(data.outcome)) # allows function to identify common field name from the two data frames
#   common_field = common_field[1] # takes and uses first common column name...in case there are multiple
#   joined = full_join(data.outcome, data.interv, by=common_field) # full join retains all values and rows
#   #names(joined)
  #out_names = colnames(dplyr::select(joined, contains('outcome', ignore.case=T))) # returns character vector with column names that contain word outcome
  #out_names = grep('^Outcome$', x=out_names, ignore.case=F, value=F) #returns character string of only 'Outcome' column name
  # joined$Outcome = as.factor(joined$Outcome) # convert outcomes to factor from character
  # joined$Int_type = as.factor(joined$Int_type) # convert intervention types to factor from character
  # in previous two lines, what if column names are different???
  #interventions = levels(joined$Int_type)
  #outcomes = levels(joined$Outcome)
  # extract only the columns we want
  aidintout <- data.file %>% select(aid, Outcome, Int_type)
  # Remove duplicates
  aidintout = unique(aidintout, incomparables = F)
  
  ### Join bibliographic data to DOI csv file
#   names(data.biblio)
#   names(DOI)
#   DOI = na.omit(DOI)
#   DOI$Title = DOI$Title.reviewer #need to build in capability to recognize lack of column name match and add a new column, if needed
#   common_field2 = intersect(names(data.biblio), names(DOI)) #may find more than one match
#   #common_field2 = grep(pattern = 'title', x = common_field2, value = T, ignore.case = T)
#   common_field2 = common_field2[1]
#   DOI_title_join = full_join(DOI, data.biblio, by=common_field2)
#   DOI_title_join = unique(DOI_title_join, incomparables = F)
  
  ### Join bibliographic data to intervention/outcome data
  ### Find DOI column name in DOI datasets
  # names(DOI_title_join)
  #DOI_col = subset(DOI_title_join, select=grep(pattern='DOI', x=colnames(DOI_title_join),
  #                                             value=T, ignore.case = T)) #subsets DF based on case insensitive column name pattern recognition
  # DOI_title_join = DOI_title_join[!sapply(DOI_title_join, function(x) all(x == ""))] #remove blank columns
  # convert all columns in character
  #DOI_col = data.frame(lapply(DOI_col, as.character), stringsAsFactors = F)
  # convert one column to character
  # DOI_title_join$Dois = as.character(DOI_title_join$Dois)
  
  DOI_title_join <- data.file %>% select(aid, Title, Pub_year, DOI ,Fullcitation)
  DOI_title_join <- unique(DOI_title_join)
  # create full reference column
  DOI_title_join$References = paste('<a href =',shQuote(DOI_title_join$DOI),'>',DOI_title_join$Fullcitation,'</a>')
  
  ### Remove http://dx.doi.org/ (messes up GetBibEntryWithDOI function)
  pp = str_split_fixed(DOI_title_join$DOI, "http://dx.doi.org/", 2)
  pp = pp[,2] #eliminate first  column, which is just ""
  DOI_title_join$DOI = pp
  
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
  
  ###12-15-15: joining part of function is (seemingly) done...next step is to run loop
  
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
      
      # Create the bibtext
      #df = df[!(is.na(df$DOI) | df$DOI==""), ]
      doi_chr = df$DOI 
      # biblio.data <- GetBibEntryWithDOI(doi = doi_chr, temp.file = tempfile('doi.bib'), delete.file = T)
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
  # replace NAs in DOI column with "No References"
  # big_join$DOI_link[is.na(big_join$DOI_link)] = "No References"
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
  #Write the file
  write.table(big_join_clean, file='join_to_the_world_Last.tsv', sep='\t', quote=F, row.names=F, fileEncoding = "UTF-8")
  
  print("All done")
}


#### MAIN ####
# Run the function
doiFunction(data.file = data_final, DOI = DOI)
 