##################################################################################
###  Get the DOIs and full citations for peer reviewed litterature             ###
###  from a title (with potential typos) using the crossref API                ###
###  Last updated: May 10, 2016                                                ###
###  Authors: Julien Brun (ORCID: 0000-0002-7751-6238) and Ian McCullough      ###
###  Contributors: Justin Kroes and Kendall Miller                             ###
###  Contact: scicomp@nceas.ucsb.edu                                           ###
##################################################################################

#### Packages #### 
library(rcrossref)


#### FUNCTIONS ####

#' Query the CrossRef API to full citation assoricated with a DOI. Handles error.
#'
#' @param doiq A character.
#' @return Full citation
#' @examples
#' get_fullcitation("10.1659/0276-4741(2001)021[0208:trc]2.0.co;2")
get_fullcitation <- function(doiq) {
  tryCatch(citation <- cr_cn(dois = doiq, locale = Sys.getlocale("LC_CTYPE"), format = "text", style = "apa"),
           error = function(err) {
             print(err)
             citation <- ""
             return(citation)
           }
  )

  return(citation)
}


#' Query the CrossRef API to find the top 5 citations associated with a specific title. Handles error.
#'
#' @param titleq A character.
#' @return The full query result from the Crossref API
#' @examples
#' get_cr("Use of film for community conservation education in primate habitat countries")
get_cr <- function(titleq) {
  tryCatch(q <- cr_works(query = titleq, limit = 5, sort = "score"),
           error = function(err) {
             print(err)
             q <- ""
             return(q)
           })

           q <- data.frame(doi=q$data$DOI, title=q$data$title, score=as.numeric(q$data$score),
                           stringsAsFactors = F)
           return(q)
}



#' Query the CrossRef API to find the DOI associated with a specific title; repeat several times the query if failed waiting 5sec between trials
#'
#' @param title A character.
#' @param nattempts An integer
#' @return A vector containing: DOI, Referenced Title, Full Citation
#' @examples
#' getdoi("Use of film for community conservation education in primate habitat countries")
#' getdoi("Use of film for community conservation education in primate habitat countries", n=10)
getdoi <- function(title, nattempts = 5) {
  #this function uses the crossref API to query the DOI database using titles entered by the reviewer
  if (is.na(title)) {
    print("the title is not availaible")
    doi <- NA
    title.new <- NA
    fullCitation <-NA
  } else {
    # Query the API
    query <- get_cr(title)
    if (query == ""){
      j = 0
      while (j <= nattempts) {
        Sys.sleep(5)
        query <- get_cr(title)
        j = j+1
      }
    }
    print(query)
    if (query != ""){
      #Keep information only if the matching score is over 2.0
      if (query$score[1] > 2.0){  ## THRESHOLD, might want to try other values based on the whole dataset
        doi <- query$doi[1]
        print(doi)
        title.new <- query$title[1]
        #queryable_doi = strsplit(doi,"http://dx.doi.org/")[[1]][2] #IM: with cr_works, the returned DOI seems already to have http://dx.doi.org/ removed
        queryable_doi = doi #added by IM to make fullCitation line run
        #It seems sometimes the API is unresponsive -> squeezing 5 attemps, need to be improved
        fullCitation <- get_fullcitation(queryable_doi)
        #if the API retuned the 500 error, try 5 more time with 5sec pause in between
        if (is.null(fullCitation)) {
          fullCitation <- NA # prevent errors about zero-length inputs to if, below
        } else {
          tryCatch(
            {
              if (fullCitation == ""){
                i = 0
                while (i <= nattempts) {
                  Sys.sleep(5)
                  fullCitation <- get_fullcitation(queryable_doi)
                  i = i+1
                  }
                }
              },
              error = function(err) {
                print(err)
              },
            finally = {
              return(fullCitation)
            }
          )
        }
      } else {
        doi <- NA
        title.new <- NA
        fullCitation <- NA
        print("No doi was matched")
      }
    } else {
      doi <- NA
      title.new <- NA
      fullCitation <- NA
      print("No doi was matched")
    }
  }
  return(c(doi,title.new, fullCitation))
}
