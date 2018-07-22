library(rvest)
library(dplyr)
library(scales)
library(magrittr)
library(ggplot2)

# This Rscript can roughly be divided into the following sections:
  # 1) Small supporting functions that are used to collect specific sections from a webpage. Used by the webscraper.
  # 2) Webscraper with the following components (for loops):
        #2a) Scrape the list compose a list of books (and their urls). Example: 'https://www.goodreads.com/list/show/1127'
        #2b) Use the urls to visit the pages of each book on the list, collecting detailed information.
  # 3) Finally call the webscraper function, and all the reading lists scraped for this project. I've taken the liberty of 
  #    of commenting out all but the smallest list so running a demo doesn't take hours.


# Section 1: Supporting functions
find.description<-function(book.url){
  desc<-html_nodes(book.url,"#description span")%>%html_text()%>%max()
  return(desc)
}

find.page.length<-function(book.url){
  book_page<-html_nodes(book.url,'#details span+ span')[1]
  pageCount<-html_text(book_page)
  if(length(book_page)==0){
    pageCounts<-NA
  }
  else{
    pageCounts<-as.numeric(gsub(" pages","",pageCount))
  }
  return(as.numeric(pageCounts))
}

find.genre.tag<-function(book.url){
  genre<-html_nodes(book.url,'.elementList:nth-child(1) .left .bookPageGenreLink')
  if(length(genre)==0){
    genreVoted=NA
  }
  else{
    genreVoted<-gsub( " *<.*?> *", "", genre)
  }
  return(genreVoted)
}

check.for.awards<-function(book.url){
  award<-html_nodes(book.url,'.clear+ .clearFloats .infoBoxRowItem')
  if(length(award)==0){
    hasAward<-FALSE
  }
  else{
    hasAward<-TRUE
  }
  return(hasAward)
}

find.total.reviews<-function(book.url){
  if(length(html_nodes(book.url,'.votes'))){
    ratings<-html_text(html_nodes(book.url,'.votes'))
    ratings<-gsub("[[:space:]]", "", ratings)
    ratings<-gsub(",", "", ratings)%>%as.numeric()
  }
  else{
    ratings<-NA
  }
  return(ratings)
}

find.text.reviews<-function(book.url){
  if(length(html_nodes(book.url,'.count'))!=0){
    reviews<-html_text(html_nodes(book.url,'.count'))
    text_reviews<-as.numeric(regmatches(reviews, regexpr("[[:digit:]]+",reviews)))
  }
  else{
    text_reviews<-NA
  }
  return(text_reviews)
}

#Section 2: The main webscraping function


goodReads.webscrape<-function(listUrl){
  
  #Section 2a: Takes the url of a reading list, scrapes it. This collects urls, goodReads ID's, titles, and authors.
  #The urls are used by the second part of the scraper to visit each book's page, and collect additional info.
  
  
  List_Main<-read_html(listUrl)
  pages<-html_nodes(List_Main,'.pagination a')%>%html_text()
  listIndex<-as.numeric(pages[length(pages)-1])
  #Exception for short lists less than 100 books
  if(length(listIndex)==0){
    listIndex=1
  }
  guide<-list()
  data<-data.frame()
  for(i in 1:listIndex){
    ReadList<-read_html(paste(listUrl,"?page=",i,sep=''))
    
    #Titles, authors, and hyperlinks from browsing the given list.
    title<-html_nodes(ReadList,"div.leftContainer")%>%html_nodes("td")%>%html_nodes('.bookTitle span')%>%html_text()
    authors<-html_nodes(ReadList,"div.leftContainer")%>%html_nodes("td")%>%html_nodes('.authorName span')%>%html_text()
    hyper<-html_nodes(ReadList,"div.leftContainer")%>%html_nodes("td")%>%html_nodes('a.bookTitle')%>%html_attr("href")
    
    #Extract Goodreads book ID from url, which can be used as a key variable. Second gsub() is used to catch an occasional exception
    goodreadsID<-gsub(".*/book/show/\\s*|-.*", "", hyper)
    goodreadsID<-gsub("\\..*","",goodreadsID)%>%as.numeric()
    
    a<-cbind(goodreadsID,title,authors,hyper)
    data<-rbind(data,a)
    complete<-i/listIndex
    cat("Completion (Step 1): ",format(percent(complete),digits=4,justify="left"),"\n","Works Found: ",nrow(data),"\n")
  }
  
  cat("Step 1 Complete! Found ",nrow(data)," separate works in this list.", "\n")
  
  
  # Section 2b: Scrape the page of individual books in the list. This gets additional information: page lengths, 
  # the genre tag (with the most votes by readers), text summaries etc. 
  # This step is takes time! (Technically I could speed this up with the doParallel package and run it on multiple cores,
  # however I would not be able to display program progress updates. I decided I'd rather be able to keep close tabs
  # on how the program is doing.)
  
  badurls=0
  total_ratings<-rep(NA,nrow(data))
  book.descriptions<-rep(NA,nrow(data))
  pageCounts<-rep(NA,nrow(data))
  genreVoted<-rep(NA,nrow(data))
  hasAward<-rep(NA,nrow(data))
  average_rating<-rep(NA,nrow(data))
  total_ratings<-rep(NA,nrow(data))
  text_reviews<-rep(NA,nrow(data))
  proportion_text_reviews<-rep(NA,nrow(data))
  
  
  for(i in 1:nrow(data)){
    #The url of a book in the target list
    url<-paste('https://www.goodreads.com',data$hyper[i],sep='')
    go<-tryCatch(read_html(url),
                 error=function(c) 'stop')
    #So it doesn't crash if it is a bad url
    if(go!='stop'){
      #the html page of a book in the target list
      goodReads<-read_html(url)
      
      # Number of total reviews and proportion that are also text reviews
      text_reviews[i]<-find.text.reviews(goodReads)
      
      total_ratings[i]<-find.total.reviews(goodReads)
      
      proportion_text_reviews[i]<-(text_reviews[i]/total_ratings[i])
      
      #Get the summaries of each book
      book.descriptions[i]<-find.description(goodReads)
      
      
      #Average Rating
      if(length(html_nodes(goodReads,'.average'))!=0){
        average_rating[i]<-as.numeric(html_text(html_nodes(goodReads,'.average')))
      } else{
        average_rating[i]<-NA
      }
      
      hasAward[i]<-check.for.awards(goodReads)
      
      genreVoted[i]<-find.genre.tag(goodReads)
      
      pageCounts[i]<-find.page.length(goodReads)
    }
    else{
      badurls=badurls+1
    }
    complete<-(i/nrow(data))*100
    cat(paste("Completion: ",format(complete,digits=4,justify="left"),"%","       Title: ",substr(data$title[i],1,45),sep=''),'\n',
        "                       Last Captured: ",format(average_rating[i],width=3,justify="centre",nsmall=2),format(pageCounts[i],width=6,justify="centre"),format(hasAward[i],width=5,justify="centre"),format(genreVoted[i],width=15,justify="centre"),format(percent(proportion_text_reviews[i]),width=5,justify="right"),'\n')
  }
  newData<-cbind.data.frame(pageCounts,proportion_text_reviews,average_rating,genreVoted,hasAward,total_ratings,book.descriptions)
  Finished<-cbind(data,newData)
  return(Finished)}

#---------------------------------------------------------------------
#-----      Section 3: Actually webscraping some lists      ----------
#---------------------------------------------------------------------
# Usage is quite simple, with only one argument.
# Argument: url of a list on goodreads.com
# Output: returns a dataframe with information scraped from each work on the list. 

# Comments: The scraper isn't perfect, and appears to have issues scraping certain works. I have number of exceptions 
# to handle this, but it does still encounter issues.
# Sometimes this is expected: 
  # Audiobooks don't have page lengths
  # Lesser known works sometimes do not have a genre tag, since no one voted on a tag.
# Also be careful feeding very large lists into the function! I'd say ~2000 books is ideal, anything further and you
# may run into issues (and can expect to wait some time for it to finish).

#bestEpicFantasy<-goodReads.webscrape('https://www.goodreads.com/list/show/50.The_Best_Epic_Fantasy')

#excellentSpaceOpera<-goodReads.webscrape('https://www.goodreads.com/list/show/1127')

#bestScienceFiction<-goodReads.webscrape('https://www.goodreads.com/list/show/19341')

#apocalyptic<-goodReads.webscrape('https://www.goodreads.com/list/show/47')

#travel<-goodReads.webscrape('https://www.goodreads.com/list/show/633.Favourite_Travel_Books')

#bestScience<-goodReads.webscrape('https://www.goodreads.com/list/show/692.Best_Science_Books_Non_Fiction_Only')

