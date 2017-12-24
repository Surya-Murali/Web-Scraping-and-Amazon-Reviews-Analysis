#Install pacman. It reduces typing in package management actions. The function names in pacman package follow the format p_xxx
install.packages("pacman")
install.packages("sentimentr")
library("pacman")
#p_load allows the user to load one or more packages as a substitute for the library function
pacman::p_load(XML, dplyr, stringr, rvest, audio)

analyseReviews <- function(prod_code, pages = 2){
  
  # Creating a trim function to remove all white spaces
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  #Amazon's URL is easy to build. Just concatenate the product code at the end
  url <- paste0("https://www.amazon.com/dp/", prod_code)
  #Read the HTML source of the URL and store it in doc
  doc <- read_html(url)

  #Obtain the product name. The product name is stored in the 'doc' html. '#productTitle' gives the product name
  #Also trim the spaces and new lines
  prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()

  #Now we have got the Product name!
  prod
  #Source funtion is used to parse Amazon html pages for data
  #The source has the amazon_scraper function in it
  #It parses the data and gives the following values - prod, title, author, date, ver.purchase, format, stars, comments and helpful for each product
  source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")
  
  #Initialising the reviews_all variable as NULL
  reviews_all <- NULL
  
  for(page_num in 1:pages){
    url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
    doc <- read_html(url)
    reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
    reviews_all <- rbind(reviews_all, cbind(prod, reviews))
  }
 
  #Applying the sentiment_by function to only the comments column of the reviews_all data using the 'with()' function
  #sent_agg <- sentiment_by(reviews_all$comments). Both perform the same function
  #sentiment_by() function returns the approximate sentiment of the reviews. It returns a datatable of element_id, sentence_id, word_count
  #standard deviation and average sentiment for each review
  sent_agg <- with(reviews_all, sentiment_by(comments))
  
  #Plot the histogram of the stars / ratings given by the customers
#(Rating is between 0 to 5)
#x <- hist(reviews_all$stars)
with(reviews_all, hist(stars))

#Plot the histogram of the average sentiment scores for the reviews
#y <- hist(reviews_all$ave_sentiment)
with(sent_agg, hist(ave_sentiment))

#Get the mean rating of the product
mean(reviews_all$stars)

#Get the mean sentiment score of the product
#Higher the sentiment score, more positive are the reviews
mean(sent_agg$ave_sentiment)

#Create a subset of the top 3 reviews as 'best_reviews' based on the sentiment score
best_reviews <- slice(reviews_all, top_n(sent_agg, 3, ave_sentiment)$element_id)
best_reviews

#Highlight the top 3 reviews by sentiment polarity (positive = green; negative = pink) as an html file.
highlight(sentiment_by(best_reviews$comments))
#with(best_reviews, sentiment_by(comments)) %>% highlight()

#Create a subset of the bottom 3 reviews as 'worst_reviews' based on the sentiment score
worst_reviews <- slice(reviews_all, top_n(sent_agg, 3, -ave_sentiment)$element_id)
worst_reviews

#Highlight the bottom 3 reviews by sentiment polarity (positive = green; negative = pink) as an html file.
highlight(sentiment_by(worst_reviews$comments))
#with(worst_reviews, sentiment_by(comments)) %>% highlight()
}

analyseReviews("B00AFTT7IW", 5)
