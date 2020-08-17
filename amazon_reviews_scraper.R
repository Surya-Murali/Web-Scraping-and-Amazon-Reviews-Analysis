#Install pacman. It reduces typing in package management actions. The function names in pacman package follow the format p_xxx
install.packages("pacman")
library("pacman")
#p_load allows the user to load one or more packages as a substitute for the library function
pacman::p_load(XML, dplyr, stringr, rvest, audio)

# Creating a trim function to remove all white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Initialising the product code of the product you want to examine 
#View the product in Amazon, get the product code from its URL. It looks something like this: B073HCFZM7
prod_code = "B075QMZH2L"

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
# source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")


#Parse Amazon html pages for data
amazon_scraper <- function(doc, reviewer = T, delay = 0){
  
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load_gh("trinker/sentimentr")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
  
  sec = 0
  if(delay < 0) warning("delay was less than 0: set to 0")
  if(delay > 0) sec = max(0, delay + runif(1, -1, 1))
  
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  # author <- doc %>%
  #  html_nodes(".review-byline .author") %>%
  #  html_text()
  
  author <- doc %>%
    html_nodes("#cm_cr-review_list .a-profile-name") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  ver.purchase <- doc%>%
    html_nodes(".review-data.a-spacing-mini") %>%
    html_text() %>%
    grepl("Verified Purchase", .) %>%
    as.numeric()
  
  format <- doc %>% 
    html_nodes(".review-data.a-spacing-mini") %>% 
    html_text() %>%
    gsub("Color: |\\|.*|Verified.*", "", .)
  #if(length(format) == 0) format <- NA
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  #helpful <- doc %>%
  #  html_nodes(".cr-vote-buttons .a-color-secondary") %>%
  #  html_text() %>%
  #  str_extract("[:digit:]+|One") %>%
  #  gsub("One", "1", .) %>%
  #  as.numeric()
  
  # Helpful votes number cannot be extracted from reviews with no helpful votes cast
  helpful <- doc %>%
    html_nodes("#cm_cr-review_list  .cr-vote-text") %>%
    html_text() %>%
    str_extract("[:digit:]+|One") %>%
    gsub("One", "1", .) %>%
    as.numeric() 
  
  if(reviewer == T){
    
    rver_url <- doc %>%
      html_nodes(".review-byline .author") %>%
      html_attr("href") %>%
      gsub("/ref=cm_cr_othr_d_pdp\\?ie=UTF8", "", .) %>%
      gsub("/gp/pdp/profile/", "", .) %>%
      paste0("https://www.amazon.com/gp/cdp/member-reviews/",.) 
    
    #average rating of past 10 reviews
    rver_avgrating_10 <- rver_url %>%
      sapply(., function(x) {
        read_html(x) %>%
          html_nodes(".small span img") %>%
          html_attr("title") %>%
          gsub("out of.*|stars", "", .) %>%
          as.numeric() %>%
          mean(na.rm = T)
      }) %>% as.numeric()
    
    rver_prof <- rver_url %>%
      sapply(., function(x) 
        read_html(x) %>%
          html_nodes("div.small, td td td .tiny") %>%
          html_text()
      )
    
    rver_numrev <- rver_prof %>%
      lapply(., function(x)
        gsub("\n  Customer Reviews: |\n", "", x[1])
      ) %>% as.numeric()
    
    rver_numhelpful <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Helpful Votes:|\n", "", x[2]) %>%
          trim()
      ) %>% as.numeric()
    
    rver_rank <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Top Reviewer Ranking:|Helpful Votes:.*|\n", "", x[2]) %>%
          removePunctuation() %>%
          trim()
      ) %>% as.numeric()
    
    df <- data.frame(title, date, ver.purchase, format, stars, comments, helpful,
                     rver_url, rver_avgrating_10, rver_numrev, rver_numhelpful, rver_rank, stringsAsFactors = F)
    
  } #else df <- data.frame(title, author, date, ver.purchase, format, stars, comments, helpful, stringsAsFactors = F)
    # Removing 'author', 'helpful' from the dataframe. (Resolving the open issue: Error in data.frame(title, author, date, ver.purchase, format, stars,  arguments imply differing number of rows) 
    # Amazon.com had changed the HTML code of the reviews page. Due to different HTML nodes, this script to extract author, and helpful votes did not work
    # I added the right HTML tags in the code, but still removed them from the data frame
    else df <- data.frame(title, date, ver.purchase, format, stars, comments, stringsAsFactors = F)
  
  return(df)
}


#Give the number of pages of reviews you want to extract 
pages <- 10

#Initialising the reviews_all variable as NULL
reviews_all <- NULL

#Extracting the first ten pages of reviews and storing it in 'reviews_all' list variable
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc <- read_html(url)  
  reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}

#Check the structure of 'reviews_all' list
str(reviews_all)
#Reviews:
reviews_all$comments

#Now we have obtained the first 10 pages of reviews
#We will do a sentimental analysis of these reviews/comments. Sentiment analysis is a Natural Language Processing method

#Load the trinker & sentimentr package using pacman / library function
pacman::p_load_gh("trinker/sentimentr")
#install.packages("sentimentr")
library("sentimentr")

#Applying the sentiment_by function to only the comments column of the reviews_all data using the 'with()' function
#sent_agg <- sentiment_by(reviews_all$comments). Both perform the same function
#sentiment_by() function returns the approximate sentiment of the reviews. It returns a datatable of element_id, sentence_id, word_count
#standard deviation and average sentiment for each review
sent_agg <- with(reviews_all, sentiment_by(comments))
#Print the first 6 rows of sent_agg
head(sent_agg)

#Order them based on the sentiment score
ordered_sent_agg <- sent_agg[order(-ave_sentiment)]

#Plot the histogram of the stars / ratings given by the customers
#(Rating is between 0 to 5)
#x <- hist(reviews_all$stars)
with(reviews_all, hist(stars))

#Plot the histogram of the average sentiment scores for the reviews
#y <- hist(reviews_all$ave_sentiment)
with(ordered_sent_agg, hist(ave_sentiment))

#Get the mean rating of the product
mean(reviews_all$stars)

#Get the mean sentiment score of the product
#Higher the sentiment score, more positive are the reviews
mean(ordered_sent_agg$ave_sentiment)

#Get the count of the number of reviews scraped
number_of_reviews <- nrow(reviews_all)

#Sort these reviews in descending order of their ave_sentiment scores
sortedReviewsFromBest <- slice(reviews_all, top_n(ordered_sent_agg, number_of_reviews, ave_sentiment)$element_id)
sortedReviewsFromBest

#Highlight these reviews by sentiment polarity (positive = green; negative = pink) and present them in an html file.
highlight(sentiment_by(sortedReviewsFromBest$comments))

#Create a subset of the top 3 reviews as 'best_reviews' based on the sentiment score
best_reviews <- slice(reviews_all, top_n(ordered_sent_agg, 3, ave_sentiment)$element_id)
best_reviews

#Highlight the top 3 reviews by sentiment polarity (positive = green; negative = pink) and present them in an html file.
highlight(sentiment_by(best_reviews$comments))
#with(best_reviews, sentiment_by(comments)) %>% highlight()

#Create a subset of the bottom 3 reviews as 'worst_reviews' based on their sentiment scores
worst_reviews <- slice(reviews_all, top_n(ordered_sent_agg, 3, -ave_sentiment)$element_id)
worst_reviews

#Highlight the bottom 3 reviews by sentiment polarity (positive = green; negative = pink) as an html file.
highlight(sentiment_by(worst_reviews$comments))
#with(worst_reviews, sentiment_by(comments)) %>% highlight()
