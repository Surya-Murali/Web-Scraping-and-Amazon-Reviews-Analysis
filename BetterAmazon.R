
install.packages("pacman")
install.packages("sentimentr")
pacman::p_load(XML, dplyr, stringr, rvest, audio)
pacman::p_load_gh("trinker/sentimentr")


library("pacman")

test <- function(prod_code, pages = 2){
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  trim
  #prod_code = "B013J7JM4K"
  #prod_code = "B075QMZH2L"
  url <- paste0("https://www.amazon.com/dp/", prod_code)
  doc <- read_html(url)
  doc
  #obtain the text in the node, remove "\n" from the text, and remove white space
  prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
  prod
  #-------------------
  #Source funtion to Parse Amazon html pages for data
  source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")
  
  #pages <- 2
  
  reviews_all <- NULL
  for(page_num in 1:pages){
    url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
    doc <- read_html(url)
    
    reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
    reviews_all <- rbind(reviews_all, cbind(prod, reviews))
  }
  
  head(reviews_all)
  #-------------------
  
  library("sentimentr")
  
  sent_agg <- with(reviews_all, sentiment_by(comments))
  head(sent_agg)
  
  par(mfrow=c(1,2))
  with(reviews_all, hist(stars))
  with(sent_agg, hist(ave_sentiment))
  mean(reviews_all$stars)
  mean(sent_agg$ave_sentiment)
  
  best_reviews <- slice(reviews_all, top_n(sent_agg, 3, ave_sentiment)$element_id)
  with(best_reviews, sentiment_by(comments)) %>% highlight()
  
  worst_reviews <- slice(reviews_all, top_n(sent_agg, 3, -ave_sentiment)$element_id)
  with(worst_reviews, sentiment_by(comments)) %>% highlight()
}

test("B00AFTT7IW", 5)
