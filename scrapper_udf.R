#Install pacman. It reduces typing in package management actions. The function names in pacman package follow the format p_xxx
install.packages("pacman")
install.packages("sentimentr")
library("pacman")
#p_load allows the user to load one or more packages as a substitute for the library function
pacman::p_load(XML, dplyr, stringr, rvest, audio)

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

reviews_all
