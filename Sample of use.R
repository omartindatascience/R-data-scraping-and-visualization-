
#We load the libraries

library(rvest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(tidytext)
library(rvest)
library(XML)
library(stringr)
library(wordcloud)
library(reshape2)
library(lexicon)
library(sentimentr)
library(stringr) 
library(knitr) 
library(DT) 

# We set the regional language configuration to English

Sys.setlocale("LC_TIME", "English")

# Function to obtain a data frame of Amazon reviews by product code
  
 getReviewsFromAmazon <- function(product_codes, product_names = c()){
  final_table <- matrix(ncol = 4, nrow = 1) #We define two additional columns, one for date and the other for stars
  
  for (product_code in product_codes) {
    
    url <- paste0("https://www.amazon.com/product-reviews/", product_code, "/?pageNumber=")
    pageNumber <- 1
    webpage <- read_html(paste0(url, pageNumber))
    
    while (length(html_nodes(webpage, ".review")) > 0) {
      reviews <- html_nodes(webpage, ".review-text-content")
      reviews <- html_text(reviews, trim = TRUE)
      date <- html_nodes(webpage, ".review .review-date") #We get the date, we specify 2 classes, because outside of normal reviews there are some dates and this would generate problems
      date <- html_text(date, trim = TRUE)
      stars <- html_nodes(webpage, ".review .review-rating") #we do the same for stars
      stars <- html_text(stars, trim = TRUE)
      table <- cbind(as.vector(reviews), as.vector(date), as.vector(stars), rep(product_code, length(reviews))) #add extracted date to table also
      final_table <- rbind(final_table, table)
      pageNumber <- pageNumber + 1
      webpage <- read_html(paste0(url, pageNumber))
    }
  }
  
  final_table <- as.data.frame(final_table)
  final_table <- final_table[-1,]
  colnames(final_table) <- c("ReviewText", "Date", "Rating", "ProductCode")
  final_table$ReviewText <- as.character(final_table$ReviewText) #Text may be read as Factor, so convert it into character
  final_table$Date <-  gsub(".*on ", "", final_table$Date) #date contains also location information, we remove it
  final_table$Rating <-  gsub(" out.*", "", final_table$Rating) #Rating is in format x out of 5 stars, we only need x
  final_table$Rating <- as.numeric(final_table$Rating) #Convert Rating to Numeric
  final_table$ProductCode <- as.factor(final_table$ProductCode)
  levels(final_table$ProductCode) <- product_names #Change the product codes to the product names
  return(final_table)
}

 
# Obtain data frame using the getReviewsFromAmazon function
 
final_table <- getReviewsFromAmazon(product_codes = c("B08WF4XDMF", "B08WJMSS8H"), product_names = c("SamsumgQ80", "SonyX85J"))


# Get sentiment analysis of the obtained reviews.
sentiment_review <- sentiment_by(final_table$ReviewText, polarity_dt = hash_sentiment_jockers_rinker)
head(sentiment_review, 10)

# Concatenate the original data frame obtained with the getReviewsFromAmazon function and the sentiment analysis data frame.

final_table <- cbind(final_table, sentiment_review[,-1])

# Plot Avg.Sentiment

ggplot(final_table, aes(x = Rating, y = ave_sentiment, group = factor(Rating), fill = factor(Rating))) + #specify axis and color vars
  geom_boxplot() +  # boxplot layer
  facet_wrap(~ProductCode) + # separates the graphics according to product code
  geom_hline(yintercept=0, color = "grey") + # we draw a line at the point x=0, showing the neutral sentiment
  ylab("Avg. Sentiment") +  # we assign the axis name of y
  xlab("Review Star Rating") + # we assign the name of the axis y
  ggtitle("Sentiment of the selected Products by Star Rating") # assign a title to the graphic

# We calculated the Pearson correlation coefficient between the sentiment analysis score and the rating.

correlation <- cor(final_table$ave_sentiment, final_table$Rating) 
print(correlation)


# Visualization of product reviews over time

final_table$Date <- as.Date(as.character(final_table$Date), "%B %d, %Y")
date_sentiment <- aggregate(ave_sentiment ~ Date + ProductCode, final_table, mean) #compute the average sentiment for each date and product

ggplot(date_sentiment, aes(x = Date, y = ave_sentiment)) +
  geom_smooth(method="loess", size=1, se=T, span = .5) +
  facet_wrap(~ProductCode) + # we produce different graphics according to product codes
  geom_hline(yintercept=0, color = "grey") + # grey line at x = 0, to mark neutral sentiment
  ylab("Avg. Sentiment") +  # axis name y
  xlab("Date") + # axis name x
  ggtitle("Sentiment of Amazon Reviews over time by Product")


# Generation of a wordcloud chart with the main words by product.

# We create a function called getWordcloud, to plot a wordcloud of a product that we pass as a parameter

getWordcloud <- function(product_name){

data_filter <- filter(final_table,ProductCode==product_name)

dat <- data.frame(data_filter$ReviewText)

tidy_dat <- tidyr::gather(dat, key, word) %>% select(word)

tidy_dat$word %>% length()

unique(tidy_dat$word) %>% length()

tokens <- tidy_dat %>% 
  unnest_tokens(word, word) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup()

tokens %>% head(10)

data("stop_words") # we clean up stop words
tokens_clean <- tokens %>%
  anti_join(stop_words)

nums <- tokens_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique() # we eliminate numeric values

tokens_clean <- tokens_clean %>% 
  anti_join(nums, by = "word")
      
 pal <- brewer.pal(8,"Dark2") # We set a color palette and generate a wordcloud with the data frame already processed.
 tokens_clean %>% 
   with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))  
 
}
 
# We generate wordcloud graphics by calling our getWordcloud function for the two products we have

getWordcloud("SonyX85J")
getWordcloud("SamsumgQ80")



 
 
 
 
