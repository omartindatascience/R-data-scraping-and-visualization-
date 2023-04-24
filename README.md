# R-data-scraping from amazon, sentiment analysis -and-visualization-
R data scraping and visualization example

README

This sample R code uses web scraping techniques to extract customer reviews for two products (SamsungQ80 and SonyX85J) from the Amazon website. It then performs sentiment analysis on the reviews using the sentimentr package and creates visualizations to explore the sentiment and rating trends over time.

Instructions:

Install and load the required packages (rvest, ggplot2, ggthemes, dplyr, tidyr, tidytext, XML, stringr, wordcloud, reshape2, lexicon, sentimentr, knitr, and DT).

Set the regional language configuration to English using the Sys.setlocale() function.

Define the getReviewsFromAmazon function to extract the reviews for the specified product codes and product names.

Call the getReviewsFromAmazon function to extract the reviews and store them in a data frame named final_table.

Use the sentiment_by function from the sentimentr package to perform sentiment analysis on the reviews.

Concatenate the original data frame (final_table) with the sentiment analysis data frame (sentiment_review) using the cbind function.

Use the ggplot2 package to create a boxplot showing the average sentiment by star rating for each product.

Calculate the Pearson correlation coefficient between the sentiment analysis score and the rating using the cor function.

Create a time series plot showing the sentiment trend over time for each product.

Note: The product codes used in this code correspond to SamsungQ80 (B08WF4XDMF) and SonyX85J (B08WJMSS8H). If you want to extract reviews for different products, you will need to obtain their product codes and update the function call in step 4 accordingly.
