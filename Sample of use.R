
#Cargamos las librerías 

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

# Establecemos la configuración regional de idioma

Sys.setlocale("LC_TIME", "English")

# 1. Función para obtener un data frame de reviews de amazon por código de producto.
  
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

 
# Obtener data frame usando la función getReviewsFromAmazon
 
final_table <- getReviewsFromAmazon(product_codes = c("B08WF4XDMF", "B08WJMSS8H"), product_names = c("SamsumgQ80", "SonyX85J"))


# 2. Obtención del análisis de sentimiento de las reviews obtenidas.

sentiment_review <- sentiment_by(final_table$ReviewText, polarity_dt = hash_sentiment_jockers_rinker)
head(sentiment_review, 10)

# Concatenamos el data frame original obtenido con la función getReviewsFromAmazon y el de análisis de sentimiento

final_table <- cbind(final_table, sentiment_review[,-1])

# 3. Plot Avg.Sentiment de las reviews factorizados por Rating

ggplot(final_table, aes(x = Rating, y = ave_sentiment, group = factor(Rating), fill = factor(Rating))) + #specify axis and color vars
  geom_boxplot() +  # capa de diagrama de caja
  facet_wrap(~ProductCode) + #separa los gráficos segun su codigo de producto
  geom_hline(yintercept=0, color = "grey") + #dibujamos una linea en el punto x=0, mostrando el sentimiento neutral
  ylab("Avg. Sentiment") +  # asignamos el nombre de la axis y
  xlab("Review Star Rating") + # asignamos el nombre a la axis x
  ggtitle("Sentiment of the selected Products by Star Rating") # asignamos un titulo al gráfico

# Calculamos la el coeficiente de correlación de Pearson entre la puntución del análisis de setimiento y el rating.

correlation <- cor(final_table$ave_sentiment, final_table$Rating) 
print(correlation)


# 4. Visualización de las reviews de los productos a lo largo del tiempo

final_table$Date <- as.Date(as.character(final_table$Date), "%B %d, %Y")
date_sentiment <- aggregate(ave_sentiment ~ Date + ProductCode, final_table, mean) #compute the average sentiment for each date and product

ggplot(date_sentiment, aes(x = Date, y = ave_sentiment)) +
  geom_smooth(method="loess", size=1, se=T, span = .5) +
  facet_wrap(~ProductCode) + #producir gráficos distintos segun codigos de producto
  geom_hline(yintercept=0, color = "grey") + # línea gris en x = 0, para marcar sentimiento neutral
  ylab("Avg. Sentiment") +  # nombre del eje y
  xlab("Date") + #nombre del eje x
  ggtitle("Sentiment of Amazon Reviews over time by Product")


# 5. Generación de un wordcloud con las principales palabras por productos

# Creamos una función llamada getWordcloud, para plotear un wordcloud de un producto que pasamos como parámetro


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

data("stop_words") # limpiamos stop words
tokens_clean <- tokens %>%
  anti_join(stop_words)

nums <- tokens_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique() # eliminamos valores numéricos

tokens_clean <- tokens_clean %>% 
  anti_join(nums, by = "word")
      
 pal <- brewer.pal(8,"Dark2") # Establecemos una paleta de colores y generamos un wordcloud con el data frame ya procesado.
 tokens_clean %>% 
   with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))  
 
}
 
# Generamos gráficos wordcloud llamando a nuestra función getWordcloud para los dos productos que tenemos

getWordcloud("SonyX85J")
getWordcloud("SamsumgQ80")


# 6. Interpretación de resultados: Se adjunta en archivo pdf,junto con los gráficos generados. 
 
 
 
 