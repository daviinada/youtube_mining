# Get youtube caption
library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(dygraphs)
library(plotly)
library(tuber)
library(stringr)
library(tm)
library(wordcloud)  
library(RColorBrewer)

setwd('/home/toshi/Desktop/work_butanta/youtube_mining/')

client_id <- '812757213029-tt2n55mmbm4143ehdhap21e25ihp0kse.apps.googleusercontent.com' 
key <- 'SblVJsud6AqXdqKpQJamZRxJ'

yt_oauth(client_id, key)

# save(file = 'all_vid_nerdologia.RObj', all_vid_nerdologia)

load('all_vid_nerdologia.RObj')

str(all_vid_nerdologia)

date_split_list <- strsplit(x = as.character(all_vid_nerdologia$publication_date), split = 'T')
date_split_list <- lapply(date_split_list, function(x){ x[1] } ) %>% unlist()
all_vid_nerdologia['publication_date'] <- date_split_list
all_vid_nerdologia$publication_date <- as.Date(all_vid_nerdologia$publication_date)

all_vid_nerdologia$viewCount <-  as.numeric(all_vid_nerdologia$viewCount)
all_vid_nerdologia$likeCount <- as.numeric(all_vid_nerdologia$likeCount)
all_vid_nerdologia$dislikeCount <- as.numeric(all_vid_nerdologia$dislikeCount)
all_vid_nerdologia$favoriteCount <- as.numeric(all_vid_nerdologia$favoriteCount)
all_vid_nerdologia$commentCount <- as.numeric(all_vid_nerdologia$commentCount)

all_vid_nerdologia[which(all_vid_nerdologia$dislikeCount == max(all_vid_nerdologia$dislikeCount)), ]
all_vid_nerdologia[which(all_vid_nerdologia$likeCount == max(all_vid_nerdologia$likeCount)), ]

all_vid_nerdologia$publication_date <- lubridate::as_datetime(all_vid_nerdologia$publication_date)

# Cleanning data title
all_vid_nerdologia$title <- gsub('\\s\\|\\s.*$', '', all_vid_nerdologia$title)

# tabela processada
all_vid_nerdologia_gather <- all_vid_nerdologia %>%
    mutate(proportion_like = likeCount/ viewCount,
           proportion_dislike = dislikeCount/ viewCount) %>%
    gather(key= type_counts, value= counts, 
           c(likeCount, dislikeCount, commentCount, viewCount)) 

str(all_vid_nerdologia)

plot_obj <- all_vid_nerdologia_gather %>%
    filter(stringr::str_detect('likeCount|dislikeCount|commentCount', type_counts)) %>%
    ggplot(aes(x= publication_date, y= as.numeric(counts), col= type_counts)) +
    geom_line() +
    geom_point() +
    scale_x_date() +
    theme_bw()

str(all_vid_nerdologia)

# Plotly
all_vid_nerdologia %>%
    mutate(proportion_like = likeCount / viewCount,
           proportion_dislike = dislikeCount / viewCount) %>%
    arrange(publication_date) %>%
    plot_ly() %>%
    add_trace(x = ~publication_date, y = ~viewCount, 
              name = 'View count', type = 'scatter', 
              mode ="markers+lines", text = ~paste('Video: ', title),
              line = list(color = '#440154FF ', width = 4)) %>%
    add_trace(x = ~publication_date, y = ~likeCount, 
              name = 'Like count', type = 'scatter',
              mode ="markers+lines", text = ~paste('Video: ', title),
              line = list(color = '#39568CFF', width = 4)) %>%
    add_trace(x = ~publication_date, y = ~dislikeCount,
              name = 'Dislike count', type = 'scatter', 
              mode = "markers+lines", text = ~paste('Video: ', title),
              line = list(color = '#29AF7FFF', width = 4)) %>%
    add_trace(x = ~publication_date, y = ~commentCount,
              name = 'Comment count', type = 'scatter', 
              mode = "markers+lines", text = ~paste('Video: ', title),
              line = list(color = '#FDE725FF', width = 4)) %>%
    layout(xaxis = list(title = "Video publication date"),
           yaxis = list (title = "Metric's count"),
           font =  list(size = 16),
           hovermode = 'compare')

all_vid_nerdologia_gather[which(all_vid_nerdologia_gather$title == 'Sexismo'), ]


# Preciso disso!!!
# https://plot.ly/r/shiny-coupled-hover-events/



library(RCurl)

url_request <- paste0('http://diycaptions.com/php/get-automatic-captions-as-txt.php?id=',  all_vid_nerdologia$id, '&language=asr')     

# html_page <- list()

for(i in 1:length(url_request) ){
    
    html_page[i] <- getURL(url_request[i])
    
    Sys.sleep(30)
    
}

# save(file = 'html_page.RObj', html_page)

load("html_page.RObj")

html_page_text <- lapply(html_page, function(x){
    
    split_1 <- strsplit(x[[1]], '<br><br>')[[1]][2]
    strsplit(split_1[[1]], '\t\t</div>')[[1]][1]
    
})

save(file = 'html_page_text.RData', html_page_text)

# Generate a data frame
text_df_total <- data.frame(doc_id = all_vid_nerdologia$id, text = unlist(html_page_text), stringsAsFactors = FALSE , drop=FALSE)

text_df_total <- text_df_total[ which(is.na(text_df_total$text)), ]

text_corpus_df <- Corpus(DataframeSource(text_df_total))

text_corpus_df_filtered <- text_corpus_df %>%
    tm_map(stripWhitespace)  %>%    
    tm_map(removePunctuation) %>%                              
    tm_map(removeNumbers)   %>%                               
    tm_map(removeWords, c(stopwords("portuguese"))) %>%  
    tm_map(removeNumbers) %>%  
    tm_map(stripWhitespace) %>%    
    tm_map(content_transformer(tolower))

# Criando a matrix de termos:
corpus_tf <- TermDocumentMatrix(text_corpus_df_filtered, control = list(minWordLength = 3))

corpus_m <- as.matrix(corpus_tf)

corpus_m_sorted <- sort(rowSums(as.matrix(corpus_m)), decreasing=TRUE)

df_total <- data.frame(word=names(corpus_m_sorted), freq=as.numeric(corpus_m_sorted))

wordcloud(df_total$word, df_total$freq, min.freq = 3, max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

