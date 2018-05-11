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

setwd('/home/davi/Desktop/youtube_mining/')

client_id <- 'xxx' 

key <- 'xxxx'

yt_oauth(client_id, key)

all_vid_nerdologia <- get_all_channel_video_stats(channel_id = 'UClu474HMt895mVxZdlIHXEA', mine = FALSE)

saveRDS(all_vid_nerdologia, "all_vid_nerdologia.RDS")

all_vid_nerdologia <- readRDS('all_vid_nerdologia.RDS')

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

all_vid_nerdologia_gather %>%
    arrange(publication_date) %>%
    plot_ly(source = "source") %>% 
    add_lines(x = ~publication_date, y = ~counts,
              mode = "lines+marker", opacity = 1, 
              color = ~type_counts,
              hoverinfo = 'y',
              line = list(width = 4)) %>%
    layout(hovermode = 'compare')

# Preciso disso!!!
# https://plot.ly/r/shiny-coupled-hover-events/

all_vid_nerdologia_gather$id[1:3]
 
get_url_capition <- function(video_id){
     
     url_request <- paste0('http://diycaptions.com/php/get-automatic-captions-as-txt.php?id=', video_id, '&language=asr')     
     
     html_page_text <- lapply(url_request, function(x){ html_text(read_html(x))  })
     
     text_df <- data.frame(doc_id = video_id, text = unlist(html_page_text), stringsAsFactors = FALSE , drop=FALSE)
     
     return(text_df)
   
 }


library(RCurl)
library(XML)

all_vid_nerdologia$title
all_vid_nerdologia$id

#html_page <- list()

for(i in 47:length(url_request) ){
    
    
    Sys.sleep(60)
    
    html_page[i] <- getURL(url_request[i])
    
    Sys.sleep(60)
    
}
# saveRDS(object = html_page, file = 'html_page.RDS')

df_corpus <- Corpus(DataframeSource(teste))

df_corpus_filtered <- df_corpus %>%
    tm_map(stripWhitespace)  %>%    
    tm_map(removePunctuation) %>%                              
    tm_map(removeNumbers)   %>%                               
    tm_map(removeWords, c(stopwords("portuguese"))) %>%  
    tm_map(removeNumbers) %>%  
    tm_map(stripWhitespace) %>%    
    tm_map(content_transformer(tolower))

inspect(df_corpus_filtered)

inspect(df_corpus)

#Criando a matrix de termos:
df_corpus_tf <- TermDocumentMatrix(df_corpus_filtered, control = list(minWordLength = 4))
df_corpus_df = as.matrix(df_corpus_tf)
?TermDocumentMatrix
v <- sort(rowSums(as.matrix(df_corpus_df)), decreasing=TRUE)
df <- data.frame(word=names(v), freq=v)

rownames(df) <- NULL

wordcloud(df$word, df$freq, min.freq = 2, max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

