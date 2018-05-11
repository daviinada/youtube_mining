library(shinydashboard)
library(highcharter)
library(ggplot2)
library(dplyr)
library(plotly)
library(tm)
library(wordcloud)  
library(VennDiagram)
library(kableExtra)

load('all_vid_nerdologia.RData')
load('html_page_text.RData')
load('html_page_text_sorted.RData')

ui <- dashboardPage(
    # Header ----
    dashboardHeader(title = "Youtube metrics"),
    
    # Sidebar content ----
    dashboardSidebar(collapsed = TRUE,
                     sidebarMenu(
                         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                     )
    ),
    
    # Body content ----
    dashboardBody(
        
        includeCSS("www/custom.css"),
        tabItems(
            # First tab content
            tabItem(tabName= "dashboard",
                    
                    fluidRow(
                        column(width= 7,
                               tags$blockquote(("Metrics from video captions")),
                               plotlyOutput(outputId = 'metrics_total_plot', height= '400px')
                               
                        ),
                        column(width= 5,
                               wellPanel(
                                   plotOutput(outputId= 'cursor_wc_plot', height= '400px')
                               )
                        )
                    ),
                    
                    fluidRow(
                        column(4,
                               tags$blockquote(("Metrics  from video titles")),
                               valueBoxOutput("likebox", width = 15),
                               valueBoxOutput("dislikebox", width = 15),
                               valueBoxOutput("commentbox", width = 15)
                        )
                    )
                    
            )
        )
    )
)

server <- function(input, output){
    
    # Plot wordcloud total ----
    output$metrics_total_plot <- renderPlotly({
        
        all_vid_nerdologia %>%
            arrange(publication_date) %>%
            plot_ly(source = "source") %>%
            add_trace(x = ~publication_date, y = ~viewCount, 
                      name = 'View count', type = 'scatter', 
                      mode ="markers+lines", text = ~paste('Video: ', title),
                      line = list(color = '#440154FF ', width = 2)) %>%
            add_trace(x = ~publication_date, y = ~likeCount, 
                      name = 'Like count', type = 'scatter',
                      mode ="markers+lines", text = ~paste('Video: ', title),
                      line = list(color = '#39568CFF', width = 2)) %>%
            add_trace(x = ~publication_date, y = ~dislikeCount,
                      name = 'Dislike count', type = 'scatter', 
                      mode = "markers+lines", text = ~paste('Video: ', title),
                      line = list(color = '#29AF7FFF', width = 2)) %>%
            add_trace(x = ~publication_date, y = ~commentCount,
                      name = 'Comment count', type = 'scatter', 
                      mode = "markers+lines", text = ~paste('Video: ', title),
                      line = list(color = '#FDE725FF', width = 2)) %>%
            layout(xaxis = list(title = ""),
                   yaxis = list (title = "Metric's count"),
                   font =  list(size = 12),
                   hovermode = 'compare',
                   legend = list(orientation = 'h')) 
    })
    
    # Plot over cursor selected word cloud ----
    output$cursor_wc_plot <- renderPlot({
        
        eventdata <- event_data("plotly_hover", source = "source")
        
        validate(need(!is.null(eventdata), "Pass the mouse over the point :)"))
        
        cursor_id <- as.numeric(eventdata$pointNumber)[1] + 1
        
        all_vid_nerdologia <- all_vid_nerdologia %>%
            arrange(publication_date)
        
        text_df_cursor <- data.frame(doc_id= all_vid_nerdologia$id[ cursor_id ], 
                                     text= unlist(html_page_text_sorted[ cursor_id ]), 
                                     stringsAsFactors= FALSE , drop=FALSE)
        
        cursor_corpus_df <- Corpus(DataframeSource(text_df_cursor))
        
        cursor_corpus_df_filtered <- cursor_corpus_df %>%
            tm_map(stripWhitespace)  %>%    
            tm_map(removePunctuation) %>%                              
            tm_map(removeNumbers)   %>%                               
            tm_map(removeWords, c(stopwords("portuguese"))) %>%  
            tm_map(removeNumbers) %>%  
            tm_map(stripWhitespace) %>%    
            tm_map(content_transformer(tolower))
        
        # Creanting a term matrix
        corpus_tf <- TermDocumentMatrix(cursor_corpus_df_filtered)
        
        cursor_corpus_m <- as.matrix(corpus_tf)
        
        cursor_corpus_m_sorted <- sort(rowSums(as.matrix(cursor_corpus_m)), decreasing= TRUE)
        
        df_cursor_final <- data.frame(word= names(cursor_corpus_m_sorted), freq= as.numeric(cursor_corpus_m_sorted))
        
        df_cursor_final$word <- as.character(df_cursor_final$word)
        
        df_cursor_final <- df_cursor_final[which(nchar(df_cursor_final$word) > 3), ]
        
        wordcloud(df_cursor_final$word, df_cursor_final$freq, min.freq = 2, max.words= 50,
                  random.order= FALSE, colors=brewer.pal(8, "Dark2"), scale=c(3, 0.09))
    })
    
    # Metrics top videos box ----
    
    # Like box
    output$likebox <- renderValueBox({
        toplike <- all_vid_nerdologia %>%
            arrange(desc(likeCount)) %>%
            head(10)
        
        valueBox(toplike$title[1], 
                 paste0("Most liked video with: ", toplike$likeCount[1], " likes"), 
                 icon = icon("thumbs-up", lib = "glyphicon"),
            color = "blue" 
        )
    })
    
    # Dislike box
    output$dislikebox <- renderValueBox({
        topdislike <- all_vid_nerdologia %>%
            arrange(desc(dislikeCount)) %>%
            head(10)
        
        valueBox(topdislike$title[1], 
                 paste0("Most disliked video with: ", topdislike$dislikeCount[1], " dislikes"),
                 icon = icon("thumbs-down", lib = "glyphicon"),
                 color = "red" 
        )
    })
    
    # Comments box
    output$commentbox <- renderValueBox({
        topcomment <- all_vid_nerdologia %>%
            arrange(desc(commentCount)) %>%
            head(10)
        
        valueBox(topcomment$title[1], 
                 paste0("Most Commented video with: ", topcomment$commentCount[1], " comments"),
                 icon = icon("comment", lib = "font-awesome"),
                 color = "orange" 
        )
    })
}

shinyApp(ui, server)
