## app.R ##
library(shinydashboard)
library(highcharter)
library(dplyr)


load('brazil_map.RData')

ui <- dashboardPage(
    dashboardHeader(title = "Alpha dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
    ),
    
    ## Body content ----
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        column(width = 4, highchartOutput("hcmap_br", height = "500px")),
                        column(width = 4, highchartOutput("barplot_total")),
                        column(width = 4, highchartOutput("pie_plot"))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    h2("Widgets tab content")
            )
        )
    )
)

server <- function(input, output){
    # Br map process ----
    brazil_data <- get_data_from_map(brazil_map) 
    
    data_fake <- brazil_data %>% 
        dplyr::select(code = `hc-a2`) %>% 
        mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))
    
    output$hcmap_br <- renderHighchart({      
        # Explanation about click value 
        # https://stackoverflow.com/questions/37208989/how-to-know-information-about-the-clicked-bar-in-highchart-column-r-shiny-plot
        canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', event.point.name);}") # event point + name gets the state name!
        # Esse eh especifico para zoom
        hcmap(map = "countries/br/br-all",
              data = data_fake, value = "value",
              joinBy = c("hc-a2", "code"),
              name = "Fake data",
              dataLabels = list(enabled = TRUE, 
                                format = '{point.name}'),
              borderColor = "#FAFAFA", borderWidth = 0.1,
              tooltip = list(valueDecimals = 2, 
                             valuePrefix = "$",
                             valueSuffix = " USD")) %>%
            hc_mapNavigation(enabled = TRUE) %>%
            hc_plotOptions(series = list(events = list(click = canvasClickFunction)))
        
    })   
    
    # makeReactiveBinding("outputText")
    
    # observeEvent(input$canvasClicked, {
    #     outputText <<- paste0("You clicked on series ", input$canvasClicked) #defined on JS()
    # })
    # 
    # output$text <- renderText({
    #     outputText      
    # })
    
    # Dependent data from click in map  ----
    
    data_example <- read.table(file='data_text.txt', sep='\t', header = T)
    
    makeReactiveBinding("click_value")
    
    observeEvent(input$canvasClicked, {
        click_value <<-  input$canvasClicked
    })
    
    plot_df <- reactive({
        
        if(length(click_value) < 1){
            data_example
        }else{
            data_example %>%
                filter(states_names == click_value)
        }
        
    })
    
    output$barplot_total <- renderHighchart({    
        
        hchart(plot_df(), "column", hcaes(x = states_names, y = counts, group = sex))
    
    })
    
    # Pie
    output$pie_plot <- renderHighchart({    
      
        temp_df <- plot_df() %>%
            filter(states_names == click_value) %>%
            group_by(states_names, sex)
        
        temp_df$percent <- temp_df$counts/sum(temp_df$counts) 
        
        highchart() %>% 
            hc_chart(type = "pie") %>%
            hc_add_series_labels_values(values = temp_df$percent, labels = temp_df$sex, name=temp_df$states_names)
    
    })

    output$click_val <- renderText({
        click_value
    })
}

shinyApp(ui, server)
