# please install it once
install.packages(c("shiny", "ggplot2", "dplyr", "jsonlite", "lubridate", "extrafont"))

library(shiny)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(lubridate)
library(extrafont)

# load the system's font
font_import(pattern = "times", prompt = FALSE)
loadfonts(device = "win")

# please use your EIA api key
api_key <- "qUb6rdFvajthr9FtxQOOHUHE7VFwSZg7QbeD65ez"

# set the regions and fuel types we want to consider
regions <- c("CAL", "CAR", "CENT", "TEX", "FLA", "MIDA", "MIDW", "NE", "NY", "NW", "SE", "SW", "TEN")
fuels <- c("SUN", "WND", "OTH", "OIL", "COL", "NG", "WAT", "NUC")

# define the fuel order preparing the ggplot graph
fuel_order <- c("SUN", "WND", "OTH", "OIL", "COL", "NG", "WAT", "NUC")

# define the ui
ui <- fluidPage(
  titlePanel("The dashboard of hourly electricity generation data"),
  
  sidebarLayout(
    sidebarPanel(
      # calendar picker
      dateRangeInput(
        inputId = "date_range",
        label = "choose the date range:",
        start = Sys.Date() - 7, # the last 7 days(one week) will be displayed by default
        end = Sys.Date() - 1, # the end day is yesterday by default
        min = "2015-07-01", # the earliest date
        max = Sys.Date() - 1, # the lastest date, we set it yesterday by default
        format = "yyyy-mm-dd",
        startview = "month", # display the month view
        weekstart = 1
      ),
      
      # filter: choose the region
      selectInput("region", "choose the region：",
                  choices = unique(regions),
                  selected = "CAL"),
      
      # filter: choose the fuel type
      checkboxGroupInput("fuel", "choose the fuel type：",
                         choices = unique(fuels),
                         selected = unique(fuels)),
      
      # confirm the selection
      actionButton(
        inputId = "confirm",
        label = "Confirm",
        class = "btn-primary",
        style = "margin-top: 20px"
      ),
      
      width = 4
    ),
    
    mainPanel(
      # to display the stacked bar chart
      uiOutput("dynamic_plot"),
      # to display the data table
      verbatimTextOutput("status"),
      # add the downloading pdf button
      downloadButton("download_pdf", "Export PDF"),
      # add the downloading data button
      downloadButton("download_data", "Export data")
    )
  )
)

# define the server
server <- function(input, output) {
  # transform the date formation get from input
  data_result <- eventReactive(input$confirm, {
    req(input$date_range)
    start_date <- input$date_range[1]
    end_date_sample <- as.Date(input$date_range[2]) + days(1)
    end_date <- format(end_date_sample, "%Y-%m-%d")
    
    if(start_date > end_date){
      showNotification("The start date should less than end date", type = "error")
      return(NULL)
    }
    
    showNotification("The data is coming from eia...", type = "message")
    
    # classify the regions to match the time offset
    west_regions <- c("CAL", "NW", "SW")
    middle_regions <- c("CENT", "TEX", "SE", "TEN")
    east_regions <- c("CAR", "FLA", "MIDA", "MIDW", "NE", "NY")
    
    # match the time offset
    # set the default time "07:00" matching the default region
    time_offset <- "-07:00"
    
    if (input$region %in% west_regions){
      time_offset <- "-07:00"
    }else if(input$region %in% middle_regions){
      time_offset <- "-05:00"
    }else if(input$region %in% east_regions){
      time_offset <- "-04:00"
    }
    
    # constructing url
    url <- paste0(
      "https://api.eia.gov/v2/electricity/rto/fuel-type-data/data/",
      "?frequency=local-hourly&data[0]=value",
      "&start=", start_date, "T00", time_offset,
      "&end=", end_date, "T00", time_offset,
      "&facets[respondent][]=", input$region,
      "&sort[0][column]=period&sort[0][direction]=desc&offset=0",
      "&length=5000",
      "&api_key=", api_key
    )
  
  # deal with the data get from the eia
  tryCatch({
    response <- fromJSON(url)
    
    if(is.null(response$response$data)){
      showNotification("There is no data", type = "warning")
      return(NULL)
    }
    
    data <- tibble(response$response$data)%>%
      filter(respondent == input$region) %>%
      filter(fueltype %in% input$fuel) %>%
      mutate(generation = value) %>%
      mutate(generation = as.numeric(generation)) %>%
      mutate(date = substring(period, 1, 10)) %>%
      mutate(day_time = substring(period, 12, 13)) %>%
      mutate(fueltype = factor(fueltype, levels = fuel_order)) %>%
      filter(date != end_date)
    
    return(data)
  }, error = function(e){
    showNotification(paste("Error:", e$message), type = "error")
    return(NULL)
  })
})
  
  # count the day used by calculating the width
  day_count <- reactive({
    req(data_result())
    length(unique(data_result()$date))
  })
  
  # set up the width of the graph of single day
  output$dynamic_plot <- renderUI({
    single_day_width <- 400
    total_width <- paste0(single_day_width * day_count(), "px")
    plotOutput("stacked_plot", width = total_width, height = "600px")
  })
  
  # draw the graph
  graph_generation <- reactive({
    req(data_result())
    
    color_mapping <- c("SUN" = "#f9d71c",
                       "WND" = "#008000",
                       "OTH" = "#9b59b6",
                       "OIL" = "#34495E",
                       "COL" = "#CCCCCC",
                       "NG" = "#FFC026",
                       "WAT" = "#0000CC",
                       "NUC" = "#CC0000")
    
    ggplot(data_result(), aes(x = day_time, y = generation, fill = fueltype)) +
      geom_col(position = "stack") +
      facet_wrap(~date, nrow = 1) +
      scale_fill_manual(values = color_mapping) +
      labs(x = "Hour", y = "Generation(MWh)", fill = "Fuel Type") +
      theme_minimal() +
      theme(panel.spacing.x = unit(0, "cm"),
            text = element_text(family = "Times New Roman", size = 12),
            axis.text.x = element_text(vjust = 1, face = "bold", size = 8),
            axis.text.y = element_text(face = "bold"),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            strip.text.x = element_text(size = 11, face = "bold"))
  })
  
  # generate the stacked bar chart
  output$stacked_plot <- renderPlot({
    graph_generation()
  })
  
  # To download the pdf of the graph generated
  output$download_pdf <- downloadHandler(
    filename = function(){
      time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      paste0("result_plot-", time, ".pdf")
    },
    
    content = function(file){
      ggsave(file, plot = graph_generation(), device = cairo_pdf,
             width = 4.7 * day_count(), height = 7, dpi = 300)
    })
  
  # To download the data of the graph used
  output$download_data <- downloadHandler(
    filename = function(){
      time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      paste0("result_plot-", time, ".csv")
    },
    
    content = function(file){
      write.csv(data_result(), file)
    })
  
  # catch the status information
  output$status <- renderPrint({
    if(is.null(data_result())){
      cat("No data to display")
    }else{
      cat("The data has been loaded successfully")
    }
  })
}

# run the application
shinyApp(ui, server)
