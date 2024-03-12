library(shiny)
library(bs4Dash)
library(gtrendsR)
library(lubridate)
library(tidyverse)
library(tidyquant)
library(tidygeocoder)
library(fs)
library(plotly)
library(shinycssloaders)
library(DT)
library(waiter)
library(rmarkdown)
library(callr)

ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#18191A"),
  fullscreen = TRUE,
  dashboardHeader(title = dashboardBrand(
    title = "University and Online Learning Technology Search Interests: Google Trends Analytics",
    color = "indigo",
    image = "logo.png"
  ),
  titleWidth = 500), # end of header
  dashboardSidebar(disable = TRUE), # end of Sidebar
  dashboardBody(
    fluidPage(
      tags$head(
        tags$style(".butt{background:#007bff;} .butt{color: white;}"),
        tags$style(
          HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
          )
        )
      ),
      fluidRow(
        column(4,
               box(title = "Query Box", status = "white",
                   height = 342,
                   width = 12, icon = icon("filter"), solidHeader = TRUE,
                   uiOutput("search_term_op"),
                   uiOutput("time_period_op"),
                   actionButton(inputId = "submit", "Submit Query", status = "danger"),
                   downloadButton("report", "Download Report", class = "butt"),
                   p(strong("HELP:"), "Click submit query after selecting a
                 search term(s) and search period to generate results and
                 wait a few seconds. The analysis are
                 based on google searches."))),
        column(8,
               box(title = "Search Term Trend Over Time", status = "white",
                   height = 342, 
                   width = 12, icon = icon("chart-line"), solidHeader = TRUE,
                   withSpinner(plotlyOutput("fig1_line_plot", width = "auto", height = 300))))
      ),
      fluidRow(
        column(5,
               box(title = "Trend By Geography", status = "white", height = 342,
                   width = 12, icon = icon("table"), solidHeader = TRUE,
                   maximizable = FALSE,
                   withSpinner(dataTableOutput("geo_tbl1", height = 200)))),
        column(7,
               box(title = "Top Related Searches", status = "white", height = 342, 
                   width = 12, icon = icon("chart-bar"), solidHeader = TRUE,
                   maximizable = FALSE,
                   withSpinner(plotlyOutput("fig3_bar_plot", width = "auto", height = 300))))
      )
      
    ) # end of page
  ) # end of body
) # end of dashboard page

server <- function(input, output, session){
  
  output$search_term_op <- renderUI({
    selectizeInput(
      "search_terms",
      "Search Term(s)",
      choices = c("bursary", "moodle",
                  "ienabler", "Blackboard Learn",
                  "canvas lms", "Udemy",
                  "coursera", "udacity","edX",
                  "university","university application",
                  "sol plaatje university","spu",
                  "University of Mpumalanga", "ump",
                  "University of South Africa", "unisa",
                  "University of Cape Town", "uct",
                  "University of the Witwatersrand", "wits",
                  "University of Johannesburg", "uj",
                  "Stellenbosch University",
                  "University of KwaZulu-Natal", "ukzn",
                  "University of Pretoria",
                  "University of the Western Cape", "uwc",
                  "Rhodes University",
                  "North-West University", "nwu",
                  "University of the Free State", "ufs",
                  "Durban University of Technology", "dut",
                  "University of Fort Hare", "ufh",
                  "Nelson Mandela University",
                  "Tshwane University of Technology", "tut",
                  "Central University of Technology",
                  "University of Zululand", "unizulu",
                  "Vaal University of Technology", "vut",
                  "University of Venda", "univen",
                  "Cape Peninsula University of Technology", "cput",
                  "Mangosuthu University of Technology", "mut",
                  "University of Limpopo",
                  "Sefako Makgatho Health Sciences University", "smu"),
      multiple = TRUE,
      options = list(maxItems = 3),
      selected = c("moodle", "Blackboard Learn", "canvas lms"))
  })
  
  output$time_period_op <- renderUI({
    selectInput(
      "search_period",
      "Search Period",
      choices = c("Last hour" = "now 1-H",
                  "Last four hours" = "now 4-H",
                  "Last day" = "now 1-d",
                  "Last seven days" = "now 7-d",
                  "Past 30 days" = "today 1-m",
                  "Past 90 days" = "today 3-m",
                  "Past 12 months" = "today 12-m",
                  "Last five years" = "today+5-y",
                  "Since the beginning of Google Trends (2004)" = "all"),
      selected = "now 7-d"
    )
  })
  
  rv <- reactiveValues()
  
  observeEvent(input$submit, {
    req(input$search_terms)
    req(input$search_period)
    
    id <- showNotification(
      "generating analysis...", 
      duration = NULL, 
      closeButton = FALSE,
      type = "error"
    )
    on.exit(removeNotification(id), add = TRUE)
    
    rv$gtrends_lst <- input$search_terms %>%
      gtrends(geo = "ZA", time = input$search_period)
    
    
  }, ignoreNULL = FALSE)
  
  # Search Term Interest Over Time ----
  output$fig1_line_plot <- renderPlotly({
    req(rv$gtrends_lst)
    ggplotly(
      rv$gtrends_lst %>% 
        pluck("interest_over_time") %>% 
        mutate(hits = as.numeric(hits)) %>% 
        as_tibble() %>% 
        ggplot(aes(date, hits, color = keyword)) +
        geom_line() +
        geom_smooth(span = 0.3, se = FALSE) +
        theme_tq() +
        scale_color_tq()
    )
  })
  
  # Trends by Geography ----
  output$geo_tbl1 <- renderDataTable({
    req(rv$gtrends_lst)
    location_tbl <- rv$gtrends_lst %>% 
      pluck("interest_by_region") %>% 
      select(location, hits, keyword)
    
    DT::datatable(location_tbl,
                  rownames = T,
                  filter = "top",
                  extensions = 'Scroller',
                  options = list(pageLength = 2, scrollY = 120,
                                 scroller = TRUE, info = TRUE,
                                 deferRender = TRUE,
                                 searching = FALSE))
  })
  
  # Top Related Searches ----
  output$fig3_bar_plot <- renderPlotly({
    req(rv$gtrends_lst)
    n_terms <- 10
    
    top_n_related_searches_tbl <- rv$gtrends_lst %>% 
      pluck("related_queries") %>% 
      as_tibble() %>% 
      filter(related_queries == "top") %>% 
      mutate(interest = as.numeric(subject)) %>% 
      
      select(keyword, value, interest) %>% 
      group_by(keyword) %>% 
      arrange(desc(interest)) %>% 
      slice(1:n_terms) %>% 
      ungroup() %>% 
      
      mutate(value = as.factor(value) %>% fct_reorder(interest))
    
    ggplotly(
      top_n_related_searches_tbl %>% 
        ggplot(aes(value, interest, color = keyword)) +
        geom_segment(aes(xend = value, yend = 0)) +
        geom_point() +
        coord_flip() +
        ylab("Interest") +
        xlab(" ") +
        # facet_wrap(~ keyword, nrow = 1, scales = "free_y") +
        theme_tq() +
        scale_color_tq()
      
    )
  })
  
  # Google Trend Report ----
  
  # Filename that includes date and time
  file_path <- reactive({
    file_path <- now() %>% # Sys.Date()
      str_replace_all("[[:punct:]]", "_") %>%
      str_replace(" ", "T") %>%
      str_c("_trends_report.html")
  })
  
  output$report <- downloadHandler(
    # Downloaded file name
    filename = file_path(),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "google_trends_report.Rmd")
      file.copy("google_trends_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(search_terms = input$search_terms,
                     search_period = input$search_period)
      id <- showNotification(
        "Rendering report...", 
        duration = NULL, 
        closeButton = FALSE,
        type = "message"
      )
      on.exit(removeNotification(id), add = TRUE)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        input = "google_trends_report.Rmd",
        output_format = "html_document",
        output_file = file,
        params = params,
        envir = new.env()
      )
    }
  )
  
}

shinyApp(ui = ui, server = server)