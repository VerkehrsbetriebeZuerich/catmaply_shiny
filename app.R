library(shiny)
library(shinyjs)
library(catmaply)
library(plotly)
library(DT)

## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Dev Dashboard VBZ"),
  dashboardSidebar(
    sidebarMenu(
      selectInput(
        inputId = "sample_id", 
        label = "Sample:",
        choices = c(
          "Sample 1" = 1,
          "Sample 2" = 2,
          "Sample 3" = 3,
          "Sample 4" = 4,
          "Sample 5" = 5
        ),
        selected = "Sample 1"
      )#,
      # menuItem("Sales dashboard",tabName = "dashboard"),
      # menuSubItem("DevOps",tabName = "DevOps"),
      # menuSubItem("Blockchain",tabName = "Blockchain"),
      # menuSubItem("AWS",tabName = "AWS")
    )
  ),
  dashboardBody(
    useShinyjs(),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Heatmap",
        plotlyOutput("heatmap", height = "700px"),
        width = 8,
        height = "800px"
      ),
      box(
        title = "boxplot",
        plotlyOutput("boxplot", height = "700px"),
        width = 4,
        height = "800px"
      )
    ),
    fluidRow(
      box(
        title = "Plotly Click Event",
        verbatimTextOutput("plotly_click_event_data"),
        width = 8
      ),
      box(
        title = "Plotly Relayout Event",
        verbatimTextOutput("plotly_relayout_event_data"),
        width = 4
      )
    ),
    fluidRow(
      box(
        title = "Hist",
        plotlyOutput("hist"),
        width = 8
      ),
      box(
        title = "Underlying Data",
        DT::dataTableOutput("raw_data"),
        width = 4
      )
    )
    
    # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
    # note that "A" needs to be replaced with plotly source string if used
    # extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }"),
    # actionButton("reset", "Reset plotly click value"),
  )
)

server <- shinyServer(function(input, output) {
  
  
  data(vbz)
  
  x <- "fahrt_seq"
  y = "Haltestellenlangname"
  y_order = "halt_seq"
  z = "Besetzung"
  
  id <- reactive({
    as.numeric(input$sample_id)
  })
  
  main_plot_data <- reactive({
    vbz[[id()]]$data
  })
  
  plotly_click_data <- reactive({
    event_data("plotly_click", source = "catmaply")
  })
  
  plotly_relayout_data <- reactive({
    event_data("plotly_relayout", source = "catmaply")
  })
  
  
  sub_plot_data <- reactive({
    data <- plotly_click_data()
    
    if (!is.null(data))
      filter(main_plot_data(), !!rlang::sym(x) == data[["x"]] & !!rlang::sym(y) == data[["y"]])
    else
      main_plot_data()
  })
  
  output$plotly_click_event_data <- renderPrint({
    plotly_click_data()
  })
  
  output$plotly_relayout_event_data <- renderPrint({
    plotly_relayout_data()
  })
  
  output$heatmap <- renderPlotly({
    
    #plot_ly(mtcars, x=~cyl, y=~mpg)
    plot <- catmaply(
      main_plot_data(),
      x = fahrt_seq,
      y = Haltestellenlangname,
      y_order = halt_seq,
      z = Besetzung,
      categorical_colorbar = T,
      categorical_col = Ausl_Kat
    )
    
    plot
  })
  
  
  output$boxplot <- renderPlotly({
    
    lower <- 0
    upper <- 30
    
    relayout_data <- plotly_relayout_data()
    
    if (!is.null(relayout_data)) {
      if (exists('xaxis.range', where = relayout_data)) {
        lower <- relayout_data$xaxis.range[1]
        upper <- relayout_data$xaxis.range[2]
      } else if (
        exists('xaxis.range[0]', where = relayout_data) &&
        exists('xaxis.range[1]', where = relayout_data)
      ){
        lower <- relayout_data$`xaxis.range[0]`
        upper <- relayout_data$`xaxis.range[1]`
      }
    }
    
    bpd <- dplyr::filter(
      main_plot_data(), 
      dplyr::between(fahrt_seq, lower, upper)
    )
    
    plot_ly() %>%
      add_trace(
        type="box",
        data=bpd,
        y=~Haltestellenlangname,
        x=~Besetzung,
        boxpoints = 'suspectedoutliers',
        notched=TRUE
      ) %>%
      plotly::layout(
        yaxis = list(
          title="",
          fixedrange = TRUE,
          categoryorder="array",
          categoryarray=unique(main_plot_data()[[y]][order(main_plot_data()[[y_order]])])
        )
      )
    
  })
  
  
  output$raw_data <- DT::renderDataTable({
    select(sub_plot_data(), x, y, z)
  })
  
  output$hist <- renderPlotly({
    pd <- sub_plot_data()
    
    if (NROW(pd) > 1)
      plot_ly(x = ~pd[[z]], type = "histogram")
    else
      plot_ly(x = ~rnorm(1000) + pd[[z]][1], type = "histogram")
    
  })
  
  # observeEvent(input$reset, {
  #   js$resetClick()
  # })
})

shinyApp(ui, server)



