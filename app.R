library(shiny)
library(dplyr)
library(sf)
library(lwgeom)
library(readr)


options(shiny.maxRequestSize = 50*1024^2) # 50 MB upload limit

source('functions.R')

# UI ----
ui <- fluidPage(
  
  fluidRow(
    
    column(
      width = 6,
      
      h1('Links & nodes'),
      
      fileInput(
        inputId = 'dr_gpkg_file',
        label = 'Select a Geopackage file',
        multiple = FALSE,
        accept = '.gpkg',
        buttonLabel = 'Browse ...',
        placeholder = 'No file selected'
      ),
      
      selectInput(
        inputId = 'dr_layer_selected',
        label = 'Select layer containing dr_linkki features',
        choices = c(),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE
      ),
      
      actionButton(
        inputId = 'dr_run_layer_read',
        label = 'Read and validate selected layer'
      ),
      
      actionButton(
        inputId = 'dr_run_layer_transform',
        label = 'Transform links and create nodes'
      ),
      
      downloadButton(
        outputId = 'dr_download_links',
        label = "Download link data",
        icon = icon("download")
      ),
      
      downloadButton(
        outputId = 'dr_download_nodes',
        label = "Download node data",
        icon = icon("download")
      )
    ),
    
    column(
      width = 6,
      
      h1('Stops & route versions'),
      
      fileInput(
        inputId = 'jore_route_files',
        label = 'Select Jore route files',
        multiple = TRUE,
        accept = '.txt',
        buttonLabel = 'Browse ...',
        placeholder = 'No files selected'
      ),
      
      actionButton(
        inputId = 'jore_run_parse',
        label = 'Parse Jore files'
      ),
      
      downloadButton(
        outputId = 'jore_download_stop',
        label = "Download stops",
        icon = icon("download")
      ),
      
      downloadButton(
        outputId = 'jore_download_route_version',
        label = "Download route versions",
        icon = icon("download")
      ),
      
      downloadButton(
        outputId = 'jore_download_stop_on_route',
        label = "Download stops on route",
        icon = icon("download")
      )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # GPKG HANDLING ----
  dr_gpkg_path <- reactive({
    file <- input$dr_gpkg_file
    req(file)
    return(file$datapath)
  })
  
  dr_gpkg_available_layers <- reactive({
    res <- st_layers(dsn = dr_gpkg_path())
    res$name
  })
  
  observe({
    updateSelectInput(inputId = 'dr_layer_selected', choices = dr_gpkg_available_layers())
  })
  
  dr_data <- eventReactive(input$dr_run_layer_read, {
    req(dr_gpkg_path(), input$dr_layer_selected)
    st_read(dsn = dr_gpkg_path(), layer = input$dr_layer_selected, stringsAsFactors = FALSE)
  })
  
  observe({
    dt <- dr_data()
    showModal(modalDialog(
      title = 'Layer validation',
      renderTable(validate_dr_data(dt))
    ))
  })
  
  dr_out <- eventReactive(input$dr_run_layer_transform, {
    req(dr_data())
    transform_dr_data(dr_data())
  })
  
  observeEvent(input$dr_run_layer_transform, {
    req(dr_out())
    li <- dr_out()$link
    nd <- dr_out()$node
    showModal(modalDialog(
      title = 'Result layers',
      h2(sprintf('%d nodes', nrow(nd))),
      renderPrint(summary(nd)),
      h2(sprintf('%d links', nrow(li))),
      renderPrint(summary(li))
    ))
  })
  
  output$dr_download_links <- downloadHandler(
    filename = 'link.csv',
    content = function(file) {write_csv(x = dr_out()$link, file = file)}
  )
  
  output$dr_download_nodes <- downloadHandler(
    filename = 'node.csv',
    content = function(file) {write_csv(x = dr_out()$node, file = file)}
  )
  
  # JORE FILES HANDLING ----
  jore_file_info <- reactive(input$jore_route_files)
  
  jore_res_list <- eventReactive(input$jore_run_parse, {
    req(jore_file_info())
    files <- jore_file_info()$datapath
    names <- jore_file_info()$name
    parse_setof_jore_files(files, names)
  })
  
  observeEvent(input$jore_run_parse, {
    req(jore_res_list())
    st <- jore_res_list()$stop
    rv <- jore_res_list()$route_version
    sor <- jore_res_list()$stop_on_route
    showModal(modalDialog(
      title = 'Jore results',
      h2(sprintf('%d stops', nrow(st))),
      renderPrint(summary(st)),
      h2(sprintf('%d route versions', nrow(rv))),
      renderPrint(summary(rv)),
      h2(sprintf('%d stops on route', nrow(sor))),
      renderPrint(summary(sor))
    ))
  })
  
  output$jore_download_stop <- downloadHandler(
    filename = 'stop.csv',
    content = function(file) {write_csv(x = jore_res_list()$stop, file = file)}
  )
  output$jore_download_route_version <- downloadHandler(
    filename = 'route_version.csv',
    content = function(file) {write_csv(x = jore_res_list()$route_version, file = file)}
  )
  output$jore_download_stop_on_route <- downloadHandler(
    filename = 'stop_on_route.csv',
    content = function(file) {write_csv(x = jore_res_list()$stop_on_route, file = file)}
  )
  
  # CLEANUP ----
  
  session$onSessionEnded(function() {stopApp()})
  
}

shinyApp(ui = ui, server = server)