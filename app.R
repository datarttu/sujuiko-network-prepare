library(shiny)
library(dplyr)
library(sf)
library(readr)


options(shiny.maxRequestSize = 50*1024^2) # 50 MB upload limit

# FUNCTIONS ----

#' Validate Digiroad link dataset
#'
#' @param x dr_linkki layer, returned by `st_read()`
#'
#' @return A data.frame of validation results
validate_dr_data <- function(x) {
  d <- data.frame(
    condition = 'Is not empty',
    result = (nrow(x) > 0),
    stringsAsFactors = FALSE
  )
  d <- rbind(d, list(condition = 'Has fields LINK_ID, AJOSUUNTA, TIENIMI_SU', 
                     result = all(c('LINK_ID', 'AJOSUUNTA', 'TIENIMI_SU') %in% colnames(x))))
  d <- rbind(d, list(condition = 'Has LINESTRING geometry',
                     result = all(st_geometry_type(x) == 'LINESTRING')))
  d <- rbind(d, list(condition = 'AJOSUUNTA has values 2, 3, 4 only',
                     result = 'AJOSUUNTA' %in% colnames(x) & all(sort(unique(x$AJOSUUNTA)) == c(2, 3, 4))))
  d <- rbind(d, list(condition = 'LINK_ID has unique values',
                     result = 'LINK_ID' %in% colnames(x) & !any(duplicated(x$LINK_ID))))
  return(d)
}

#' Transform from Digiroad links to sujuiko links and nodes
#'
#' @param x dr_linkki layer, returned by `st_read()`
#'
#' @return A list with `link` and `node` data.frames
transform_dr_data <- function(x) {
  link <- x %>%
    mutate(link_id = as.integer(LINK_ID),
           i_node = NA_integer_,
           j_node = NA_integer_,
           oneway = case_when(
             AJOSUUNTA == 2 ~ FALSE,
             AJOSUUNTA == 3 ~ TRUE,
             AJOSUUNTA == 4 ~ TRUE,
             TRUE ~ NA
           ),
           link_modes = '{"bus"}',
           link_label = TIENIMI_SU,
           data_source = 'Digiroad',
           source_date = format(Sys.Date(), '%Y-%m-%d'),
           geom = if_else(AJOSUUNTA == 3, st_cast(st_reverse(geom), 'GEOMETRY'), st_cast(geom, 'GEOMETRY'))) %>%
    .[, c('link_id', 'i_node', 'j_node', 'oneway', 'link_modes', 'link_label', 'data_source', 'source_date', 'geom')] %>%
    mutate(geom = st_cast(geom, 'LINESTRING'))
  
  link_inode <- link %>%
    select(link_id, geom) %>%
    mutate(geom = st_startpoint(geom))
  
  link_jnode <- link %>%
    select(link_id, geom) %>%
    mutate(geom = st_endpoint(geom))
  
  node <- rbind(link_inode, link_jnode) %>%
    select(geom) %>%
    distinct() %>%
    # Number the nodes by X and Y order
    mutate(nd_x = st_coordinates(geom)[, 'X'], nd_y = st_coordinates(geom)[, 'Y']) %>%
    arrange(nd_x, nd_y) %>%
    mutate(node_id = row_number()) %>%
    select(node_id, geom)
  
  link_inode_noded <- st_join(x = link_inode, y = node, join = st_intersects) %>%
    st_drop_geometry() %>%
    select(link_id, i_node = node_id)
  
  link_jnode_noded <- st_join(x = link_jnode, y = node, join = st_intersects) %>%
    st_drop_geometry() %>%
    select(link_id, j_node = node_id)
  
  link <- link %>%
    left_join(y = link_inode_noded, by = 'link_id') %>%
    left_join(y = link_jnode_noded, by = 'link_id') %>%
    select(link_id, i_node, j_node, oneway, link_modes, link_label, data_source,
           source_date, geom) %>%
    mutate(geom_wkt = st_as_text(geom)) %>%
    st_drop_geometry()
  
  node <- node %>%
    mutate(geom_wkt = st_as_text(geom)) %>%
    st_drop_geometry()
  
  return(list(link = link, node = node))
}

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
        multiple = FALSE,
        accept = '.txt',
        buttonLabel = 'Browse ...',
        placeholder = 'No files selected'
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
  
  dr_out <- withProgress(eventReactive(input$dr_run_layer_transform, {
    req(dr_data())
    transform_dr_data(dr_data())
  }))
  
  output$dr_download_links <- downloadHandler(
    filename = 'link.csv',
    content = function(file) {write_csv(x = dr_out(), path = file)}
  )
  
  session$onSessionEnded(function() {stopApp()})
  
}

shinyApp(ui = ui, server = server)