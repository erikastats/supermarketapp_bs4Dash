

# UI module ---------------------------------------------------------------

product_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    actionBttn(
      inputId = ns("exclude_product"),
      label = "Exclude products",
      icon = icon("edit")
    ),
    useSweetAlert()
  )
  
  
}


# Server module -----------------------------------------------------------

product_server <- function(id, r, df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    product_id <- reactiveVal({
      df$product_data |>
        pull(p_id) |>
        unique()
    })
    
    # Observers
    observeEvent(input$exclude_product, {
      showModal(modalDialog(
        title = "",
        fluidRow(
                 h2("Exclude a product")
        ),
        column(width = 12,
               br(),
               uiOutput(ns("p_exclude")),
               br(),
               textOutput(ns("product_to_exclude"))),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("p_del"), "Delect product")
        )
      ))
    })
    

    
    all_fields_filled <- reactive({
      !is.null(input$product_exclude) && input$product_exclude != ""
    })
    
    observe({
      toggleState <- all_fields_filled()
      updateActionButton(session, ns("p_del"), disabled = !toggleState)
    })
    
    observeEvent(input$p_del, {
      sendSweetAlert(
        session = session,
        title = "Deleted!",
        text = "Product deleted",
        type = "error"
      )
      
      r$product_deleted <- input$product_exclude
      
    })
    

    
    # Outputs
    output$p_exclude <- renderUI({
      pickerInput(
        inputId = ns("product_exclude"),
        label = "Select product id to be deleted",
        choices = product_id(),
        selected = NULL,
        multiple = TRUE
      )
    })
    
    output$product_to_exclude <- renderText({
      paste0("Product deleted: ",
             (paste(input$product_exclude, sep = ", ")) )
    })
    
    
  })
}



