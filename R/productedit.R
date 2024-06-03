

# UI module ---------------------------------------------------------------

product_exclude_ui <- function(id){
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

product_exclude_server <- function(id, r2, data_p){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    

    
    # Observers
    # observeEvent(input$exclude_product, {
    #   showModal(modalDialog(
    #     title = "",
    #     fluidRow(
    #              h2("Exclude a product")
    #     ),
    #     column(width = 12,
    #            br(),
    #            uiOutput(ns("p_exclude")),
    #            br(),
    #            textOutput(ns("product_to_exclude"))),
    #     footer = tagList(
    #       modalButton("Close"),
    #       actionButton(ns("p_del"), "product_exclude_ui")
    #     )
    #   ))
    # })
    
    observeEvent(input$exclude_product, {
      showModal(modalDialog(
        title = "Exclude product",
        selectInput(ns("p_name"), "Select product to exclude:", choices = data_p()$p_id),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("p_confirm"), "Confirm Exclusion")
        )
      ))
    })
    

    
    observeEvent(input$p_confirm, {
      product_to_exclude <- input$p_name
      r2$product_deleted <- c(r2$product_deleted, product_to_exclude)
      
      removeModal()
      
      sendSweetAlert(
        session = session,
        title = "Success!",
        text = paste("Product", product_to_exclude, "excluded successfully!"),
        type = "success"
      )
    })
    
    
    # output$product_to_exclude <- renderText({
    #   paste0("Product deleted: ",
    #          (paste(input$product_exclude, sep = ", ")) )
    # })
    # 
    
  })
}



