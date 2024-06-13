

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

product_exclude_server <- function(id, r, Choices){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$exclude_product, {
      showModal(modalDialog(
        title = "Exclude product",
        pickerInput(ns("p_name"),
                    "Select product to exclude:",
                    choices = Choices(),
                    multiple = TRUE,
                    options = list(
                      `live-search` = TRUE)
                    
                    ),
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("p_confirm"), "Confirm Exclusion")
        )
      ))
    })
    
    # data_p()$p_id
    
    observeEvent(input$p_confirm, {
      product_to_exclude <- input$p_name
      r$product_deleted <- c(r$product_deleted, product_to_exclude)
      
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



