
# UI module ---------------------------------------------------------------

save_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    actionBttn(ns("save"),
               label = "Save table",
               icon = icon("floppy-o")),
    useSweetAlert()
  )
}


# SERVER module -----------------------------------------------------------

save_table_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$save, {
      
      data() |>
        saveRDS("./Data/product_table.rds")
      
      sendSweetAlert(
        session = session,
        title = "Table saved!",
        text = "Your product table was saved and updated successfully!",
        type = "info"
      )
      
    })
  })
}

