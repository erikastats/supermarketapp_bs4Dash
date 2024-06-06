
# UI module ---------------------------------------------------------------

save_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    actionBttn(ns("save"),
               label = "Save table",
               icon = icon("floppy-disk")),
    useSweetAlert()
  )
}


# SERVER module -----------------------------------------------------------

save_table_server <- function(id, data, path_data, type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$save, {
      # "./Data/product_table.rds"
      data() |>
        saveRDS(path_data)
      
      sendSweetAlert(
        session = session,
        title = "Table saved!",
        text = paste0("Your ", str_to_lower(type) , " table was saved and updated successfully!"),
        type = "info"
      )
      
    })
  })
}

