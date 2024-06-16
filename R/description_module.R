library(echarts4r)
library(readr)
library(dplyr)



# UI module ---------------------------------------------------------------

description_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("descr"))
  )
}


# Server module -----------------------------------------------------------

description_server <- function(id, text_number,
                                  text_header,
                                  text_title,
                                  arrow_icon){
  moduleServer(id, function(input, output, session){
    
    # output
    output$descr <- renderUI({
      
      if (arrow_icon == 'up'){
        direction_arrow = 'caret-up'
        color_number = 'pink'
      } else {
        direction_arrow = 'caret-down'
        color_number = 'green'
      }
      
      descriptionBlock(
        number =   text_number, #"17%", 
        numberColor = color_number, #"pink", 
        numberIcon = icon(direction_arrow), #icon("caret-up"),
        header = text_header, #"$35,210.43", 
        text = text_title, #"TOTAL REVENUE", 
        rightBorder = TRUE,
        marginBottom = FALSE
      )
    })
    
    
  })
}



