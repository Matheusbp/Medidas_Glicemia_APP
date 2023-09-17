#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  library('shiny')
  library("tidyverse")
  library("lubridate")
  library("shinyTime")
  library("gsheet")
  library("scales")
  library("shinyWidgets")
  library("echarts4r")
  library("shinydashboard")
  
    # Leave this function for adding external resources
    # golem_add_external_resources(),
    # Your application UI logic
    
    # Put them together into a dashboardPage

  #pegando tabela com pacote gsheet
  url<- construct_download_url(url = 'https://docs.google.com/spreadsheets/d/1QqYHWXl4KzhZmHQyzddeWCT6yXC9aHMtYOmPGV-nzEw/edit?usp=sharing', format = "csv", sheetid = NULL)
  
  tabela <- gsheet2tbl(url, sheetid = NULL) |> tibble()
  
  
  tabela$Glicemia <- tabela$Glicemia |> as.numeric()
  tabela$Gramas_Carbo <- tabela$Gramas_Carbo |> as.numeric()
  
  tabela$Hora <- format(as.POSIXct(tabela$Hora), format = "%H:%M")
  tabela$DataHora = as.POSIXct(paste(tabela$Dia, tabela$Hora), format = "%d/%m/%Y %H:%M")
  
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      dateRangeInput('daterange',
                     label = paste("Escolha o período para ter os dados"),
                     start = Sys.Date()-7, end = Sys.Date()+1,
                     min = "20/07/2023", max = Sys.Date()+1,
                     separator = " - ", format = "dd/mm/yyyy", 
                     language = "pt"
      ),
      pickerInput(
        inputId = "periodo",
        label = "Escolha o período de análise", 
        choices = c("Todos", unique(tabela$Periodo)),
        selected = "Todos",
        options = list(
          title = "This is a placeholder")
      )
    )
  )
    
    body <- shinydashboard::dashboardBody(
      
      fluidRow(
        # Dynamic valueBoxes
        shinydashboard::valueBoxOutput("mean_medida"),#, icon = icon("fa-solid fa-align-center")),
        shinydashboard::valueBoxOutput("max_medida"),#, icon = icon("fa-solid fa-arrow-up-long")),
        shinydashboard::valueBoxOutput("min_medida")#, icon = icon("fa-solid fa-arrow-down-long"))
      ),
      
      fluidRow(
        
        echarts4r::echarts4rOutput("plot_medidas"),
        echarts4r::echarts4rOutput("plot_insulinas")
        # echarts4rOutput("plot_ambos")
        
      )
    )
    
    ui <- dashboardPage(
      dashboardHeader(title = "Medidas App"),
      sidebar = sidebar,
      body = body, 
      title = "Glicemia Matheus"
    )
}
  


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
