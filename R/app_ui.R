#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    sidebar <- dashboardSidebar(
      sidebarMenu(
        dateRangeInput('daterange',
                       label = paste("Escolha o período para ter os dados"),
                       start = Sys.Date()-30, end = Sys.Date(),
                       min = "20/07/2023", max = Sys.Date(),
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
    ),
    
    body <- dashboardBody(
      
      fluidRow(
        # Dynamic valueBoxes
        valueBoxOutput("mean_medida"),#, icon = icon("fa-solid fa-align-center")),
        valueBoxOutput("max_medida"),#, icon = icon("fa-solid fa-arrow-up-long")),
        valueBoxOutput("min_medida")#, icon = icon("fa-solid fa-arrow-down-long"))
      ),
      
      fluidRow(
        
        echarts4rOutput("plot_medidas"),
        echarts4rOutput("plot_insulinas")
        # echarts4rOutput("plot_ambos")
        
      )
    ),
    
    
    # Put them together into a dashboardPage
    ui <- dashboardPage(
      dashboardHeader(title = "Medidas App"),
      sidebar,
      body
    )
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
