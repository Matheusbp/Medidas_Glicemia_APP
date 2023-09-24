#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  FAZER O ECHARTS COM DADOS DE OUTRAS COLUNAS 
  https://www.youtube.com/watch?v=my88ERFUPl8
  
  library('shiny')
  library("tidyverse")
  library("lubridate")
  library("shinyTime")
  library("gsheet")
  library("scales")
  library("shinyWidgets")
  library("echarts4r")
  library("shinydashboard")
  
  #pegando tabela com pacote gsheet
  url<- construct_download_url(url = 'https://docs.google.com/spreadsheets/d/1QqYHWXl4KzhZmHQyzddeWCT6yXC9aHMtYOmPGV-nzEw/edit?usp=sharing', format = "csv", sheetid = NULL)
  
  tabela <- gsheet2tbl(url, sheetid = NULL) |> tibble()
  
  
  tabela$Glicemia <- tabela$Glicemia |> as.numeric()
  tabela$Gramas_Carbo <- tabela$Gramas_Carbo |> as.numeric()
  
  tabela$Hora <- format(as.POSIXct(tabela$Hora), format = "%H:%M")
  tabela$DataHora = as.POSIXct(paste(tabela$Dia, tabela$Hora), format = "%d/%m/%Y %H:%M")
  
  server <- {
    
    #aux to give initial data values
    media_medida <- tabela$Glicemia |> mean(na.rm = T) |> round(0)
    min_medida <- tabela$Glicemia |> min(na.rm = T) |> round(0)
    max_medida <- tabela$Glicemia |> max(na.rm = T) |> round(0)
    
    event_trigger <- reactive({
      list(input$daterange, input$periodo)
    })
    
    observeEvent(event_trigger(), {
      
      # Reactive values to store data
      # Pegando o período correto
      
      if(input$periodo == "Todos"){
        dado <- tabela
      }else{
        dado <- tabela |> filter(Periodo == input$periodo)    
      }
      #filtrando pelo daterange
      dado <- dado |> filter(input$daterange[1] <= DataHora &
                               DataHora <= input$daterange[2])
      
      #dados para o dash
      media_medida <- dado$Glicemia[dado$Glicemia>0] |> mean(na.rm = T) |> round(0)
      min_medida <- dado$Glicemia[dado$Glicemia>0] |> min(na.rm = T) |> round(0)
      max_medida <- dado$Glicemia[dado$Glicemia>0] |> max(na.rm = T) |> round(0)
      
      ##### pegando os boxes do dashboard
      output$mean_medida <- renderValueBox({
        valueBox(
          paste0(media_medida), "Média", icon = icon("glyphicon-resize-horizontal", lib = "glyphicon"),
          color = ifelse(media_medida <= 120, "blue", "maroon")
        )
      })
      
      output$max_medida <- renderValueBox({
        valueBox(
          paste0(max_medida), "Valor máximo \n do período", icon = icon("glyphicon-arrow-up", lib = "glyphicon"),
          color = "red"
        )
      })
      
      output$min_medida <- renderValueBox({
        valueBox(
          paste0(min_medida), "Valor mínimo \n do período", icon = icon("glyphicon-arrow-down", lib = "glyphicon"),
          color = "aqua"
        )
      })
      
      ##### plotando os dados
      
      # print(input$daterange[1])
      # print(data$DataHora)
      plot_data <- dado
      
      
      # terei dados de glicemia e insulinas separados
      df_glicemia <- plot_data |> filter(Tipo == "Medida") |> filter(Glicemia > 0)
      df_insulina <- plot_data |> filter(Tipo == "Alimentação/Aplicação")
      
      
      #fazendo os plots
      
      #ambos
      # plot_data |> 
      #     e_charts(DataHora) |> 
      #     e_area(Glicemia) |> 
      #     e_labels() |>
      #     e_bar(Humalog, x_index = 1, y_index = 1) |>
      #     e_labels() |>
      #     e_bar(Tresiba, x_index = 1, y_index = 1) |>
      #     e_legend_unselect("Tresiba") |>
      #     e_bar(Gramas_Carbo, x_index = 1, y_index = 1) |>
      #     e_legend_unselect("Gramas_Carbo") |>
      #     e_grid(height = "35%") |> 
      #     e_grid(height = "35%", top = "50%") |>
      #     e_y_axis(gridIndex = 1) |> # put x and y on grid index 1
      #     e_x_axis(gridIndex = 1)
      
      dia1 <- plot_data$DataHora |> min() |> floor_date() |> format('%Y-%m-%d %H:%M:%S')
      dia2 <- plot_data$DataHora |> max() |> ceiling_date() |> format('%Y-%m-%d %H:%M:%S')
      
      # dia2 <- df_glicemia$DataHora |> max() |> round(units = "days")
      
      
      output$plot_medidas <- renderEcharts4r({
        e_charts(df_glicemia, DataHora) |>
          e_area(data = df_glicemia, Glicemia) |>
          e_mark_line(data = list(xAxis = df_glicemia$DataHora |> unique())) |>
          e_labels() |>
          # e_tooltip(trigger = "axis") |>
          e_add_nested('extra', Periodo) |>
          e_mark_line(data = list(yAxis = 120), title = "Meta") |>
          e_mark_line(data = list(yAxis = 60), title = "Hipo") |>
          e_title("Glicemia") |>
          #dia 
          e_x_axis(min = dia1, max = dia2) |>
          e_tooltip(
            formatter = htmlwidgets::JS(
              'function(params){
        return "<span><strong> Período: " + params.data.extra.Periodo + "</span></strong>";
      }'
            )
          )
        
        # e_datazoom(x_index = c(0, 1)) # add data zoom for for x axis
        
      })
      output$plot_insulinas <- renderEcharts4r({
        
        e_charts(df_insulina, DataHora) |>
          e_bar(data = df_insulina, Humalog) |>
          e_bar(serie = Gramas_Carbo) |>
          e_bar(serie =  Tresiba) |>
          e_labels() |>
          # e_tooltip(trigger = "axis") |>
          # e_theme("bee-inspired")|>
          e_add_nested('extra', Periodo, Alimentos) |>
          #dia 
          e_x_axis(min = dia1, max = dia2) |>
          # e_add_nested('extra', Alimentos) |>
          e_tooltip(axisPointer = list(
            type = "cross",
            trigger = "axis",
            axis = "x"
          ),
          formatter = htmlwidgets::JS("
                                         function(params){
        return('Período: ' + params.data.extra.Periodo + 
               '<br />Alimento: ' + params.data.extra.Alimentos)
      }
                                                  
                                                  ")
          )|>
          e_legend_unselect("Tresiba") |>
          e_legend_unselect("Gramas_Carbo") |>
          
          e_title("Insulina Aplicada & \n Carboidratos ingeridos")
      })
    })
    
  }
  
}

# shinyApp(app_ui, app_server)
