#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # FAZER O ECHARTS COM DADOS DE OUTRAS COLUNAS 
  # https://www.youtube.com/watch?v=my88ERFUPl8
  
  library('shiny')
  library("tidyverse")
  library("lubridate")
  library("shinyTime")
  library("gsheet")
  library("scales")
  library("shinyWidgets")
  library("echarts4r")
  library("shinydashboard")
  library("RColorBrewer")
  library("DT")
  
  #pegando tabela com pacote gsheet
  url<- construct_download_url(url = 'https://docs.google.com/spreadsheets/d/1QqYHWXl4KzhZmHQyzddeWCT6yXC9aHMtYOmPGV-nzEw/edit?usp=sharing', format = "csv", sheetid = NULL)
  
  tabela <- gsheet2tbl(url, sheetid = NULL) |> tibble()
  
  
  tabela$Glicemia <- tabela$Glicemia |> as.numeric()
  tabela$Gramas_Carbo <- tabela$Gramas_Carbo |> as.numeric()
  
  tabela$Hora <- format(as.POSIXct(tabela$Hora), format = "%H:%M")
  tabela$DataHora = as.POSIXct(paste(tabela$Dia, tabela$Hora), format = "%d/%m/%Y %H:%M")
  
  tabela_show <- tabela
  #fazendo uma tabela que ordene os dads de glicemia e insulina conforme o periodo:
  #
  #1 - quero primeiro ordenar os periodos. A ordem é: jejum, cafe_manha, colacao,
  #pre_almoco, almoço, pre_lanche_tarde, lanche_tarde, pre_janta, janta, antes_dormir
  
  tabela_show$Periodo <- factor(tabela$Periodo, levels = c("jejum", "cafe_manha", "colacao", "pre_almoço", 
                                                      "almoço", "pre_lanche_tarde", "lanche_tarde", "pre_janta", 
                                                      "janta", "antes_dormir", "nota"))

  #transformar a coluna Dia em Date
  # tabela_show$Dia <- as.Date(tabela_show$Dia, format = "%d/%m/%Y")


  #2 - agora quero passar os dados de long format para wide format com os dados de glicemia e humalog 
  #e tresiba lado a lado e os periodos serão as colunas 
  tabela_glicemia <- tabela_show |> 
    pivot_wider(names_from = Periodo, values_from = c(Glicemia), values_fn = max) |>
    select(-c("Tipo", "Hora", "Gramas_Carbo", "Alimentos")) |> as.data.frame() |>
    #tirar todos os 0 e transformar em NA
    mutate(across(everything(), ~ifelse(. == 0, NA, .))) |>
    #deletar as coluna que tem apenas NA
    # select(where(~ !all(is.na(.)))) |>
    select(c(Dia, DataHora, jejum, pre_almoço, pre_lanche_tarde, pre_janta, antes_dormir, Nota))|> 
    arrange(-DataHora)
  
  tabela_humalog <- tabela_show |> 
    pivot_wider(names_from = Periodo, values_from = c(Humalog), values_fn = max) |>
    select(-c("Tipo", "Hora", "Gramas_Carbo", "Alimentos")) |> as.data.frame() |>
    #tirar todos os 0 e transformar em NA
    mutate(across(everything(), ~ifelse(. == 0, NA, .))) |>
    #deletar as coluna que tem apenas NA
    # select(where(~ !all(is.na(.)))) |>
    select(c(Dia, DataHora, cafe_manha, almoço, lanche_tarde, janta)) |> 
    arrange(-DataHora)
  
  tabela_tresiba <- tabela_show |> 
    pivot_wider(names_from = Periodo, values_from = c(Tresiba), values_fn = max) |>
    select(-c("Tipo", "Hora", "Gramas_Carbo", "Alimentos")) |> as.data.frame() |>
    #tirar todos os 0 e transformar em NA
    mutate(across(everything(), ~ifelse(. == 0, NA, .))) |>
    #deletar as coluna que tem apenas NA
    # select(where(~ !all(is.na(.)))) |>
    select(c(Dia, DataHora, cafe_manha)) |> 
    arrange(-DataHora)
  
  #tem mais de uma informação para cada dia, sendo assim, eu preciso quecada linha da tabela 
  #seja unica para cada dia, simplesmente pegando os valores de glicemia e insulina de cada periodo
  #e colocando em uma unica linha para cada dia mas retirando as linhas que tem NAs antes disso
  
  tabela_glicemia1 <- tabela_glicemia |> 
    group_by(Dia) |> 
    filter(!all(is.na(jejum) & is.na(pre_almoço) & is.na(pre_lanche_tarde) & is.na(pre_janta) & is.na(antes_dormir)) ) |> 
    summarise_all(~first(na.omit(.)))|> 
    arrange(-DataHora) |>
    select(-DataHora)
  
  tabela_humalog1 <- tabela_humalog |>
    group_by(Dia) |>
    filter(!all(is.na(cafe_manha) & is.na(almoço) & is.na(lanche_tarde) & is.na(janta)) ) |>
    summarise_all(~first(na.omit(.)))|> 
    arrange(-DataHora) |>
    select(-DataHora)
  
  tabela_tresiba1 <- tabela_tresiba |>
    group_by(Dia) |>
    filter(!all(is.na(cafe_manha))) |>
    summarise_all(~first(na.omit(.)))|> 
    arrange(-DataHora) |>
    select(-DataHora)
  
  
  #agora criarei a tabela_show1 que será usada para fazer uma tabela que mostre os dados 
  #na seguinte ordem: Dia, Jejum - glicemia, café_manha - humalog e tresiba, pre_almoco - glicemia
  #almoco - humalog, pre_lanche_tarde - glicemia, lanche_tarde - humalog, 
  #pre_janta - glicemia, janta - humalog, antes_dormir - glicemia.
  #Estes dados serão puxados das taeblas criadas acima, de glicemia, humalog e tresiba
  
  #para fazer isso, preciso primeiro colocar todos os dias ordenados em uma unica tabela
  #pois tem datas que nao tem dados de diferentes tabelas, depois disso, preciso juntar as tabelas
  #pelos dias e colocar as colunas na ordem que eu quero
  
  tabela_show1 <- tabela_glicemia1 |> 
    full_join(tabela_humalog1, by = "Dia", suffix = c("glicemia", "_humalog")) |> 
    full_join(tabela_tresiba1, by = "Dia", suffix = c("_humalog", "_tresiba"))
  
  names(tabela_show1)
  
  
  tabela_show1 <- tabela_show1 |> 
    select(c(Dia, jejum, cafe_manha_tresiba, cafe_manha_humalog,
             pre_almoço, almoço, pre_janta, janta, antes_dormir, Nota)) |>
    #substituir NAs por "-"
    mutate(across(everything(), ~ifelse(is.na(.), "-", .)))
  
  
  # library(RColorBrewer)
  # library(DT)
  # dt <- datatable(tabela_show1, options = list(
  #   rownames = FALSE,
  #   ordering = FALSE,
  #   searching = FALSE,
  #   dom = 'Bfrtip',
  #   buttons = 
  #     list('copy', 'print', list(
  #       extend = 'collection',
  #       buttons = c('csv', 'excel', 'pdf'),
  #       text = 'Download'
  #     ))
  # )
  # ) |>
  #   formatStyle(
  #     c('jejum', 'pre_almoço', 'pre_janta', 'antes_dormir'),
  #     color = styleInterval(c(0,60, 140, 200, 300), brewer.pal(6, "YlOrRd")),
  #     # color = 'black',
  #     fontWeight = 'bold'
  #     ) |>
  #   formatStyle(
  #     c('cafe_manha_tresiba', 'cafe_manha_humalog', 'almoço', 'janta'),
  #     fontWeight = 'bold',
  #     color = 'black')
  # 
  # saveWidget(dt, "tabela.html", selfcontained = TRUE)
  
  #servidor
  server <- {
    
    
    #aux to give initial data values
    tabela_aux <- tabela
    
    tabela_aux <- tabela_aux |> filter(Sys.Date()-7 <= DataHora &
                             DataHora <= Sys.Date()+1)
    
    media_medida <- tabela_aux$Glicemia |> mean(na.rm = T) |> round(0)
    min_medida <- tabela_aux$Glicemia |> min(na.rm = T) |> round(0)
    max_medida <- tabela_aux$Glicemia |> max(na.rm = T) |> round(0)
    
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
          e_mark_line(data = list(yAxis = 150), title = "Meta") |>
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
      output$tabelaDT <- renderDataTable({
        
        datatable(tabela_show1, 
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
            ordering = FALSE,
            searching = FALSE,
            pageLength = 20,
            dom = 'Bfrtip',
            buttons =
              list(list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              ))
          )
          ) |>
            formatStyle(
              c('jejum', 'pre_almoço', 'pre_janta', 'antes_dormir'),
              color = styleInterval(c(0,60, 140, 200, 300), brewer.pal(6, "YlOrRd")),
              # color = 'black',
              fontWeight = 'bold'
              ) |>
            formatStyle(
              c('cafe_manha_tresiba', 'cafe_manha_humalog', 'almoço', 'janta'),
              fontWeight = 'bold',
              color = 'black')
        
      })
      
    })
    
  }
  
}

shinyApp(app_ui, app_server)

# shinyApp(ui = "./R/app_ui", server = "./R/app_server")



