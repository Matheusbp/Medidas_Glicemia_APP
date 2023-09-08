#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  library('shiny')
  library('DT')
  library('xlsx')
  library("tidyverse")
  library("lubridate")
  library("shinyTime")
  library('googlesheets4')
  
  #Read google sheets data into R
  x <- read_sheet('https://docs.google.com/spreadsheets/d/1J9-ZpmQT_oxLZ4kfe5gRvBs7vZhEGhSCIpNS78XOQUE/edit?usp=sharing')
  
  setwd("C:/Users/mathe/OneDrive/Documents/Coding/projects/medidas_app")
  # Função para salvar a tabela em um arquivo Excel
  save_table <- function(data) {
    write.xlsx(data, "www/medidas1.xlsx", row.names = FALSE)
  }
  
  tabela <- read.xlsx("www/medidas.xlsx", sheetIndex = 1) |> tibble()
  tabela$Glicemia <- tabela$Glicemia |> as.numeric()
  tabela$Gramas_Carbo <- tabela$Gramas_Carbo |> as.numeric()
  
  tabela$Hora <- format(as.POSIXct(tabela$Hora), format = "%H:%M")
  
  tabela$DataHora = as.POSIXct(paste(tabela$Dia, tabela$Hora), format = "%Y-%m-%d %H:%M")
  
  server <- function(input, output, session) {
    
    # Reactive values to store data
    data <- reactiveValues(table = data.frame())
    plot_data <- tabela
    
    # Update input panel based on selected choice
    observeEvent(input$input_type, {
      if (input$input_type == "Hora de comer") {
        output$input_panel <- renderUI({
          fluidRow(
            column(6, dateInput("date", "Dia:", value = Sys.Date())),
            column(6, timeInput("time", "Hora:", value = format(Sys.time(), "%H:%M"))),
            column(12, selectInput("periodo", "Período:",
                                   choices = c("cafe_manha", "colação", "almoço", "lanche_tarde", "jantar", "ceia"))),
            column(4, numericInput("humalog", "Quantidade Humalog:", value = 0)),
            column(4, numericInput("tresiba", "Quantidade Tresiba:", value = 0)),
            column(4, numericInput("gramas_carbo", "Gramas de Carboidrato:", value = 0)),
            column(12, textInput("alimentos", "Alimentos:", value = "")),
            column(12, textInput("nota", "Nota:", value = ""))
          )
        })
      } else {
        output$input_panel <- renderUI({
          fluidRow(
            column(6, dateInput("date", "Dia:", value = Sys.Date())),
            column(6, timeInput("time", "Hora:", value = format(Sys.time(), "%H:%M"))),
            column(6, selectInput("periodo", "Período:",
                                  choices = c("jejum", "pré-almoço", "pré_lanche_tarde", "pré-janta", "antes_de_dormir"))),
            column(6, numericInput("glicemia", "Glicemia:", value = 0)),
            column(6, numericInput("humalog", "Quantidade Humalog:", value = 0)),
            column(6, numericInput("tresiba", "Quantidade Tresiba:", value = 0)),
            column(12, textInput("nota", "Nota:", value = ""))
          )
        })
      }
    })
    
    # Observe the submit button and update the data table
    observeEvent(input$submit_btn, {
      new_entry <- data.frame(
        Dia = format(input$date, "%Y/%m/%d"),
        Hora = format(input$time, "%H:%M"),
        Periodo = input$periodo,
        Glicemia = ifelse(input$input_type == "Hora de aplicar insulina", input$glicemia, NA),
        Humalog = input$humalog,
        Tresiba = input$tresiba,
        Gramas_Carbo = input$gramas_carbo,
        Alimentos = ifelse(input$input_type == "Hora de comer", input$alimentos, ""),
        Nota = input$nota,
        DataHora = as.POSIXct(paste(format(input$date, "%Y/%m/%d"),
                                    format(input$time, "%H:%M")),
                              format = "%Y-%m-%d %H:%M")
        
      )
      data$table <- rbind(tabela, data$table, new_entry)
      plot_data <- data$table
    })
    
    # Observe the save button and save the data to Excel file
    observeEvent(input$save_btn, {
      save_table(data$table)
    })
    
    # Download the Excel file
    output$download_btn <- downloadHandler(
      filename = function() {
        "medidas1.xlsx"
      },
      content = function(file) {
        save_table(data$table)
        file.copy("www/medidas1.xlsx", file)
      }
    )
    library(scales)
    # plot_data$DataHora <- format(plot_data$DataHora,format='%Y%m%d %H:%M')
    # Update the plot
    output$glucose_plot <- renderPlot({
      # plot_data <- data$table
      # plot_data$DataHora <- as.POSIXct(paste(plot_data$Dia, plot_data$Hora), format = "%Y/%m/%d %H:%M")
      
      # terei dados de glicemia e insulinas separados
      df_glicemia <- plot_data |> filter(is.na(Glicemia) == F)
      df_insulina <- plot_data |> filter(is.na(Humalog) == F &
                                           is.na(Tresiba) == F)
      
      #para pinntar o fundo
      d = data.frame(x = seq(from = min(plot_data$DataHora), 
                             by = "1 hour", 
                             to = max(plot_data$DataHora)), y = 1)
      d$hour = as.numeric(format(d$x, "%H"))
      d$dia = as.numeric(format(d$x, "%d"))
      
      dias <- d |> filter(hour == 0)
      ggplot() +
        geom_line(data = df_glicemia, aes(x = DataHora, y = Glicemia, color = "Glicemia"),
                  linewidth = 1) +
        #Here is where I format the x-axis
        geom_vline(xintercept = dias$x)
      scale_x_datetime(labels = date_format("%Y-%m-%d %H"),
                       date_breaks = "6 hours") +
        
        geom_col(data = df_insulina, aes(x = DataHora,y = Humalog, fill = "Humalog"),
                 width = 0.2, position = position_nudge(x = -0.1)) +
        
        geom_col(data = df_insulina, aes(x = DataHora, y = Tresiba, fill = "Tresiba"),
                 width = 0.2, position = position_nudge(x = 0.1)) +
        scale_fill_manual(values = c(Humalog = "#1f77b4", Tresiba = "#ff7f0e")) +
        scale_color_manual(values = c(Glicemia = "#1f77b4")) +
        
        
        #pintando o fundo
        # geom_rect(data = d, aes(xmin = min(x), xmax = max(x), ymin = -Inf, ymax = Inf,
        #               fill = hour_shade))
        
        labs(
          x = "Data e Hora",
          y = "Valores",
          color = NULL,
          fill = NULL
        ) +
        # theme_minimal() +
        theme(
          legend.position = "top"
        )
      p
      
      # library(echarts4r)
      # 
      # a <- d 
      # a$x <- as.character(a$x)
      # dias <- a |> filter(hour == 0)
      # # dias$hor <- as.POSIXct(dias$x, format = "%Y-%m-%d %H:%M")
      # 
      # df <- data.frame(
      #   x = seq(50),
      #   y = rnorm(50, 10, 3),
      #   z = rnorm(50, 11, 2),
      #   w = rnorm(50, 9, 2)
      # )
      # 
      # 
      #   e_charts(df_glicemia, DataHora) |>
      #   e_area(data = df_glicemia, Glicemia) |>
      #     e_mark_line(data = list(xAxis = dias$x))
      #   # e_bar(Glicemia) |> 
      #   e_labels() |>
      #   e_title("Line and area charts")
    })
    
    # Display the data table
    output$glucose_table <- renderDataTable({
      datatable(data$table, options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
    })
  }
  
  shinyApp(ui, server)
  
}
