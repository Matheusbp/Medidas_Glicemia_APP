library('shiny')
library("tidyverse")
library("lubridate")
library("shinyTime")
library("gsheet")
library("scales")
library("shinyWidgets")
library("echarts4r")
library("shinydashboard")

#Read google sheets data into R
#autenticando
# gargle::gargle_oauth_client_from_json(path = "./inst/app/www/client_secret_318380545634-46s6g789qttd9va880scpqfob2q0ri28.apps.googleusercontent.com.json")

# tabela <- get_ut('https://docs.google.com/spreadsheets/d/1QqYHWXl4KzhZmHQyzddeWCT6yXC9aHMtYOmPGV-nzEw/edit?usp=sharing')

#pegando tabela com pacote gsheet
url<- construct_download_url(url = 'https://docs.google.com/spreadsheets/d/1QqYHWXl4KzhZmHQyzddeWCT6yXC9aHMtYOmPGV-nzEw/edit?usp=sharing', format = "csv", sheetid = NULL)

tabela <- gsheet2tbl(url, sheetid = NULL) |> tibble()


tabela$Glicemia <- tabela$Glicemia |> as.numeric()
tabela$Gramas_Carbo <- tabela$Gramas_Carbo |> as.numeric()

tabela$Hora <- format(as.POSIXct(tabela$Hora), format = "%H:%M")
tabela$DataHora = as.POSIXct(paste(tabela$Dia, tabela$Hora), format = "%d/%m/%Y %H:%M")


################# UI

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
    )


# Put them together into a dashboardPage
ui <- dashboardPage(
    dashboardHeader(title = "Medidas App"),
    sidebar,
    body
)


server <- function(input, output, session) {
    
    #aux to give initial data values
    tabela1 <- tabela |> filter(Glicemia >0)
    media_medida <- tabela1$Glicemia |> mean(na.rm = T) |> round(0)
    min_medida <- tabela1$Glicemia |> min(na.rm = T) |> round(0)
    max_medida <- tabela1$Glicemia |> max(na.rm = T) |> round(0)
    
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
        
        
        output$plot_medidas <- renderEcharts4r({
        e_charts(df_glicemia, DataHora) |>
          e_area(data = df_glicemia, Glicemia) |>
          e_mark_line(data = list(xAxis = df_glicemia$DataHora |> unique())) |>
          e_labels() |>
          e_mark_line(data = list(yAxis = 120), title = "Meta") |>
          e_mark_line(data = list(yAxis = 60), title = "Hipo") |>
                e_title("Glicemia")
            
              # e_datazoom(x_index = c(0, 1)) # add data zoom for for x axis

        })
        output$plot_insulinas <- renderEcharts4r({

        e_charts(df_insulina, DataHora) |>
          e_bar(data = df_insulina, Humalog) |>
            e_labels() |>
            e_bar(data = df_insulina, Tresiba) |>
            e_legend_unselect("Tresiba") |>
            e_bar(data = df_insulina, Gramas_Carbo) |>
            e_legend_unselect("Gramas_Carbo") |>

          e_title("Insulina Aplicada & \n Carboidratos ingeridos")
        })
    })
    
}

shinyApp(ui, server)
