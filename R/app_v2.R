library('shiny')
library('DT')
library('xlsx')
library("tidyverse")
library("lubridate")
library("shinyTime")
library("gsheet")
library("scales")

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

ui <- fluidPage(
    titlePanel("Medições de Glicemia"),
    sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
            plotOutput("glucose_plot"),
            dataTableOutput("glucose_table")
        )
    )
)

server <- function(input, output, session) {
    
    # Reactive values to store data
    data <- reactiveValues(table = data.frame())
    plot_data <- tabela
    
    # plot_data$DataHora <- format(plot_data$DataHora,format='%Y%m%d %H:%M')
    # Update the plot
    output$glucose_plot <- renderPlot({
        # plot_data <- data$table
        # plot_data$DataHora <- as.POSIXct(paste(plot_data$Dia, plot_data$Hora), format = "%Y/%m/%d %H:%M")
        
        # terei dados de glicemia e insulinas separados
        df_glicemia <- plot_data |> filter(Tipo == "Medida")
        df_insulina <- plot_data |> filter(Tipo == "Alimentação/Aplicação")
        
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

            geom_col(data = df_insulina, aes(x = DataHora,y = Humalog, fill = "Humalog"),
                     position = position_nudge(x = -0.1)) +

            geom_col(data = df_insulina, aes(x = DataHora, y = Tresiba, fill = "Tresiba"),
                     position = position_nudge(x = 0.1)) +
            
            scale_fill_manual(values = c(Humalog = "#1f77b4", Tresiba = "#ff7f0e")) +
            scale_color_manual(values = c(Glicemia = "#1f77b4")) +

            #Here is where I format the x-axis
            geom_vline(xintercept = dias$x)+
            scale_x_datetime(labels = date_format("%Y-%m-%d %H"),
                             date_breaks = "1 day") +
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
                legend.position = "top",
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
        p
        
        dias = data.frame(x = seq(from = min(plot_data$DataHora), 
                               by = "1 day", 
                               to = max(plot_data$DataHora)), y=1)
        library(echarts4r)
          e_charts(df_glicemia, DataHora) |>
          e_area(data = df_glicemia, Glicemia) |>
          e_mark_line(data = list(xAxis = df_glicemia$DataHora |> unique())) |>
          e_labels() |>
          e_mark_line(data = list(yAxis = 120), title = "Meta") |>
          e_mark_line(data = list(yAxis = 60), title = "Hipo") |>
              e_datazoom(x_index = c(0, 1)) # add data zoom for for x axis
          
          
          
          e_bar(Glicemia) |>
          e_labels() |>
          e_title("Line and area charts")
    })
    
    # Display the data table
    output$glucose_table <- renderDataTable({
        datatable(data$table, options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
    })
}

shinyApp(ui, server)
