pacman::p_load('tidyverse','shiny','shinythemes','openxlsx','rsconnect')

rsconnect::setAccountInfo(name='<SECRET>',
                          token='<SECRET>',
                          secret='<SECRET>')

# Leitura dos dados
dados <- read.xlsx('ipc.xlsx')
sequenciaDatas <- seq(as.Date("2022-01-01"),
                      as.Date("2022-09-30"),
                      by = "months")
dados$data <- sequenciaDatas

# Calcule as percentagens com base na população estimada
variaveisPercentuais <- colnames(dados)[1:2]
dadosAnalise <- dados
dadosAnalise[variaveisPercentuais] <- (
  dadosAnalise[variaveisPercentuais] * 100)/dados$populacao_estimada

plotarDados1 <- data.frame(
  pobreza = dadosAnalise$pobreza,
  extrema_pobreza = dadosAnalise$extrema_pobreza,
  totais_pobreza = dados$pobreza,
  totais_pobreza_extrema = dados$extrema_pobreza,
  data = dadosAnalise$data
)

plot_pobreza <- ggplot(plotarDados1, aes(x = as.Date(data))) +
  geom_bar(aes(y = pobreza, fill = "Pobreza"), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  geom_bar(aes(y = extrema_pobreza, fill = "Extrema Pobreza"), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  geom_text(aes(y = pobreza, 
                label = sprintf("%.2f%%", pobreza)),
            position = position_dodge(width = 0.9), 
            vjust = 1, hjust = 1, size = 4.0, color = "black") +
  geom_text(aes(y = extrema_pobreza,
                label = sprintf("%.2f%%", extrema_pobreza)),
            position = position_dodge(width = 0.9),
            vjust = 1, hjust = 1, size = 4.0, color = "black") +
  geom_text(aes(y = pobreza, 
                label = sprintf("(%d)", totais_pobreza)),
            position = position_dodge(width = 0.9), 
            vjust = -0.6, hjust = 1, size = 4.0, color = "black") +
  geom_text(aes(y = extrema_pobreza, 
                label = sprintf("(%d)", totais_pobreza_extrema)),
            position = position_dodge(width = 0.9),
            vjust = -0.6, hjust = 1, size = 4.0, color = "black") +
  labs(title ="NÚMERO DE CADASTROS NO CAD ÚNICO: FAMÍLIAS EM POBREZA E EXTREMA POBREZA",
       x = "DATA",
       y = "FREQUÊNCIA RELATIVA") +
  scale_fill_manual(
    values = c("Pobreza" = "red", "Extrema Pobreza" = "darkgray"),
    name = NULL) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               labels = element_text(size = 14)) + # Aumenta o tamanho do texto do eixo X
  theme(axis.text.x = element_text(size = 14, angle = 45),
        axis.text.y = element_text(size = 14),  # Aumenta o tamanho do label do eixo Y
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(color = "blue", size = 18,hjust = 0.2)) +  # Ajusta cor e tamanho do título
  coord_flip()

# Reformular os dados
plotarDados2 <- data.frame(
  familias_pobreza = dadosAnalise$familias_pobreza,
  familias_extrema_pobreza = dadosAnalise$familias_extrema_pobreza,
  quilombolas_pobreza = dadosAnalise$quilombolas_pobreza,
  quilombolas_extrema_pobreza = dadosAnalise$quilombolas_extrema_pobreza,
  ciganos_pobreza = dadosAnalise$ciganos_pobreza,
  ciganos_extrema_pobreza = dadosAnalise$ciganos_extrema_pobreza,
  data = dadosAnalise$data
)

plotarDados2_long <- gather(plotarDados2,
                            key = "variavel",
                            value = "valor", -data)
# Filtrar dados por variável
familias_dados <- plotarDados2_long[grep("familias",
                                         plotarDados2_long$variavel), ]
quilombolas_dados <- plotarDados2_long[grep("quilombolas",
                                            plotarDados2_long$variavel), ]
ciganos_dados <- plotarDados2_long[grep("ciganos", 
                                        plotarDados2_long$variavel), ]

# Define UI
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_selecao", "SELECIONE O GRÁFICO",
                  choices = c(
                    "Cadastros Gerais",
                    "Famílias Comuns",
                    "Famílias Quilombolas",
                    "Famílias de Ciganos")
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Define server
server <- function(input, output) {
  observeEvent(input$tipo_selecao, {
    if (input$tipo_selecao == "Cadastros Gerais") {
      output$plot <- renderPlot({
        plot_pobreza
      })
    } else if (input$tipo_selecao == "Famílias Comuns") {
      output$plot <- renderPlot({
        criar_grafico(familias_dados, "NÚMERO DE FAMÍLIAS COMUNS EM ESTADO DE POBREZA E EXTREMA POBREZA")
      })
    } else if (input$tipo_selecao == "Famílias Quilombolas") {
      output$plot <- renderPlot({
        criar_grafico(quilombolas_dados, "NÚMERO DE FAMÍLIAS QUILOMBOLAS EM ESTADO DE POBREZA E EXTREMA POBREZA")
      })
    } else if (input$tipo_selecao == "Famílias de Ciganos") {
      output$plot <- renderPlot({
        criar_grafico(ciganos_dados, "NÚMERO DE FAMÍLIAS CIGANAS EM ESTADO DE POBREZA E EXTREMA POBREZA")
      })
    }
  })
}


# Função para criar o gráfico
criar_grafico <- function(dados, titulo) {
  ggplot(dados, 
         aes(x = data, 
             y = valor, 
             color = variavel,
             linetype = variavel,
             label = valor)) +
    geom_line() +
    geom_point(size = 3) +
    geom_text(aes(y = valor, label = valor), 
              vjust = -0.5,
              size = 4, 
              color = "black") +  # Cor do texto
    labs(title = titulo,
         x = "DATA",
         y = "TOTAIS") +
    scale_color_manual(values = c("darkgray", "#FF5733"),name = NULL) +  # Cores vivas
    scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 12),
          plot.margin = unit(c(1,1,1,1), "lines"),
          plot.title = element_text(color = "blue", size = 18),
          panel.grid.major = element_blank(),# Remove linhas de grade principais
          panel.grid.minor = element_blank(),# Remove linhas de grade secundárias
          panel.background = element_blank(),  # Remove fundo do painel
          axis.line = element_line(colour = "black"))+  # Cor das linhas dos eixos
    expand_limits(x = c(min(dados$data) - weeks(1), max(dados$data) + weeks(1)),
                  y = c(min(dados$valor), max(dados$valor) + 10))
}

transformar_dados_longos <- function(dados) {
  dados_longos <- gather(dados, key = "variavel", value = "valor", -data)
  return(dados_longos)
}

# Transformar dados para formato longo
plotarDados2_long <- transformar_dados_longos(plotarDados2)


# Run the application 
shinyApp(ui = ui, server = server)
# Deploy the application
