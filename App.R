# Instala e carrega pacotes (somente uma vez):
if (!require(shiny)) install.packages("shiny"); library(shiny)
if (!require(BlandAltmanLeh)) install.packages("BlandAltmanLeh"); library(BlandAltmanLeh)
if (!require(ggspatial)) install.packages("ggspatial"); library(ggspatial)
if (!require(htmlwidgets)) install.packages("htmlwidgets"); library(htmlwidgets)
if (!require(irr)) install.packages("irr"); library(irr)
if (!require(leaflet)) install.packages("leaflet"); library(leaflet)
if (!require(readxl)) install.packages("readxl"); library(readxl)
if (!require(nortest)) install.packages("nortest"); library(nortest)

# Outros pacotes
library(viridis)
library(cowplot)
library(dplyr)
library(geobr)
library(ggplot2)
library(haven)
library(htmltools)
library(janitor)
library(kableExtra)
library(knitr)
library(lubridate)
library(naniar)
library(sf)
library(tidyverse)

# === Leitura e preparação de dados ===
# Dados do SISAM-CAMS

dadosA=read.csv("PM2.5_diario_2023.csv")

#Criando as colunas data, mês, ano, dia e dia da semana
dadosA$Data=ymd(dadosA$Date)
dadosA$mes=month(dadosA$Data)
dadosA$ano=year(dadosA$Data)
dadosA$wday=weekdays(dadosA$Data)

#Comando para extrair só os dois primeiros dígitos de Cod
dadosA$UF=substr(dadosA$Cod,1,2)

#Filtrando só os dados de São Paulo Cod 35, SP deve ter 234780 linhas
#cada linha é uma medida diária de cada municiipio, de cada dia do ano
SP1Dia=subset(dadosA,dadosA$UF==35)

#Calcula média de  PM2,5 de cada Cod de municipio,  por mês 
#SP1mes tera 7740 linhas que sao 12 meses*645 municipios
SP1Mes <- SP1Dia %>%
  group_by(Cod, mes) %>%
  summarise(pm2.5 = mean(PM2.5, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(CD_MUN = Cod)


#Calcula média Anual de  PM2,5 de cada Cod de municipio,cada municipio tinha 12 observacoes, 
#entao vai calcular a média mensal de cada municipio, 645 linhas
SP1Ano <- SP1Mes %>%
  group_by(CD_MUN) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE)) %>%
  ungroup()


#Calcula média de  PM2,5 de cada mês ,cada municipio tinha observações de 12 meses 
#entao vai calcular a média de cada mês, considerando todos os municipios, 12 linhaS
SP1UF <- SP1Mes %>%
  group_by(mes) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE)) %>%
  ungroup()


#Lendo a base de dados 2 (Donkelaar)
dadosB=read_excel("dados_completos_consolidado_Donkelar.xlsx")

#Filtrando só os casos de SP na base de dados 2
# SP2Mes tera 7740 linhas que sao 12meses*645 municipios
SP2Mes <- subset(dadosB, SIGLA_UF == 35) %>%
  rename(mes = Mes)


#Calcula média de  PM2,5 de cada Cod de municipio,cada municipio tinha 12 observacoes, 
#entao vai calcular a média mensal de cada municipio, 645 linhas
SP2Ano <- SP2Mes %>%
  group_by(CD_MUN) %>%
  summarise(Media_PM25 = mean(Media_PM25, na.rm = TRUE)) %>%
  ungroup()

#Calcula média de  PM2,5 de cada mês ,cada municipio tinha observações de 12 meses 
#entao vai calcular a média de cada mês, considerando tods os municipios, 12 linhaS
#SP2UF deve ter entao 12 linhas, uma média de cada mês
SP2UF <- SP2Mes %>%
  group_by(mes) %>%
  summarise(Media_PM25 = mean(Media_PM25, na.rm = TRUE)) %>%
  ungroup()

# Bases combinadas
baseMes <- inner_join(SP1Mes, SP2Mes, by = c("CD_MUN", "mes"))
baseAno <- inner_join(SP1Ano, SP2Ano, by = "CD_MUN")
baseUF  <- inner_join(SP1UF,  SP2UF,  by = "mes")


# === UI do Shiny ===
ui <- fluidPage(
  titlePanel("Concentração Média de PM2.5 no Estado de São Paulo"),
  sidebarLayout(
    sidebarPanel(
      helpText("Comparação das concentrações médias de PM2.5 no Estado de São Paulo fornecidas pelo CAMS-SISAM e Donkelaar"),
      helpText("Controle das visualizações"),
      checkboxInput("viewmap", "Mostrar Mapa Interativo", value = TRUE),
      sliderInput("bins", "Número de classes do Histograma:", min = 20, max = 100, value = 30)
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Análise de Normalidade",
                 # Texto explicativo antes do gráfico
                 p(strong("Análise de normalidade da distribuição das 7740 médias mensais de PM2,5 obtidas pelo CAMS-SISAM (12 médias mensais para cada um dos 645 municípios).")),
                 verbatimTextOutput("TesteNormalidade"),
                 plotOutput("Histograma"),
                 plotOutput("QQplot")
        ),
        
      
        tabPanel("Boxplots",
                 # Texto explicativo antes do gráfico
                 p(strong("Boxplots das distribuições das médias mensais de PM2.5 para os 465 municípios (CAMS-SISAM e Donkelaar).")),
                 plotOutput("Boxplot")
      ),
        
        tabPanel("Gráfico de Dispersão",
                 p(strong("Gráfico de dispersão conjunta das 7740 médias mensais de PM2,5 obtidas pelo CAMS-SISAM e pelo Donkelaar (12 médias mensais para cada um dos 645 municípios).")),
                 plotOutput("Dispersao")
        ),
        
        tabPanel("Gráfico de Altman-Bland",
                 p(strong("Gráfico de Altman Bland avaliando a concordância das 7740 médias mensais de PM2,5 obtidas pelo CAMS-SISAM e pelo Donkelaar (12 médias mensais para cada um dos 645 municípios) .")),
                 plotOutput("AltmanBland")
        ),
        
      
        tabPanel("Mapas",
                 # Mapas estáticos quando viewmap == FALSE
                                  conditionalPanel(condition = "!input.viewmap",
                                  p(strong("Mapa coroplético estático das médias anuais das medidas de concentração de PM2.5 para cada município do estado de São Paulo, obtidas pelo CAMS-SISAM")),
                                  plotOutput("Mapa1"),
                                  p(strong("Mapa coroplético estático das médias anuais das medidas de concentração de PM2.5 para cada município do estado de São Paulo, obtidas pelo Donkelaar.")),
                                  plotOutput("Mapa2")
                 ),
                 conditionalPanel(condition = "input.viewmap",
                                  p(strong("Mapa coroplético interativo das médias anuais das medidas de concentração de PM2.5 para cada município do estado de São Paulo, obtidas pelo CAMS-SISAM e pelo Donkelaar. (A escala de cores usa como referência os dados do CAMS-SISAM)")),
                                  leafletOutput("MapaInt")
                 )
        ),
        
        tabPanel("Gráfico das Séries Temporais",
                 # Texto explicativo antes do gráfico
                 p(strong("Evolução das médias mensais de PM2,5 obtidas pelo CAMS-SISAM e pelo Donkelaar (12 médias mensais globais do Estado).")),
                 plotOutput("Grafico_De_Linha")
        )
      )
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  
  
  #  Teste de Normalidade Anderson-Darling
  output$TesteNormalidade <- renderPrint({
    ad.test(SP1Mes$pm2.5)
  })
  
  # Histograma
  output$Histograma <- renderPlot({
    x <- SP1Mes$pm2.5
    # Calcula histograma sem normalizar com limite de 0 a 65 no eixo X
    h <- hist(x,
              breaks = input$bins,
              xlim   = c(0, 65),
              main   = "Histograma com Curva de Densidade",
              xlab   = "Média Mensal de PM2.5",
              ylab   = "Frequência Absoluta",
              col    = "blue",
              border = "black")
    # Ajusta curva de densidade para frequência absoluta
    dens <- density(x, na.rm = TRUE)
    lines(dens$x,
          dens$y * length(x) * diff(h$breaks)[1],
          col = "red", lwd = 2)
    # Assegura que o eixo y comece em zero e x no ponto 0
    box()
  })
  
  # QQplot
  output$QQplot <- renderPlot({
    qqnorm(SP1Mes$pm2.5,  xlab = expression(bold("Percentis Teóricos")), ylab = expression(bold("Percentis Amostrais")))
    qqline(SP1Mes$pm2.5, col = "blue")
  })
  
  
  ##Boxplots
  output$Boxplot <- renderPlot({
    baseMes_long <- baseMes %>%
      pivot_longer(c(pm2.5, Media_PM25), names_to = "Fonte", values_to = "Valor") %>%
      mutate(
        mes = factor(mes, levels = 1:12,
                     labels = month.name),
        Fonte = recode(Fonte, pm2.5 = "CAMS-SISAM", Media_PM25 = "Donkelaar")
      )
    ggplot(baseMes_long, aes(mes, Valor, fill = Fonte)) +
      geom_boxplot(position = position_dodge(0.8), width = 0.6) +
      labs(x = "Mês", y = expression(PM[2.5]~~(mu*g/m^3)), fill = "Fonte") +
      theme_minimal()
  })
  
  
  

  # Dispersão + reta
  output$Dispersao <- renderPlot({
    ggplot(baseMes, aes(pm2.5, Media_PM25)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = expression(PM[2.5]~CAMS-SISAM),
           y = expression(PM[2.5]~Donkelaar)) +
      theme_minimal()
  })
  
  # Altman-Bland
  output$AltmanBland <- renderPlot({
    bland.altman.plot(baseMes$pm2.5, baseMes$Media_PM25,
                      graph.sys = "base",
                      xlab = "Média das duas medidas",
                      ylab = "Diferença das duas medidas")
  })
  
  # Mapas
  sf::sf_use_s2(FALSE)
  shp <- "SP_Municipios_2024.shp"
  muni <- tryCatch({
    st_read(shp, quiet = TRUE)
  }, error = function(e) {
    geobr::read_municipality(code_muni = "SP", year = 2020)
  })
  
  muni$CD_MUN <- as.character(muni$CD_MUN)
  baseAno$CD_MUN <- as.character(baseAno$CD_MUN)
  SP2Ano$CD_MUN <- as.character(SP2Ano$CD_MUN)
  
  mapa1 <- dplyr::left_join(muni, baseAno, by = "CD_MUN")
  mapa2 <- dplyr::left_join(muni, SP2Ano, by = "CD_MUN")
  
  
  output$Mapa1 <- renderPlot({
    ggplot(mapa1) + geom_sf(aes(fill = pm2.5)) + theme_minimal()
  })
  output$Mapa2 <- renderPlot({
    ggplot(mapa2) + geom_sf(aes(fill = Media_PM25)) + theme_minimal()
  })
  
  
  output$MapaInt <- renderLeaflet({
    label_col <- ifelse("name_muni" %in% names(muni), "name_muni",
                        ifelse("NM_MUN" %in% names(muni), "NM_MUN", NA))
    pal <- leaflet::colorNumeric(palette = "viridis", domain = mapa1$pm2.5)
    leaflet::leaflet(mapa1) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        fillColor = ~pal(pm2.5), fillOpacity = 0.7,
        weight = 0.5, color = "white",
        label = ~paste0(
          get(label_col), "
",
"SISAM: ", round(pm2.5,1), " µg/m³", "
",
"Donkelaar: ", round(Media_PM25,1), " µg/m³"
        ),
highlight = leaflet::highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      leaflet:: addLegend(
        pal = pal,
        values = ~`pm2.5`,
        title = "Média PM2.5 (µg/m³)",
        position = "topright",
        labFormat = labelFormat(suffix = " µg/m³", between = " – ", digits = 1),
        opacity = 0.7
      )
      })
  
  #Gráfico das Séries temporais
  output$Grafico_De_Linha <- renderPlot({
    baseUF_long <- baseUF %>%
      pivot_longer(
        cols = c(pm2.5, Media_PM25),
        names_to = "Fonte",
        values_to = "Valor"
      ) %>%
      mutate(
        Fonte = recode(Fonte,
                       "pm2.5" = "SISAM",
                       "Media_PM25" = "Donkelaar"),
        Fonte = factor(Fonte, levels = c("SISAM", "Donkelaar")),
        mes = factor(mes,
                     levels = 1:12,
                     labels = c("Janeiro", "Fevereiro", "Março", "Abril",
                                "Maio", "Junho", "Julho", "Agosto",
                                "Setembro", "Outubro", "Novembro", "Dezembro"))
      )
    
    # Criar o gráfico base
    ggplot(baseUF_long, aes(x = mes, y = Valor, color = Fonte, group = Fonte)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("SISAM" = "blue", "Donkelaar" = "red")) +
      labs(
        x = "Mês",
        y = expression("Média de PM2.5 ("*mu*"g/m"^3*")"),
        color = "Fonte"
      ) +
      theme_minimal() +
      theme(
        legend.position = c(0.25, 0.85),
        legend.background = element_rect(fill = alpha("white", 0.7), color = "gray"),
        axis.title.x  = element_text(face = "bold"),
        axis.title.y  = element_text(face = "bold"),
        axis.text.x   = element_text(face = "bold"),
        axis.text.y   = element_text(face = "bold"),
        legend.title  = element_text(face = "bold")
      )
  })
}

# Roda o app
shinyApp(ui, server)
