library(echarts4r)
library(tidyverse)

dfag <- read.csv("data/AGD.csv", header = T)
dfat <- read.csv("data/ATD.csv", header = T)

dfag$Ano <- forcats::as_factor(dfag$Ano)
dfag$Meses <- forcats::as_factor(dfag$Meses)
dfag$Agendamentos <- forcats::as_factor(dfag$Agendamentos)
dfat$Ano <- forcats::as_factor(dfat$Ano)
dfat$Meses <- forcats::as_factor(dfat$Meses)
dfat$Classificacao<- forcats::as_factor(dfat$Classificacao)

dfag |> group_by(Ano, Meses) |>
  filter(
  Meses %in% c(
    "Jan/22",  "Fev/22",  "Mar/22",  "Abr/22",  "Mai/22", 
    "Jun/22",  "Jul/22",  "Ago/22",  "Jan/23",  "Fev/23",  
    "Mar/23", "Abr/23",  "Mai/23",  "Jun/23", "Jul/23", 
    "Ago/23", "Jan/24", "Fev/24",  "Mar/24",  "Abr/24",  
    "Mai/24",  "Jun/24", "Jul/24", "Ago/24"
    )
  ) |>  
  summarise(Total = sum(Quantidade)) |> 
  mutate(Freq_Rel = round( (Total/sum(Total)*100) ,1)) |>
  e_chart(x = Meses, timeline = TRUE) |> 
  e_area(serie = Freq_Rel, name = 'Freq. Relativa (em %)',
         label = list(show = TRUE, fontWeight = "bold",
                      formatter = htmlwidgets::JS("
                      function(params){
                      return(params.value[1] +'%')}")),
         itemStyle = list(color = "#d95850"),
         animation = TRUE,
         animationDuration = 2000) |> 
  e_theme("essos") |> 
  e_x_axis(name = "Meses", 
           nameTextStyle = list(fontWeight = 'bold'),
           axisLabel = list(fontWeight = 'bold')) |> 
  e_timeline_opts(axis_type = 'category',
                  lineStyle = list(color = "#eb8146"),
                  label = list(fontWeight = "bold"),
                  progress = list(
                    lineStyle = list(color = "#f2d643",
                                     type = "dashed"),
                    itemStyle = list(color = "#ffb248",
                                     borderColor = "#eb8146"))) |>
  e_format_y_axis(suffix = "%") |> 
  e_tooltip(trigger = "axis")

dfag |> group_by(Agendamentos) |>
  filter(
    Meses %in% c(
      "Jan/22",  "Fev/22",  "Mar/22",  "Abr/22",  "Mai/22", 
      "Jun/22",  "Jul/22",  "Ago/22",  "Jan/23",  "Fev/23",  
      "Mar/23", "Abr/23",  "Mai/23",  "Jun/23", "Jul/23", 
      "Ago/23", "Jan/24", "Fev/24",  "Mar/24",  "Abr/24",  
      "Mai/24",  "Jun/24", "Jul/24", "Ago/24"
    )
  ) |> 
  e_charts(Meses, timeline = F) |> 
  e_bar(Quantidade, symbol_size = 5,name = "Frequência:", itemStyle = list(
    borderColor = "darkgrey", borderWidth = '.7') ) |> 
  # e_labels(position = 'insideTop') |> 
  e_legend(show = FALSE) |> 
  e_theme("macarrons") |> 
  e_tooltip(trigger = "axis",
            valueFormatter = htmlwidgets::JS("
                                        (value) => '' + Number(value).toFixed(0)
                                        ")
            ) |> 
  e_facet(cols = 1, rows = 2)

# Boxplot
dfag |> 
  filter(
    Meses %in% c(
      "Jan/22",  "Fev/22",  "Mar/22",  "Abr/22",  "Mai/22", 
      "Jun/22",  "Jul/22",  "Ago/22",  "Jan/23",  "Fev/23",  
      "Mar/23", "Abr/23",  "Mai/23",  "Jun/23", "Jul/23", 
      "Ago/23", "Jan/24", "Fev/24",  "Mar/24",  "Abr/24",  
      "Mai/24",  "Jun/24", "Jul/24", "Ago/24"
    )
  ) |> group_by(Agendamentos) |> 
  e_charts() |> 
  e_boxplot(Quantidade, outliers = T) |> 
  e_tooltip(trigger = "axis",  valueFormatter = htmlwidgets::JS("
                                        (value) => '' + Number(value).toFixed(0)
                                        ")) |> 
  e_title(text = "Boxplot - Classificações de Agendamento")


