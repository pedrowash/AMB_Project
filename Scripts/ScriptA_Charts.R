library(echarts4r)
library(tidyverse)
library(apexcharter)

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


####
dfag |> 
  filter(
    Ano == 2024
  ) |> group_by(Meses) |> 
  summarise(Total = sum(Quantidade)) |> 
  e_charts() |> 
  e_boxplot(Total, outliers = T) |> 
  e_tooltip(trigger = "axis",  valueFormatter = htmlwidgets::JS("
                                        (value) => '' + Number(value).toFixed(0)
                                        ")) |> 
  e_title(text = "Boxplot - Agendamentos")

##

dfat |> 
  filter(
    Ano == 2024
  ) |> group_by(Meses) |> 
  summarise(Total = sum(Quantidade)) |> 
  e_charts() |> 
  e_boxplot(Total, outliers = T) |> 
  e_tooltip(trigger = "axis",  valueFormatter = htmlwidgets::JS("
                                        (value) => '' + Number(value).toFixed(0)
                                        ")) |> 
  e_title(text = "Boxplot - Atendimentos")
#####


dfag |> filter(
  Ano == 2024
) |> group_by(Meses, Agendamentos) |> 
  summarise(Total = sum(Quantidade)) |> 
  mutate(Freq_Rel = round( (Total/sum(Total)*100) ,1))

####





dfag |> filter(
  Ano == 2024
) |> group_by(Meses, Agendamentos) |> 
  summarise(Total = sum(Quantidade)) |> 
  mutate(Freq_Rel = round( (Total/sum(Total)*100) ,1)) |>
  select(-Total) |> 
  tidyr::pivot_wider(names_from = Agendamentos ,values_from = Freq_Rel) |> 
  kbl(caption = "Tabela 1.3. Frequências relativas (em %): Classificações de Agendamento  <br><i>Valores agrupados por mês e classificação.</i>",
    col.names = c("Meses", 	
                  "Atendimentos efetuados",	
                  "Atendimentos não efetuados"),
    align = 'ccc',
) |> 
  kable_minimal(full_width = F, html_font = "Cambria", c("hover", "striped")) |> 
  # column_spec(1, color = "black", bold = T, italic = T) |> 
  column_spec(1, color = "darkgray", bold = F, italic = T) |> 
  column_spec(2, color = "darkblue", bold = T) |> 
  column_spec(3, color = "darkred", bold = T) |> 
  footnote(general = "Tasy.",
           general_title = "Fonte: ", 
           number_title = "Type I: ",
           footnote_as_chunk = T)







##3

library(apexcharter)

dfag |> 
  filter(Ano == 2024) |> 
  group_by(Agendamentos) |>
  # mutate(Freq_Rel = round(Quantidade/sum(Quantidade)*100,1)  ) |> 
  apex(type = "column", mapping = aes(x = Meses, y = Quantidade, fill = Agendamentos))


apex(data = dfag, type = "column", mapping = aes(x = Meses, y = Quantidade, fill = Agendamentos))

dfag |> group_by(Agendamentos) |>  
  filter(Ano == 2024) |> 
  summarise(Total = sum(Quantidade)) |> 
  apex(type = "donut", mapping = aes(x = Agendamentos, y = Total))




####3

dfag |> 
  filter(Ano == 2024) |> 
  group_by(Agendamentos) |>
  # mutate(Freq_Rel = round(Quantidade/sum(Quantidade)*100,1)  ) |> 
  apex(type = "column", mapping = aes(x = Meses, y = Quantidade, fill = Agendamentos))


apex(data = dfag, type = "column", mapping = aes(x = Meses, y = Quantidade, fill = Agendamentos))

dfag |> group_by(Agendamentos) |>  
  filter(Ano == 2024) |> 
  summarise(Total = sum(Quantidade)) |> 
  apex(type = "donut", mapping = aes(x = Agendamentos, y = Total)) |>
  ax_labs(
    title = "Life expectancy : 1972 vs. 2007",
    subtitle = "Data from Gapminder dataset")



####  ERRADDOOOOOOOOO ===================================================

dfag |> 
  filter(Ano == 2024) |> 
  group_by(Agendamentos) |> 
  # summarise(Total = sum(Quantidade)) |> 
  # mutate(Freq_Rel = round(Total/sum(Total)*100,1)  ) |> 
  apex(aes(Agendamentos, Quantidade), "boxplot") %>% 
    ax_plotOptions(
      boxPlot = boxplot_opts(color.upper = "#8BB0A6", color.lower = "#8BB0A6" )
    ) %>% 
    ax_stroke(colors = list("#2A5744")) %>% 
    ax_grid(
      xaxis = list(lines = list(show = TRUE)),
      yaxis = list(lines = list(show = FALSE))
    ) |> 
  ax_tooltip()

######################################################################




# 
# dfag |> group_by(Agendamentos) |>
#   filter(
#    Ano == 2024
#   ) |> 
#   e_charts(Meses, timeline = T) |> 
#   e_bar(Quantidade, symbol_size = 5,name = "Frequência:", itemStyle = list(
#     borderColor = "darkgrey", borderWidth = '.7') ) |> 
#   # e_labels(position = 'insideTop') |> 
#   e_legend(show = FALSE) |> 
#   e_theme("essos") |> 
#   e_tooltip(trigger = "axis",
#   valueFormatter = htmlwidgets::JS("
#               (value) => '' + Number(value).toFixed(0)
#                                         "))



#### Parte B - Atendimentos
dfat |> filter(
  Ano == 2024
) |> group_by(Ano, Classificacao) |> 
  summarise(Total = sum(Quantidade)) |> 
  mutate(Freq_Rel = round( (Total/sum(Total)*100) ,1))

# Alterado por:

dfat |> filter(
  Ano == 2024
) |> group_by(Classificacao) |> 
  summarise(Total = sum(Quantidade)) |> 
  mutate(Freq_Rel = round( (Total/sum(Total)*100) ,1)) 


############
dfat |> filter(
  Ano == 2024
) |> group_by(Meses, Classificacao) |> 
  summarise(Total = sum(Quantidade)) |> 
  mutate(Freq_Rel = round( (Total/sum(Total)*100) ,1)) |>
  select(-Total) |> 
  tidyr::pivot_wider(names_from = Classificacao ,values_from = Freq_Rel) |> 
  kbl(caption = "Tabela 2.3. Frequências relativas mensais (em %): Classificações de Atendimento.  <br><i>Valores agrupados por mês e classificação.</i>",
      col.names = c("Meses", 	
                    "Interconsulta", 
                    "Primeira Vez", 
                    "Quimioterapia",
                    "Retorno"),
      align = 'ccccc',
  ) |> 
  kable_minimal(full_width = F, html_font = "Cambria", c("hover", "striped")) |> 
  # column_spec(1, color = "black", bold = T, italic = T) |> 
  column_spec(1, color = "darkgray", bold = F, italic = T) |> 
  column_spec(2, color = "darkred", bold = T) |> 
  column_spec(3, color = "darkred", bold = T) |> 
  column_spec(4, color = "darkred", bold = T) |> 
  column_spec(5, color = "darkred", bold = T) |> 
  footnote(general = "Tasy.",
           general_title = "Fonte: ", 
           number_title = "Type I: ",
           footnote_as_chunk = T)



####



dfat |> group_by(Classificacao) |>
  filter(
    Ano == 2024
  ) |> 
  e_charts(Meses, timeline = T) |> 
  e_bar(Quantidade, symbol_size = 5,name = "Frequência:", itemStyle = list(
    borderColor = "darkgrey", borderWidth = '.7') ) |> 
  e_labels(position = 'insideTop', fontSize = 16, fontStyle = 'bold') |> 
  e_legend(show = FALSE) |> 
  e_theme("essos") |> 
  e_tooltip(trigger = "axis",
            valueFormatter = htmlwidgets::JS("
              (value) => '' + Number(value).toFixed(0)
                                        "))




#### Medianas
dfag |> 
  filter(
    Ano == 2022
  ) |> 
  group_by(Meses) |> 
  summarise(Total = sum(Quantidade)) |> 
  summarize(mediana = median(Total))
