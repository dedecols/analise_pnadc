library(gt)
library(gtExtras)

# Dados brutos ----

taxa_participacao <- c(coef(indicador1_q1),
                       coef(indicador1_q2),
                       coef(indicador1_q3),
                       coef(indicador1_q4))

taxa_desocupacao <- c(coef(indicador2_q1),
                      coef(indicador2_q2),
                      coef(indicador2_q3),
                      coef(indicador2_q4))

renda_media <- c(coef(indicador3_q1),
                 coef(indicador3_q2),
                 coef(indicador3_q3),
                 coef(indicador3_q4))

resultados_bp <- data.frame(periodo = c("2024_T1", "2024_T2", "2024_T3", "2024_T4"),
                         taxa_participacao, 
                         taxa_desocupacao,
                         renda_media)


# Tabela Geral ----

resultados_refinados <- resultados %>%
  mutate(trimestre_ordinal = paste0(str_extract(periodo, "\\d$"), "º")) %>% 
  select(trimestre_ordinal, taxa_participacao, taxa_desocupacao, renda_media)

tabela_geral <- gt(resultados_refinados, id = "tabela_geral") %>%
  cols_label(
    trimestre_ordinal = "Trimestre",
    taxa_participacao = html("Taxa de<br>Participação (%)"),
    taxa_desocupacao = html("Taxa de<br>Desocupação (%)"),
    renda_media = html("Renda Média<br>dos Ocupados (R$)")
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = "vertical-align: middle;",
    locations = cells_column_labels(columns = trimestre_ordinal)
  ) %>%
  fmt_percent(columns = c(taxa_participacao, taxa_desocupacao),
              decimals = 2, dec_mark = ",") %>%
  fmt_currency(columns = renda_media, currency = "BRL",
               decimals = 2, sep_mark = ".", dec_mark = ",") %>%
  tab_header(
    title = md("Indicadores de Mercado e Renda para o Brasil"),
    subtitle = "Resultados Trimestrais de 2024"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = "13pt"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_footnote(
    footnote = "Valores reais, ajustados pelo INPC com base no 4º trimestre de 2024.",
    locations = cells_column_labels(columns = renda_media)
  ) %>%
  tab_source_note(source_note = "Fonte: Pesquisa Nacional por Amostragem de Domicílios Contínua") %>%
  gt_theme_538() %>%
  opt_align_table_header(align = "left") %>%
  cols_align(align = "center", columns = everything())

# gtsave(tabela_geral, filename = "tabela_geral")


# Gráfico Taxa de Participação ----

dados_grafico <- resultados_bp %>%
  mutate(trimestre_label = str_replace(periodo, "2024_T", "T"))

grafico_participacao
ggplot(dados_grafico, aes(x = trimestre_label, y = taxa_participacao, group = 1)) +
  # Adiciona a linha que conecta os pontos
  geom_line(color = "#0072B2", size = 1) +
  
  # Adiciona os pontos para marcar cada trimestre
  geom_point(color = "#0072B2", size = 3) +
  
  # Adiciona os rótulos de dados em cada ponto
  geom_text(
    aes(label = percent(taxa_participacao, accuracy = 0.01, decimal.mark = ",")),
    vjust = -1.5, # Ajusta a posição vertical do texto (acima do ponto)
    size = 3.5,
    # fontface = "bold",
    color = "gray20"
  ) +
  
  # Formata os eixos e adiciona títulos
  scale_y_continuous(
    labels = percent_format(accuracy = 1, decimal.mark = ","),
    limits = c(0.615, 0.63) 
  ) +
  labs(
    title = "Taxa de Participação na Força de Trabalho em 2024",
    x = "Trimestres",
    y = "Taxa de Participação (%)",
    caption = "Fonte: Pesquisa Nacional por Amostragem de Domicílios Contínua"
  ) +
  
  # Aplica um tema limpo e customiza a aparência
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "gray0"),
    axis.title = element_text(face = "plain", color = "gray30"),
    panel.grid.minor = element_blank(), # Remove as linhas de grade menores
    panel.grid.major.x = element_blank() # Remove as linhas de grade verticais
  )


# Salvar o gráfico na pasta de resultados
ggsave(
  here::here("resultados", "grafico_taxa_participacao.png"),
  plot = grafico_participacao,
  width = 8, # Largura em polegadas
  height = 5, # Altura em polegadas
  dpi = 300 # Alta resolução para a apresentação
)



# Gráfico Taxa de Desocupação ----

# O data.frame 'dados_grafico' já foi criado na etapa anterior, então podemos reutilizá-lo.

# Criar o gráfico
grafico_desocupacao <- ggplot(dados_grafico, aes(x = trimestre_label, y = taxa_desocupacao, group = 1)) +
  # Adiciona a linha que conecta os pontos (usei uma cor diferente para distinção)
  geom_line(color = "#D55E00", size = 1) +
  
  # Adiciona os pontos para marcar cada trimestre
  geom_point(color = "#D55E00", size = 3) +
  
  # Adiciona os rótulos de dados em cada ponto
  geom_text(
    aes(label = percent(taxa_desocupacao, accuracy = 0.01, decimal.mark = ",")), # <-- MUDOU AQUI
    vjust = -1.5,
    size = 3.5,
    # fontface = "bold",
    color = "gray20"
  ) +
  
  # Formata os eixos e adiciona títulos
  scale_y_continuous(
    labels = percent_format(accuracy = 1, decimal.mark = ","),
    limits = c(0.06, 0.085) # <-- MUDOU AQUI (novos limites para a escala)
  ) +
  labs(
    title = "Taxa de Desocupação em 2024", # <-- MUDOU AQUI
    # subtitle = "Valores trimestrais para o Brasil",
    x = "Trimestre de 2024",
    y = "Taxa de Desocupação (%)", # <-- MUDOU AQUI
    caption = "Fonte: Pesquisa Nacional por Amostragem de Domicílios Contínua"
  ) +
  
  # Aplica um tema limpo e customiza a aparência
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "gray0"),
    axis.title = element_text(face = "plain", color = "gray30"),
    panel.grid.minor = element_blank(), # Remove as linhas de grade menores
    panel.grid.major.x = element_blank() # Remove as linhas de grade verticais
  )

# Exibir o gráfico no RStudio
print(grafico_desocupacao)

# Salvar o gráfico na pasta de resultados
ggsave(
  here::here("resultados", "grafico_taxa_desocupacao.png"), # <-- MUDOU AQUI (novo nome de arquivo)
  plot = grafico_desocupacao, # <-- MUDOU AQUI (novo objeto de gráfico)
  width = 8,
  height = 5,
  dpi = 300
)


# Gráfico Renda Média ----




# --- Gráfico: Renda Média (Versão Linhas) ---

# Criar o gráfico
grafico_renda_linhas <- ggplot(dados_grafico, aes(x = trimestre_label, y = renda_media, group = 1)) +
  # Adiciona a linha (usei uma terceira cor distinta)
  geom_line(color = "#009E73", size = 1) +
  
  # Adiciona os pontos
  geom_point(color = "#009E73", size = 3) +
  
  # Adiciona os rótulos de dados, formatados como moeda
  geom_text(
    aes(label = dollar(renda_media, prefix = "R$", big.mark = ".", decimal.mark = ",")), # <-- MUDOU AQUI
    vjust = -1.5,
    size = 3.5,
    # fontface = "bold",
    color = "gray20"
  ) +
  
  # Formata o eixo Y como moeda
  scale_y_continuous(
    labels = dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ","), # <-- MUDOU AQUI
    limits = c(3100, 3220) # <-- MUDOU AQUI (novos limites)
  ) +
  labs(
    title = "Renda Média dos Ocupados no Trabalho Principal em 2024", # <-- MUDOU AQUI
    subtitle = "Valores ajustados pelo INPC para o 4º trimestre de 2024",
    x = "Trimestre",
    y = "Renda Média (R$)", # <-- MUDOU AQUI
    caption = "Fonte: Pesquisa Nacional por Amostragem de Domicílios Contínua"
  ) +
  
  # Aplica o mesmo tema dos gráficos anteriores
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "gray10"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(face = "plain", color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Exibir o gráfico
print(grafico_renda_linhas)

# Salvar o gráfico
ggsave(
  here::here("resultados", "grafico_renda_media_linhas.png"),
  plot = grafico_renda_linhas,
  width = 8, height = 5, dpi = 300
)