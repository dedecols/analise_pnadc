# Gerenciamento de Pacotes ----

pacotes_necessarios <- c("PNADcIBGE", "rbcb", "dplyr", "lubridate", "survey", "openxlsx", "here")

for (pacote in pacotes_necessarios) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote, dependencies = TRUE)
  }
  library(pacote, character.only = TRUE)
}


# Obter os microdados da PNAD Contínua de 2024 ---- 

# Criar um subdiretório para os dados, se ele não existir
if (!dir.exists(here::here("dados"))) {
  dir.create(here::here("dados"))
}

# Lista de trimestres para processar
trimestres <- 1:4

# Loop para baixar ou carregar os dados de cada trimestre
for (q in trimestres) {
  # Define o nome do arquivo .rds e o nome do objeto
  caminho_arquivo_rds <- here::here("dados", paste0("pnadc_Q", q, "24.rds"))
  nome_objeto <- paste0("pnadc_Q", q, "24")
  
  # Verifica se o arquivo já existe localmente
  if (file.exists(caminho_arquivo_rds)) {
    # Se existe, apenas carrega
    message(paste("Carregando", basename(caminho_arquivo_rds), "do disco."))
    assign(nome_objeto, readRDS(caminho_arquivo_rds))
  } else {
    # Se não existe, baixa, salva e atribui ao objeto
    message(paste("Baixando dados do T", q, "/2024. Isso pode levar alguns minutos...", sep = ""))
    dados_trimestre <- PNADcIBGE::get_pnadc(year = 2024,
                                            quarter = q,
                                            vars = c("V2009", "VD4001", "VD4002", "VD4016"),
                                            savedir = here::here("dados")) # Salva na pasta 'dados'
    
    saveRDS(dados_trimestre, file = caminho_arquivo_rds)
    assign(nome_objeto, dados_trimestre)
  }
}


# Criação de variável deflatora usando o INPC (para o indicador 3) ----
# OBSERVAÇÃO:
# Usei os dados obtidos do INPC para criar a variável deflatora
# Assim, será possível corrigir os valores de renda média tendo como base o quarto trimestre de 2024

# Obter os dados mensais do INPC de 2024 (variação percentual mensal)
codigos_sgs <- c(INPC = 188)
inpc_mensal <- rbcb::get_series(code = codigos_sgs, start_date = "2024-01-01", end_date = "2024-12-31") %>%
  # Converter a variação percentual para proporção
  dplyr::mutate(fator_mensal = 1 + INPC / 100)

# Calcular a série de Números-Índice Acumulados (base Dez/2023 = 100)
# A função cumprod() calcula o produto acumulado
inpc_acumulado <- inpc_mensal %>%
  dplyr::mutate(
    indice_acumulado = cumprod(fator_mensal) * 100, # Acumula o fator e multiplica por 100
    periodo = lubridate::quarter(date, type = "year.quarter")
  )

# Selecionar o Índice de Referência (último mês de cada trimestre)
indices_referencia <- inpc_acumulado %>%
  dplyr::group_by(periodo) %>%
  dplyr::slice_max(order_by = date, n = 1) %>% 
  dplyr::select(periodo, indice_qx = indice_acumulado)

# Definir o Índice Base (Q4/2024)
indice_base_q4 <- indices_referencia %>% 
  dplyr::filter(periodo == "2024.4") %>% 
  dplyr::pull(indice_qx)

# Calcular os Fatores de Deflação para cada trimestre (D_Qx = I_Base / I_Qx)
fatores_deflacao <- indices_referencia %>%
  dplyr::mutate(fator_deflacao = indice_base_q4/indice_qx) %>%
  dplyr::select(periodo, fator_deflacao)

# Separar os valores obtidos para inserir nos microdados de cada trimestre
fator_q1 <- fatores_deflacao %>% dplyr::filter(periodo == "2024.1") %>% dplyr::pull(fator_deflacao)
fator_q2 <- fatores_deflacao %>% dplyr::filter(periodo == "2024.2") %>% dplyr::pull(fator_deflacao)
fator_q3 <- fatores_deflacao %>% dplyr::filter(periodo == "2024.3") %>% dplyr::pull(fator_deflacao)



# Tratamento nos dados da PNADc ----
# OBSERVAÇÃO:
# Foi preciso criar variáveis binárias para evitar erros nos taxas (estimador do tipo razão)
# A renda foi ajustada com o INPC para atender o critério pedido por e-mail

pnadc_Q124 <- update(pnadc_Q124,
                     pia = ifelse(V2009 >= 14, 1, 0),
                     desocupado = ifelse(VD4002 == "Pessoas desocupadas", 1, 0),
                     forca_trabalho = ifelse(VD4001 == "Pessoas na força de trabalho", 1, 0),
                     rend_med_ocup_ajustada = VD4016*fator_q1)

pnadc_Q224 <- update(pnadc_Q224,
                     pia = ifelse(V2009 >= 14, 1, 0),
                     desocupado = ifelse(VD4002 == "Pessoas desocupadas", 1, 0),
                     forca_trabalho = ifelse(VD4001 == "Pessoas na força de trabalho", 1, 0),
                     rend_med_ocup_ajustada = VD4016*fator_q2)

pnadc_Q324 <- update(pnadc_Q324,
                     pia = ifelse(V2009 >= 14, 1, 0),
                     desocupado = ifelse(VD4002 == "Pessoas desocupadas", 1, 0),
                     forca_trabalho = ifelse(VD4001 == "Pessoas na força de trabalho", 1, 0),
                     rend_med_ocup_ajustada = VD4016*fator_q3)

pnadc_Q424 <- update(pnadc_Q424,
                     pia = ifelse(V2009 >= 14, 1, 0),
                     desocupado = ifelse(VD4002 == "Pessoas desocupadas", 1, 0),
                     forca_trabalho = ifelse(VD4001 == "Pessoas na força de trabalho", 1, 0),
                     rend_med_ocup_ajustada = VD4016)



# Indicador 1 - Taxa de Participação na Força de Trabalho ----

indicador1_q1 <- survey::svyratio(numerator = ~forca_trabalho,
                                  denominator = ~pia,
                                  design = pnadc_Q124,
                                  na.rm = TRUE)
indicador1_q2 <- survey::svyratio(numerator = ~forca_trabalho,
                                  denominator = ~pia,
                                  design = pnadc_Q224,
                                  na.rm = TRUE)
indicador1_q3 <- survey::svyratio(numerator = ~forca_trabalho,
                                  denominator = ~pia,
                                  design = pnadc_Q324,
                                  na.rm = TRUE)
indicador1_q4 <- survey::svyratio(numerator = ~forca_trabalho,
                                  denominator = ~pia,
                                  design = pnadc_Q424,
                                  na.rm = TRUE)

taxa_participacao <- c(coef(indicador1_q1),
                       coef(indicador1_q2),
                       coef(indicador1_q3),
                       coef(indicador1_q4))


# Indicador 2 - Taxa de Desocupação ----

indicador2_q1 <- survey::svyratio(numerator = ~desocupado,
                                  denominator = ~forca_trabalho,
                                  design = pnadc_Q124,
                                  na.rm = TRUE)
indicador2_q2 <- survey::svyratio(numerator = ~desocupado,
                                  denominator = ~forca_trabalho,
                                  design = pnadc_Q224,
                                  na.rm = TRUE)
indicador2_q3 <- survey::svyratio(numerator = ~desocupado,
                                  denominator = ~forca_trabalho,
                                  design = pnadc_Q324,
                                  na.rm = TRUE)
indicador2_q4 <- survey::svyratio(numerator = ~desocupado,
                                  denominator = ~forca_trabalho,
                                  design = pnadc_Q424,
                                  na.rm = TRUE)

taxa_desocupacao <- c(coef(indicador2_q1),
                      coef(indicador2_q2),
                      coef(indicador2_q3),
                      coef(indicador2_q4))


# Indicador 3 - Renda média dos ocupados no trabalho principal ----

indicador3_q1 <- survey::svymean(~rend_med_ocup_ajustada, design = pnadc_Q124, na.rm = TRUE)
indicador3_q2 <- survey::svymean(~rend_med_ocup_ajustada, design = pnadc_Q224, na.rm = TRUE)
indicador3_q3 <- survey::svymean(~rend_med_ocup_ajustada, design = pnadc_Q324, na.rm = TRUE)
indicador3_q4 <- survey::svymean(~rend_med_ocup_ajustada, design = pnadc_Q424, na.rm = TRUE)

renda_media <- c(coef(indicador3_q1),
                 coef(indicador3_q2),
                 coef(indicador3_q3),
                 coef(indicador3_q4))


# Gerar planilha ----

# Organizar resultados em um dataframe
resultados <- data.frame(periodo = c("2024_T1", "2024_T2", "2024_T3", "2024_T4"),
                         taxa_participacao, 
                         taxa_desocupacao,
                         renda_media)

# Criar estrutura da planilha
wb <- openxlsx::createWorkbook(creator = "André Lessa Silva")
openxlsx::addWorksheet(wb, "Painel PNADC")

# Inserir dados obtidos na planilha (sem formatação)
openxlsx::writeData(wb, sheet = "Painel PNADC", x = resultados)

# Formatar a saída/visualização dos indicadores
# As taxas serão exibida em porcentagem
style_pct <- openxlsx::createStyle(numFmt = "0.00%")
openxlsx::addStyle(wb, sheet = "Painel PNADC", style = style_pct,
                   rows = 2:(nrow(resultados) + 1), 
                   cols = 2:3, 
                   gridExpand = TRUE)

# A renda média é exibida em real
style_moeda <- openxlsx::createStyle(numFmt = "R$ #,##0.00")
openxlsx::addStyle(wb, sheet = "Painel PNADC", style = style_moeda,
                   rows = 2:(nrow(resultados) + 1), 
                   cols = 4, 
                   gridExpand = TRUE)

# Criar um subdiretório para os resultados, se ele não existir
if (!dir.exists(here::here("resultados"))) {
  dir.create(here::here("resultados"))
}

# Salvar tudo num arquivo no diretório especificado 
caminho_arquivo <- here::here("resultados", "indicadores_pnadc_2024.xlsx")
openxlsx::saveWorkbook(wb, file = caminho_arquivo, overwrite = TRUE)
