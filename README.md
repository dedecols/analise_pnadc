# Análise de Indicadores do Mercado de Trabalho - PNADc 2024

Este repositório contém o script R para calcular indicadores do mercado de trabalho brasileiro usando os microdados da PNAD Contínua de 2024.

## Descrição

O script `pnadc-2024.R` realiza as seguintes etapas:
1. Verifica e instala todas as dependências de pacotes necessárias.
2. Baixa os microdados trimestrais da PNADc para o ano de 2024 (apenas na primeira execução) e os salva localmente em uma pasta `dados/`.
3. Calcula os fatores de deflação com base no INPC para ajustar os valores de renda.
4. Calcula três indicadores: Taxa de Participação, Taxa de Desocupação e Renda Média dos Ocupados no Trabalho Principal.
5. Gera uma planilha Excel formatada `indicadores_pnadc_2024.xlsx` com os resultados finais e os salva localmente na pasta `resultados/`.

## Como Executar

1. Abra o arquivo `analise_pnadc.Rproj` no RStudio para iniciar o projeto.
2. Abra o script `pnadc-2024.R`.
3. Execute o script do início ao fim.

O script cuidará de todo o processo de instalação de pacotes (se necessário), download de dados e armazenamento dos resultados na planilha `indicadores_pnadc_2024.xlsx`.