# Iniciando o script de Machine Learning

# --------------------------------------------------------------------
# Carregando o Dataset
dfTrain <- read.csv("data/train.csv")
dfTest <- read.csv("data/test.csv")

# Observacoes Iniciais
# --------------------------------------------------------------------
# O dataset de treino contém 14.803 observacoes e 32 atributos. 
# O dataset de teste contém 4.932 observacoes e 32 atributos. 
# A coluna "Appliances" é o alvo.

# Juntando os dados de treino e teste
df <- rbind(dfTrain,dfTest)

# Visualizando os dados
head(df)

# Visualizando os nomes das colunas
names(df)


# Descricao das variáveis
# --------------------------------------------------------------------
# date: tempo de coleta dos dados pelo sensor (year-month-day hour:minute)
# Appliances: uso de energia (em watt-hora)
# lights: potencia de energia de lampadas na casa (em watt-hora)
# TXX: Temperatura em um lugar da casa (em Celsius)
# RH_XX: umidade em um lugar da casa (em %)
# T_out: temperatura externa (em Celsius) in Celsius 
# Pressure: pressão externa (em mm Hg)
# RH_out: umidade externa (em %) 
# Wind speed: velocidade do vento (em m/s)
# Visibility; visibilidade (em km)
# Tdewpoint:
# rv1: variavel randomica adicional
# rv2, variavel randomica adicional
# WeekStatus: indica se é dia de semana ou final de semana (weekend ou weekday)
# Day_of_week: dia da semana
# NSM: segundos (extraido da data)