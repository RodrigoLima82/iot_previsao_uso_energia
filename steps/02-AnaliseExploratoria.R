# Analise Exploratoria de Dados
# --------------------------------------------------------------------

# Carregando os Pacotes
library(dplyr)
library(Hmisc)
library(ggplot2)
library(PerformanceAnalytics)
library(corrgram)
library(zoo)

# Verificar se existem valores ausentes (missing) em cada coluna
# Nenhum valor encontrado
any(is.na(df))

# Verificando os dados estatisticos das variaveis numericas
describe(df)

# Observacoes Estatisticas
# --------------------------------------------------------------------
# Temperaturas internas: variacao entre 14.89 a 29.95 graus Celsius
# Temperaturas externas (T6 e T_out): variacao entre -6.06 a 28.29 graus Celsius. 

# Umidade interna: variacao entre 20.60% a 63.36% com excecao do RH_5
# Umidade externa (RH_6 e  RH_out): variacao entre 1% to 100%

# Energia: 75% do consumo de energia é menor que 100W
# O maior consumo é de 1080W o que representa um outlier no dataset

# Lights: 15.252 valores 0 (zero) em 19.735 observacoes
# Verificar se tem significancia para performance do modelo

# WeekStatus: 72,3% das observacoes foram durante a semana e 27,7% nos finais de semana


# Analise de Correlacao
# --------------------------------------------------------------------
# Separando as colunas numericas para correlacao
numeric.vars <- c('Appliances','T1','RH_1','T2','RH_2','T3','RH_3','T4','RH_4','T5','RH_5','T6','RH_6','T7','RH_7','T8','RH_8','T9','RH_9',
                  'T_out','Press_mm_hg','RH_out','Windspeed','Visibility','Tdewpoint','rv1','rv2','NSM')
data_cor <- cor(df[,numeric.vars])

# Visualizando a correlacao
chart.Correlation(data_cor, 
                  method="spearman",
                  histogram=TRUE,
                  pch=16)


# Visualizando um corrgram
corrgram(data_cor, order=TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)

# Observacoes da correlacao
# --------------------------------------------------------------------
# Temperaturas: todas essas features tem correlacao positiva com o target "Appliances"
# Atributos do Tempo: Visibility, Tdewpoint, Press_mm_hg tem correlacao baixa
# Umidade: nao tem correlacao significante (> 0.9 por exemplo)
# Variaveis Randomicas: sem influencia


# Avaliando a variavel target "Appliances"
# --------------------------------------------------------------------
hist(df$Appliances, "FD", xlab="Appliances", ylab="Frequencia")

# Verificar a quantidade outliers para o consumo de energia
boxplot(df$Appliances)
outliers <- boxplot(df$Appliances, plot=FALSE)$out

# Observacoes da target
# --------------------------------------------------------------------
# 1. esta coluna esta com skewed positivo
# 2. muitos valores estao com media de 100W (75%)
# 3. existem outliers (neste caso 2.138 registros sao outliers)


# Visualizando alguns plots para analise
# --------------------------------------------------------------------
# Analise de Serie Temporal
x <- zoo(df$Appliances, df$date)
plot(x)


# Visualizando o consumo de energia por dia x mes
ggplot(df)+
  geom_bar(aes(x=day, y=Appliances), stat="identity")+
  scale_y_continuous(name="Consumo de Energia (W)")+
  facet_wrap(~month, scale="free")+
  theme_bw()

# Visualizando o consumo de energia por dia x semana e final de semana
ggplot(df)+
  geom_bar(aes(x=day, y=Appliances), stat="identity")+
  scale_y_continuous(name="Consumo de Energia (W)")+
  facet_wrap(~WeekStatus, scale="free")+
  theme_bw()

