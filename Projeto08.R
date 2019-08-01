# --------------------------------------------------------------------
# 01 - Iniciando o script de Machine Learning
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
# Optei por realizar esse processo pois irei fazer um split dos dados no momento de treinamento do modelo
# Assim, incorporo também os padroes do dataset de teste
df <- rbind(dfTrain,dfTest)

# Visualizando os dados
head(df)

# Visualizando os nomes das colunas
names(df)


# Descricao das variáveis
# --------------------------------------------------------------------
# date: tempo de coleta dos dados pelos sensores (year-month-day hour:minute)
# Appliances: uso de energia (em W)
# lights: potencia de energia de eletrodomesticos na casa (em W)
# TXX: Temperatura em um lugar da casa (em Celsius)
# RH_XX: umidade em um lugar da casa (em %)
# T_out: temperatura externa (em Celsius) in Celsius 
# Pressure: pressão externa (em mm Hg)
# RH_out: umidade externa (em %) 
# Wind speed: velocidade do vento (em m/s)
# Visibility; visibilidade (em km)
# Tdewpoint: nao descobri o que significa mas acredito que dados de algum sensor
# rv1: variavel randomica adicional
# rv2, variavel randomica adicional
# WeekStatus: indica se é dia de semana ou final de semana (weekend ou weekday)
# Day_of_week: dia da semana
# NSM: medida do tempo em segundos

# --------------------------------------------------------------------
# 02 - Aplicando Engenharia de Atributos (Feature Engineering)
# --------------------------------------------------------------------

# Transformar o objeto de data
df$date <- strptime(as.character(df$date),format="%Y-%m-%d %H:%M")
df$date <- as.POSIXct(df$date , tz="UTC")

# Extraindo ano, mes, dia, hora e minuto do campo data
df$day   <- as.integer(format(df$date, "%d"))
df$month <- as.factor(format(df$date, "%m"))
df$hour <- as.integer(format(df$date, "%H"))

# Transformando variáveis numéricas em variáveis categóricas
df$lights <- as.factor(df$lights)


# --------------------------------------------------------------------
# 03 - Analise Exploratoria de Dados
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

# Visualizando a correlacao usando metodo 'spearman'
# É uma visualizacao completa entre as variaveis
chart.Correlation(data_cor, 
                  method="spearman",
                  histogram=TRUE,
                  pch=16)


# Visualizando um corrgram
# É uma visualizacao mais confortavel, porem com interpretacao dos dados numericos
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
head(outliers)

# Observacoes da variavel target
# --------------------------------------------------------------------
# 1. esta coluna esta com skewed positivo
# 2. muitos valores estao com media de 100W (75% dos dados)
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


# Observacoes dos graficos
# --------------------------------------------------------------------
# 1. pelos graficos apresentados, conseguimos observar um pico de consumo no mes de janeiro
#    e alguns periodos com baixa frequencia de consumo (final de janeiro e inicio de abril principalmente)
# 2. o consumo de energia do mes de marco, abril e maio é menor que os meses de janeiro e fevereiro 
#    pode ser um periodo de ferias ou devido ao verao


# --------------------------------------------------------------------
# 04 - Feature Selection (Selecao de Variaveis)
# --------------------------------------------------------------------

# Carregando os Pacotes
library(caret)
library(scales)

# Normalizando as variáveis numericas
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

# Definindo as variaveis que serao normalizadas
numeric.vars <- c('T1','RH_1','T2','RH_2','T3','RH_3','T4','RH_4','T5','RH_5','T6','RH_6','T7','RH_7','T8','RH_8','T9','RH_9',
                  'T_out','Press_mm_hg','RH_out','Windspeed','Visibility','Tdewpoint','rv1','rv2','NSM')
df <- scale.features(df, numeric.vars)

# Transformando o campo data para index
rownames(df) <- df$date
df$date <- NULL

# Gerando dados de treino e de teste
# --------------------------------------------------------------------
splits <- createDataPartition(df$Appliances, p=0.7, list=FALSE)

# Separando os dados de treino e teste
dados_treino <- df[ splits,]
dados_teste <- df[-splits,]

# Verificando o numero de linhas
nrow(dados_treino)
nrow(dados_teste)

# Verificando as features mais importantes usando RandomForest
# --------------------------------------------------------------------
formula <- "Appliances ~ ."
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 3, repeats = 2)
result <- train(formula, data = dados_treino, method = "rf", trControl = control, importance=T)
importance <- varImp(result, scale = FALSE)

# Plot do resultado
plot(importance, type=c("g", "o"))


# Observacoes da Feature Importance
# --------------------------------------------------------------------
# NSM: tem uma grande importancia dentro do dataset
# Features de Temperaturas: todas as features de temperatura tem importancia entre 20 a 40%
# Features de Umidade: todas as features de umidade tem importancia entre 20 e 40% (com excecao da RH_out)
# Features de Tempo: tambem estao dentro do quadrante de 20-40%
# Essas sera as variaveis selecionadas para a criaçao dos modelos:
#   NSM, T1...T9, T_out, RH_1...RH_9, RH_out, Press_mm_hg, Tdewpoint, Visibility, Windspeed, day, hour


# --------------------------------------------------------------------
# 05 - Criando alguns modelos de ML para comparacoes
# --------------------------------------------------------------------

# Definindo a formula com as features selecionadas
formula <- "Appliances ~ NSM+
                         Press_mm_hg+
                         T1+T2+T3+T4+T5+T6+T7+T8+T9+
                         RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+
                         T_out+RH_out+
                         day+hour"

formula <- as.formula(formula)


# Construindo um modelo Multiple Logistic Regression (GLM)
# --------------------------------------------------------------------
controlGLM <- trainControl(method="cv", number=5)
modeloGLM <- train(formula, data = dados_treino, method = "glm", metric="Rsquared", trControl=controlGLM)

# Resumo do Modelo Linear Model
print(modeloGLM)


# Construindo um modelo Generalized Boosted Regression Modeling (GBM)
# --------------------------------------------------------------------
controlGBM <- trainControl(method="cv", number=5)
modeloGBM <- train(formula, data=dados_treino, method="gbm", verbose=FALSE, metric="Rsquared", trControl=controlGBM)

# Resumo do Modelo GBM
print(modeloGBM)


# Construindo um modelo eXtreme Gradient Boosting (XGBoost)
# --------------------------------------------------------------------
controlXGB <- trainControl(method = "cv", number = 5)
modeloXGB <- train(formula, data=dados_treino, method="xgbLinear", trControl=controlXGB)

# Resumo do Modelo GBM
print(modeloXGB)

# Observacoes da Performance dos modelos (dados de treino)
# --------------------------------------------------------------------
# Multiple Logistic Regression (GLM): RMSE=93.63 e R_squared=0.14
# Generalized Boosted Regression Modeling (GBM): RMSE=85.79 e R_squared=0.28
# eXtreme Gradient Boosting (XGBoost): RMSE=73.70 e R_squared=0.47


# --------------------------------------------------------------------
# 06 - Otimizando o modelo eXtreme Gradient Boosting
# --------------------------------------------------------------------

# Carregando os Pacotes
library(xgboost)

# Definindo os parametros de controle do modelo
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

# Modificando os hyperparametros usando o gridSearch
xgbGrid <- expand.grid(nrounds = c(50, 150, 200),
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

# Treinando o modelo otimizado
xgb_model <- train(formula, data=dados_treino, method="xgbTree", trControl=xgb_trcontrol, tuneGrid=xgbGrid)

# Visualizando a comparacao dos hyperparametros
plot(xgb_model)

# Verificando os melhores parametros do modelo
xgb_model$bestTune

# Treinando o modelo com os melhores parametros
xgb_model <- train(formula, data=dados_treino, method="xgbTree", trControl=xgb_trcontrol, tuneGrid=xgb_model$bestTune)

# Resultado do modelo
print(xgb_model)

# Observacoes da Performance no modelo XGBoost Otimizado (dados de treino)
# --------------------------------------------------------------------
# RMSE=65.38 e R_squared=0.58


# --------------------------------------------------------------------
# 07 - Avaliando o modelo XGBoost Otimizado nos dados de teste
# --------------------------------------------------------------------

# Calculando o RMSE
predicted = predict(xgb_model, dados_teste)
residuals = dados_teste$Appliances - predicted
RMSE = sqrt(mean(residuals^2))
cat('O RMSE nos dados de teste é: ', round(RMSE,3),'\n')

# Calculando o R-square
y_test_mean = mean(dados_teste$Appliances)
tss =  sum((dados_teste$Appliances - y_test_mean)^2 )
rss =  sum(residuals^2)
rsq  =  1 - (rss/tss)
cat('O R-square nos dados de teste é: ', round(rsq,3), '\n')


# Visualizando as previsoes do modelo
# --------------------------------------------------------------------
options(repr.plot.width=8, repr.plot.height=4)

dfPrevisoes = as.data.frame(cbind(predicted = predicted,
                                  observed = dados_teste$Appliances))

# Plot previsoes vs dados de teste
ggplot(dfPrevisoes,aes(predicted, observed)) + 
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm) + 
  ggtitle('Linear Regression ') + 
  ggtitle("Extreme Gradient Boosting - Otimizado: Previsões vs Dados de Teste") +
  xlab("Dados previstos ") + 
  ylab("Dados reais ") + 
  theme(plot.title = element_text(color="black",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


# Observacoes Finais da primeira versao
# --------------------------------------------------------------------
# Na primeira versao selecionei todas as variaveis do dataset 
# mas com alguns testes usando variaveis visualizadas pelo Feature Importance usando RandomForest
# obtive obtive a melhor acuracia no modelo XGBoost
# Usei ele para otimizar e obter um resultado de:
# Dados de Treino: RMSE 65.38 e R_square 0.58
# Dados de Teste : RMSE 65.81 e R_square 0.61


# Realizando o treinamento removendo os outliers da variavel target 'Appliances'
# --------------------------------------------------------------------
# Conforme identificado na analise exploratoria, 2.138 registros sao outliers
# Vamos remover esses 10% e analisar a performance do modelo
df2 <- rbind(dfTrain,dfTest)
df2 <- df2[-which(df2$Appliances %in% outliers),]
boxplot(df2$Appliances)

# Realizar as transformacoes na feature date
df2$date <- strptime(as.character(df2$date),format="%Y-%m-%d %H:%M")
df2$date <- as.POSIXct(df2$date , tz="UTC")
df2$day   <- as.integer(format(df2$date, "%d"))
df2$month <- as.factor(format(df2$date, "%m"))
df2$hour <- as.integer(format(df2$date, "%H"))
# Transformando variáveis numéricas em variáveis categóricas
df2$lights <- as.factor(df2$lights)
# Aplicando a mesma escala nos dados numericos
df2 <- scale.features(df2, numeric.vars)
# Transformando o campo data para index
rownames(df2) <- df2$date
df2$date <- NULL

# Gerando dados de treino e de teste
splits2 <- createDataPartition(df2$Appliances, p=0.7, list=FALSE)
dados_treino2 <- df[ splits2,]
dados_teste2 <- df[-splits2,]

# Treinando o modelo com os melhores parametros
xgb_model2 <- train(formula, data=dados_treino2, method="xgbTree", trControl=xgb_trcontrol, tuneGrid=xgb_model$bestTune)

# Resultado do modelo
print(xgb_model2)

# Observacoes da Performance no modelo XGBoost Otimizado (dados de treino s/ outliers)
# --------------------------------------------------------------------
# RMSE=69.01 e R_squared=0.55


# Conclusao Final
# --------------------------------------------------------------------
# O melhor algoritmo para esse dataset é o eXtreme Gradient Boosting
# O modelo otimizado e sem tratamento de outliers foi capaz de explicar 61% da variancia nos dados de teste
# Realizando a remocao de outliers no dataset, nao houve melhora significativa na performance do modelo
# O ideal agora seria obter mais dados para aumentar a performance do modelo
