# Feature Selection (Selecao de Variaveis)
# --------------------------------------------------------------------

# Carregando os Pacotes
library(caret)
library(scales)

# Antes de selecionar as melhores variaveis, vamos tratar os outliers da target
# Conforme identificado na analise exploratoria, 1.974 registros sao os 10% de outliers
# Realizando o modelo preditivo com todos os registros, obtive um R_square de 0.55
# Vamos remover esses 10% e analisar a performance do modelo
df <- df[-which(df$Appliances %in% outliers),]
boxplot(df$Appliances)

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

# Transformando o campo data para index (ajudará na analise de serie temporal)
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
# Features de Umidade: todas as features de umidade tem importancia entre 20 e 40%
# Features de Tempo: tambem estao dentro do quadrante de 20-40%
# Essas sera as variaveis selecionadas para a criaçao dos modelos

