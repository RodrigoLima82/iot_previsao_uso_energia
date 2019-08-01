# Otimizando o modelo eXtreme Gradient Boosting
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

# Gravando os melhores hyperparametros
bestParam <- expand.grid(nrounds = 200,
                         max_depth = 20,
                         colsample_bytree = 0.6,
                         eta = 0.1,
                         gamma = 0,
                         min_child_weight = 1,
                         subsample = 1)

# Treinando o modelo otimizado
xgb_model <- train(formula, data=dados_treino, method="xgbTree", trControl=xgb_trcontrol, tuneGrid=bestParam)

# Resultado do modelo
print(xgb_model)
