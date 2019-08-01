# Criando alguns modelos de ML para comparacoes
# --------------------------------------------------------------------

# Definindo a formula com as features selecionadas
formula <- "Appliances ~ T2+T3+T6+T8+T9+
                         RH_1+RH_2+RH_3+RH_6+RH_7+RH_8+
                         T_out+RH_out+
                         lights+
                         NSM+
                         WeekStatus+
                         Day_of_week"

formula <- as.formula(formula)


# Construindo um modelo Multiple Logistic Regression (GLM)
# --------------------------------------------------------------------
controlGLM <- trainControl(method="cv", number=5)
modeloGLM <- train(formula, data = dados_treino, method = "glm", metric="Rsquared", trControl=controlGLM)

# Resumo do Modelo Linear Model
print(modeloGLM)


# Construindo um modelo Random Forest (RF)
# --------------------------------------------------------------------
controlRF <- trainControl(method="cv", number=5)
modeloRF <- train(formula, data = dados_treino, method = "rf", metric="Rsquared", trControl=controlRF)

# Resumo do Modelo Random Forest
print(modeloRF)

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


# Observacoes da Performance dos modelos
# --------------------------------------------------------------------
# Multiple Logistic Regression (GLM):
# Random Forest (RF):
# Generalized Boosted Regression Modeling (GBM): 
# eXtreme Gradient Boosting (XGBoost): 

