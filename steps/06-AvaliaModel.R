# Avaliando o modelo eXtreme Gradient Boosting (otimizado)
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


# Observacoes Finais
# --------------------------------------------------------------------
# Na primeira versao selecionei algumas variaveis visualizadas pelo Feature Importance usando RandomForest
# Assim, testando alguns modelos, obtive a melhor acuracia no modelo XGBoost
# Usei ele para otimizar e obter um resultado de RMSE XXX e R_square XXX

# Na segunda versao, tratei os outliers da variavel target 'Appliances'
# Aplicando no modelo otimizado de XGBoost, obtive uma melhora na performance nos dados de teste
# RMSE XXX e R_squared XXX


# Conclusao
# --------------------------------------------------------------------
# O melhor algoritmo para esse dataset é o eXtreme Gradient Boosting
# O modelo otimizado e sem tratamento de outliers foi capaz de explicar 55% da variancia nos dados de teste
# Realizando a remocao de outliers no dataset, houve uma melhora de 27%, sendo capaz de explicar 70% da variancia nos dados de teste
# O ideal seria obter mais dados e observar a recorrencia destes outliers e a relacao dos eventos relacionados a isso
