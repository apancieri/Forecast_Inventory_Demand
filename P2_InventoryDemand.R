### Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicação Mobile

# Este código foi criado para o projeto da Formação Cientista de Dados da
# Data Science Academy

# Problema de Negócio: prever a demanda de estoque com base no histórico de vendas.

# As informações foram disponibilizado pelo Grupo Bimbo e podem ser
# encontradas no Kaggle
# https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data

# Bibliotecas
library(data.table)
library(dplyr)
library(ggplot2)
library(plyr)
library(caTools)
library(caret)
library(rpart.plot)
library(GGally)
library(ggfortify)
library (gdata)
library(tidyr)
library(rmarkdown)

# Importando os arquivos
train <- fread('train.csv')
product <- fread('producto_tabla.csv')
city_state <- fread('town_state.csv')
client <- fread('cliente_tabla.csv')


# PRÉ-PROCESSAMENTO DOS DADOS

# Criando um subset dos dados pois dataset train contém muitas observações
train_index <- sample.split(train$Semana, SplitRatio = 0.06)
df_train <- subset(train, train_index == TRUE)

# Excluindo arquivos da memória do R
rm(train_index)
rm(train)

# Tabela Cliente
head(client)
str(client)

# Os códigos de clientes duplicados serão excluídos.
df_client <- client %>% group_by(Cliente_ID) %>% filter (! duplicated(Cliente_ID))
head(df_client)
rm(client)

# Agrupando os datasets para formar o dataset treino para análise exploratóra dos 
# dados
train_complete <- join_all(list(df_train, product, city_state, df_client))
str(train_complete)

# Verificar se existem dados missing
sapply(train_complete, function(x) sum(is.na(x)/length(x))*100)


########################### ANÁLISE EXPLORATÓRIA ###########################

# A Distribuição dos dados referentes à Demanda_uni_equil se distribui de maneira
# assimétrica. Apesar dos métodos baseados em árvore que serão usados no modelo
# se tratarem de técnicas não paramétricas a transformação dos dados será feita 
# pois um modelo de regressão linear tamém será utilizado

summary(train_complete$Demanda_uni_equil)

ggplot(train_complete) +
  geom_histogram(aes(x=Demanda_uni_equil), bins = 100) +
  ggtitle(label = 'Demanda') +
  labs(x = 'Unidades', y = 'Observações') +
  theme_minimal()

# A target tem distribuição assimétrica (positiva). 
# Para facilitar as análises do modelo de regressão linear, vamos trabalhar com o
# log da target como variável dependente.

# criando o log do target
train_complete = train_complete %>%
  mutate(log_demanda = log(Demanda_uni_equil))

# visualização do novo target
ggplot(train_complete) +
  geom_histogram(aes(x=log_demanda), bins = 100) +
  ggtitle(label = 'Log da Demanda') +
  labs(x = 'Log(Unidades)', y = 'Observações') +
  theme_minimal()


# Venta_uni_hoy e Venta_hoy

# Análise da distribuição das variáveis
vup <- ggplot(train_complete) +
  geom_histogram(aes(x=Venta_uni_hoy), bins = 100) +
  ggtitle(label = 'Venda unitaria') +
  labs(x = 'Unidades', y = 'Observações') +
  theme_minimal()

vp <- ggplot(train_complete) +
  geom_histogram(aes(x=Venta_hoy), bins =50) +
  ggtitle(label = 'Venda ($)') +
  labs(x = 'Pesos ($)', y = 'Observações') +
  theme_minimal()

gridExtra::grid.arrange(vup,vp,nrow=2)


# Dev_uni_proxima e Dev_proxima
# Analisando a distribuição das variáveis
dup <- ggplot(train_complete) +
  geom_histogram(aes(x=Dev_uni_proxima), bins =50) +
  ggtitle(label = 'Unidades Devolvidas') +
  labs(x = 'Unidades', y = 'Observações') +
  theme_minimal()


dp <- ggplot(train_complete) +
  geom_histogram(aes(x=Dev_proxima), bins =50) +
  ggtitle(label = 'Unidades Devolvidas ($)') +
  labs(x = 'Pesos ($)', y = 'Observações') +
  theme_minimal()

gridExtra::grid.arrange(dup,dp,nrow=2)

## Produtos

# Produtos mais vendidos e mais devolvidos (em unidades)

v1 <- train_complete %>% 
  arrange(desc(Venta_uni_hoy)) %>% 
  head() %>%
  ggplot(aes(x = reorder(NombreProducto, Venta_uni_hoy),Venta_uni_hoy))+
  geom_bar(stat = "identity", fill = "dodgerblue")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais vendidos", 
       y = "UNIDADE",
       x = "PRODUTO")

d2 <- train_complete %>% 
  arrange(desc(Dev_proxima)) %>% 
  head() %>% 
  ggplot(aes(x = reorder(NombreProducto, Dev_uni_proxima),Dev_uni_proxima))+
  geom_bar(stat = "identity", fill = "orange")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais devolvidos",
       y = "UNIDADE",
       x = "PRODUTO")

gridExtra::grid.arrange(v1,d2,nrow=2)


# Produtos mais vendidos e mais devolvidos (em pesos)
# Podemos observar que os produtos mais devolvidos não estão entre os produtos 
# mais vendidos

p1 <- train_complete %>% 
  arrange(desc(Venta_hoy)) %>% 
  head() %>%
  ggplot(aes(x = reorder(NombreProducto, Venta_hoy),Venta_hoy))+
  geom_bar(stat = "identity", fill = "dodgerblue")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais vendidos", 
       y = "PESOS",
       x = "PRODUTO")

p2 <- train_complete %>% 
  arrange(desc(Dev_proxima)) %>% 
  head() %>% 
  ggplot(aes(x = reorder(NombreProducto, Dev_proxima),Dev_proxima))+
  geom_bar(stat = "identity", fill = "orange")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais devolvidos",
       y = "PESOS",
       x = "PRODUTO")

gridExtra::grid.arrange(p1,p2,nrow=2)


## Estados

# Maiores demandas identificadas por Estado
train_complete %>% 
  top_n(10, log_demanda)%>% 
  ggplot(aes(x = log_demanda, y = reorder(State, log_demanda))) +
  geom_point(size = 3, color = "dodgerblue")+
  labs(title = "Estados com maior demanda", 
       y = "ESTADO",
       x = "DEMANDA")

## Cliente

# Cliente que mais vendem
train_complete %>% 
  arrange(desc(Venta_hoy)) %>% 
  head() %>%
  ggplot(aes(x = reorder(NombreCliente, Venta_hoy),Venta_hoy))+
  geom_bar(stat = "identity", fill = "dodgerblue")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Clientes que mais vendem (em pesos)", 
       y = "PESOS ($)",
       x = "CLIENTE")

# Criando o dataset com as variáveis selecionadas para o divisão em treino e 
# validação
df_model <- train_complete %>% select(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID, log_demanda)


# Verificar a existência e excluir dados infinito que podem ter surgido após a 
# transformação em log

df_model <- df_model[is.finite(rowSums(df_model)),]

sapply(df_model, function(x) sum(is.infinite(x)/length(x))*100)

################### DIVIDIR O DATASET EM TREINO E VALIDAÇÃO ##################

# divisao dos dados
df_index <- sample.split(df_model$Semana, SplitRatio = 0.70)

# Criando dados de treino e de validação
trainset <- subset(df_model, df_index == TRUE)
validset <- subset(df_model, df_index == FALSE)

rm(df_index)



#################### CORRELAÇÃO ################################


# Definindo as colunas (numéricas) para a análise de correlação 
# As variáveis Venta_uni_hoy e Dev_uni_proxima não entrarão pois elas compõem 
# a variável target.

# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(trainset, method = method)))

head(cors)

# Preparando o plot
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação mostra correlação alta entre a target e Venta_hoy e Venta_uni_hoy
Map(plot.cors, cors, metodos)

#################### TREINANDO E AVALIANDO OS MODELOS ########################

# Árvore de Decisão

control <- trainControl(method = "cv", number = 5,verboseIter = F)

tunegrid <- expand.grid(cp=seq(0.001, 0.01, 0.001))

model_tree <- 
  train(y=trainset$log_demanda, x=trainset[,-1],
        method="rpart",
        trControl=control,
        tuneGrid=tunegrid,
        metric = "Rsquared"
  )
model_tree

# Plot do resultado
rpart.plot(model_tree$finalModel, cex = 0.5)

# Salvando o resultado em disco
validset$Cliente_ID %>% cbind(round(predict(model_tree, validset) %>% exp)) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("model_tree.csv",row.names = F)


# BAGGING

control <- trainControl(method = "cv", number = 5,verboseIter = F)

model_bag <- train(y=trainset$log_demanda, 
                    x=trainset[,-1], 
                    method = "treebag",
                    metric = "Rsquared",
                    trControl=control
)
model_bag

## Salvando o resultado em disco
validset$Cliente_ID %>% cbind(round(predict(model_bag, validset)%>% exp)) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("Model_bag.csv",row.names = F)



# REGRESSÃO LINEAR

control <- trainControl(method = "cv", number = 5,verboseIter = F)

model_lm <- train(log_demanda~.,data=trainset,
                  method = "lmStepAIC",
                  trControl=control,
                  metric = "Rsquared",trace=F
)
model_lm

# Visualizando o modelo
ggcoef(
  model_lm$finalModel,                      
  vline_color = "red",            
  errorbar_color = "blue",      
  errorbar_height = .25,
  shape = 18,                  
  size=2,                      
  color="black",
  exclude_intercept = TRUE,                
  mapping = aes(x = estimate, y = term, size = p.value))+
  scale_size_continuous(trans = "reverse")+ 
  theme_bw()

# Análise dos ruídos
model_lm$finalModel %>% 
  autoplot(which = 1:2) + 
  theme_bw()

# Salvando o modelo em disco
validset$Cliente_ID %>% cbind(round(predict(model_lm, validset) %>% exp )) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("Model_lm.csv",row.names = F)


####################### COMPARANDO OS MODELOS ##########################3

resamps <- resamples(list(rpart = model_tree,
                          treebag = model_bag, lm = model_lm))

summary(resamps)
dotplot(resamps)


resamps$values %>%
  select(1, ends_with("RMSE")) %>%
  gather(model, RMSE, -1) %>%
  mutate(model = sub("~RMSE", "", model)) %>%
  {bwplot(model ~ RMSE, data = .)}


######################### DADOS TESTE ######################################

# Segundo as informações relatadas sobre o dataset Test, este não possui algumas 
# variáveis. 

# importar dataset
test <- fread('test.csv')

# O dataset test não possui todas as variáveis do dataset train
str(test)
head(test)


## PRÉ-PROCESSAMENTO

# Ajustar o dataset teste inserindo a variável target demanda media de vendas 
# que será calculada a partir dos dados do dataset de treino

# Excluindo a variável id
test$id <- NULL

# alterando a ordem das colunas
trainset <- trainset %>% select (log_demanda, Semana, Cliente_ID,Agencia_ID, 
                                 Canal_ID, Ruta_SAK, Producto_ID)

# criando a variável com a média de compra por cliente
df_mean_cliente <- aggregate(trainset[, 1], list(trainset$Cliente_ID), mean)

# Renomear colunas
names(df_mean_cliente) <- c("Cliente_ID", "log_demanda")

# criando a variável com a demanda média por produto (Producto_ID é uma das 
# variáveis que mais impactamo modelo)
df_mean_prod <- aggregate(trainset[, 1], list(trainset$Producto_ID), mean)

# Renomear colunas
names(df_mean_prod) <- c("Producto_ID", "Media_Demanda_Prod")

# unir os df de médias com o df teste (apenas as linhas que possuem Cliente_ID
# correspondente)
testset <- left_join(test, df_mean_cliente, by = "Cliente_ID")
testset <- left_join(testset, df_mean_prod, by = "Producto_ID")

# Imputando dados de média de Demanda de acordo com produto em dados NA
i <- is.na(testset$log_demanda)
testset$log_demanda[i] <- testset$Media_Demanda_Prod[i]

# Excluindo coluna Media Demanda_Prod
testset$Media_Demanda_Prod <- NULL

# Verificar os NAs
sapply(testset, function(x) sum(is.na(x)/length(x))*100)

# A estratégia escolhida para imputar os poucos dados NAs restante foi tomada de
# maneira arbitrária. Os valores faltantes serão preenchidos com a média da
# variável Média_Demanda_Cli
summary(testset$log_demanda)
testset$log_demanda[which(is.na(testset$log_demanda))] <- mean(testset$log_demanda)

head(testset)
str(testset)

#################### TESTANDO O MODELO  #######################

# Gerando arquivos com dados de teste do modelo avaliado com melhores métricas

testset$Cliente_ID %>% cbind(predict(model_tree, testset) %>% exp) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("model_tree_test.csv",row.names = F)