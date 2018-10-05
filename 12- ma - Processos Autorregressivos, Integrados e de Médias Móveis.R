                          #Aula 12 - Modelos  ARIMA

library("urca")                                #Carrega Pacote URCA
library(readxl)                                #Carrega Pacote readxl
library(pwt8)                                  #Carrega o pacote PWT8.0


data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoiníveis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0


br <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (país) igual a "Brazil", selecionando as colunas cujas variáveis são "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (CÂMBIO)

colnames(br) <-  c("PIB","Emprego","Câmbio")   #Renomeia as colunas para PIB, Trabalho e Câmbio

                                        #Separando as variáveis
PIB <- br$PIB[45:62]                    #Cria o vetor para variável PIB                  
EMPREGO <- br$Emprego[45:62]            #Cria o vetor para variável EMPREGO
CAMBIO <- br$Câmbio[45:62]              #Cria o vetor para variável CAMBIO
Anos <- seq(from=1994, to=2011, by=1)   #Cria um vetor para o tempo em anos de 1994 até 2011 


                                    #Analise para o Emprego

plot(EMPREGO, type = "l")                            #Cria gráfico para o EMPREGO
emprego <- ts(EMPREGO, start = 1994, frequency = 1)  #Define como Série Temporal
plot(emprego, main="Pessoa Empregadas no Brasil", 
     ylab="Qte de Pessoas Empregadas-milhões", 
     xlab="Ano")                                      #Cria gráfico da Série Temporal

plot(CAMBIO, type = "l")                            #Cria gráfico para o CAMBIO
cambio <- ts(CAMBIO, start = 1994, frequency = 1)  #Define como Série Temporal
plot(cambio, main="Cambio no Brasil", 
     ylab="Cambio-milhões", 
     xlab="Ano")    

plot(PIB, type = "l")                            #Cria gráfico para o PIB
pib <- ts(PIB, start = 1994, frequency = 1)  #Define como Série Temporal
plot(pib, main="PIB no Brasil", 
     ylab="PIB-milhões", 
     xlab="Ano") 

acf(cambio)                                          #Função de Autocorrelação
pacf(cambio)
reglinCAM <- lm(CAMBIO ~ Anos)                       #Regressão linear simples do emprego em relação ao tempo
reglinCAM                                             #Exibe os resultados da regressão linear
summary(reglinCAM)
plot(cambio)                                         #Gráfcio dos dados
abline(reglinCAM, col="Red")  

acf(pib)                                          #Função de Autocorrelação
pacf(pib)
reglinPIB <- lm(PIB ~ Anos)                       #Regressão linear simples do emprego em relação ao tempo
reglinPIB                                             #Exibe os resultados da regressão linear
summary(reglinPIB)
plot(pib)                                         #Gráfcio dos dados
abline(reglinPIB, col="Green")  

acf(emprego)                                          #Função de Autocorrelação
pacf(emprego)                                         ##Função de Autocorrelação Parcial
reglinEMP <- lm(EMPREGO ~ Anos)                       #Regressão linear simples do emprego em relação ao tempo
reglinEMP                                             #Exibe os resultados da regressão linear
summary(reglinEMP)
plot(emprego)                                         #Gráfcio dos dados
abline(reglinEMP, col="Blue")                         #Insere a linha de regressão linear estimada


#Removendo Tendência

residuosEMP <- reglinEMP$residuals                   #Salva os resíduos no vetor residuosEMP
reglinEMP$residuals 
reglinEMPres <- lm(residuosEMP ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosEMP,type="l")                            #Gráfico dos resíduos
abline(reglinEMPres, col="Blue")                      #Insere a linha de regressão linear dos resíduos

residuosCAM <- reglinCAM$residuals                    #Salva os resíduos no vetor residuosEMP
reglinCAM$residuals
reglinCAMres <- lm(residuosCAM ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosCAM,type="l")                            #Gráfico dos resíduos
abline(reglinCAMres, col="Red")                      #Insere a linha de regressão linear dos resíduos

residuosPIB <- reglinPIB$residuals                    #Salva os resíduos no vetor residuosEMP
reglinPIB$residuals
reglinPIBres <- lm(residuosPIB ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosPIB,type="l")                            #Gráfico dos resíduos
abline(reglinPIBres, col="Green")                      #Insere a linha de regressão linear dos resíduos


#Removendo Tendência por meio da diferença

pdemprego <- diff(EMPREGO)                                #Calcula a primeira diferença da série de dados
diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))       #Exibe a tabela da série original coma diferença <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Blue")) #Cria o grafico com as duas series
plot(pdemprego, type="l")                                   #Cria gr´pafico somente para a serie da diferença

pcambio <- diff(CAMBIO)                                #Calcula a primeira diferença da série de dados
pdiferenca1 <- (data.frame(CAMBIO[2:18],pcambio))       #Exibe a tabela da série original coma diferença <- 
PDIFERENCA <- ts(pdiferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(PDIFERENCA, plot.type="single", col=c("Black","Red")) #Cria o grafico com as duas series
plot(pcambio, type="l")                                   #Cria gr´pafico somente para a serie da diferença

ppib <- diff(PIB)                                #Calcula a primeira diferença da série de dados
pidiferenca1 <- (data.frame(PIB[2:18],ppib))       #Exibe a tabela da série original coma diferença <- 
PIDIFERENCA <- ts(pidiferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(PIDIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(ppib, type="l")                                   #Cria gr´pafico somente para a serie da diferença


#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

### Análise dados Emprego

pdemprego1 <- diff(emprego)                                            #Calculando-se a primeira diferença
TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego1_trend) 

pdemprego2 <- diff(diff(emprego))                                      #Calculando-se a segunda diferença
TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego2_trend)

#Estimando a série temporal

arima123 <- arima(emprego, c(1,2,3))

#ARMA
arima120 <- arima(emprego, c(1,2,0))
arima121 <- arima(emprego, c(1,2,1))
arima122 <- arima(emprego, c(1,2,2))

arima220 <- arima(emprego, c(2,2,0))
arima221 <- arima(emprego, c(2,2,1))
arima222 <- arima(emprego, c(2,2,2))
arima223 <- arima(emprego, c(2,2,3))
#MA
arima021 <- arima(emprego, c(0,2,1))
arima022 <- arima(emprego, c(0,2,2))
arima023 <- arima(emprego, c(0,2,3))
#AR
arima020 <- arima(emprego, c(0,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,arima221,
                   arima222,arima223,arima021,arima021, arima022,
                   arima023,arima020)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
AIC
BIC

Modelo <-c("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima020") 
Resultados <- data.frame(Modelo,AIC,BIC)
Resultados

### Análise para o Câmbio

pcambio1 <- diff(cambio)                                            #Calculando-se a primeira diferença
TesteDF_CAMBIO1_trend <- ur.df(pcambio1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_CAMBIO1_trend) 

pcambio2 <- diff(diff(cambio))                                      #Calculando-se a segunda diferença
TesteDF_CAMBIO2_trend <- ur.df(pcambio2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_CAMBIO2_trend)

#Estimando a série temporal

carima122 <- arima(cambio, c(1,2,2))

#ARMA
carima120 <- arima(cambio, c(1,2,0))
carima121 <- arima(cambio, c(1,2,1))


carima220 <- arima(cambio, c(2,2,0))
carima221 <- arima(cambio, c(2,2,1))

#MA
carima021 <- arima(cambio, c(0,2,1))

#AR
carima020 <- arima(cambio, c(0,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(carima122,carima120,carima121,
                   carima220,carima221,
                   carima021,carima020)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
AIC
BIC

Modelo <-c("carima122","carima120","carima121",
           "carima220","carima221",
           "carima021","carima020") 
Resultados <- data.frame(Modelo,AIC,BIC)
Resultados


###Análise para o PIB

#(AR3;MA1)