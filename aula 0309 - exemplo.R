#pacotes
install.packages("kmed")
library(kmed)
library(dplyr)
install.packages("sjPlot")
library(sjPlot)
library(ggplot2)

#preparação dos dados
dados<-heart

#1 preparação de dados
#selecionando colunas
dados_novos <- dados%>%select(age,sex,cp,thalach,class)


#renomeando colunas
dados_novos <- dados_novos%>%rename(idade=age,sexo=sex,dor_no_peito=cp,freq_maxima=thalach,doenca_cardiaca=class)

#renomeando valores para a variavel sexo

dados_novos$sexo<-factor(dados_novos$sexo,levels = c(FALSE,TRUE),labels = c("feminino","masculino"))

#renomeando valores para dor no peito
dados_novos$dor_no_peito<-factor(dados_novos$dor_no_peito,
                                 levels= 1:4,
                                 labels = c("angina tipica","angina atipica","dor não_anginal","assintomático"))

#codificando para doente ou não doente

dados_novos$doenca_cardiaca<-ifelse(dados_novos$doenca_cardiaca==0,0,1)

#Passando para fator

dados_novos$doenca_cardiaca<-factor(dados_novos$doenca_cardiaca,
                                    levels = c(0,1),
                                    labels = c('sem doença','com doença'))


#2-Modelo 1 - Regressão logistica simples-Variável independente quantitaviva


ggplot(data=dados_novos,aes(y=idade,fill=doenca_cardiaca))+
  geom_boxplot()

dados_novos%>%group_by(doenca_cardiaca)%>%summarise(media=mean(idade))

modelo_1<-glm(doenca_cardiaca~idade,data=dados_novos,family = 'binomial')
summary(modelo_1)


#Quando o coeficiente é igual a zero,x e y são independentes
#Quando o coeficiente é>0, a probabilidade de y=1(doente) aumenta com x
#Quando o coeficiente é<0, a probabilidade de y=1(doente) diminui

#quantificando a  relação
exp(coef(modelo_1)['idade'])

#Um ano extra de vida aumenta a chance de desenvolver uma doença cardíaca por um fator de 1,05

#Análise do intercepto

exp(coef(modelo_1)[1])/(1+exp(coef(modelo_1)[1]))


# Uma pessoa de 0 ano tem uma chance de desenvolver doença cardiaca de 0,04

#Predizendo a probabilidade de ter doença

novo_paciente<-data.frame(idade=30)
predict(modelo_1,novo_paciente,type='response')


#Uma pessoa de 30 anos tem chance de 18,78% de ter doença cardiaca

plot_model(modelo_1,type = "pred",terms = "idade")





#3 modelo 2 regressão logistica simples - variavel independente qualitativa

#Análise gráfica
ggplot(data=dados_novos,aes(x=sexo,fill=doenca_cardiaca))+
  geom_bar()

# Teste qui-quadrado para idependencia

chisq.test(table(dados_novos$doenca_cardiaca,dados_novos$sexo))

#Valor p<0,05. Podemos rejeitar a hipoteses de independencia das variaveis e supor que elas sejam associadas, com um nivel de confiança de 95%.


modelo_2<-glm(doenca_cardiaca~sexo,data = dados_novos,family = 'binomial')
summary(modelo_2)


exp(coef(modelo_2)['sexomasculino'])

#uma pessoa do sexo mascuino tem um fator multiplicado por 3,574 de desenvolver doença cardiaca.

plot_model(modelo_2,type='pred',terms = 'sexo')



#4 Modelo 3 Várias variáveis
modelo_3<-glm(doenca_cardiaca~.,data = dados_novos,family = 'binomial')
summary(modelo_3)



























