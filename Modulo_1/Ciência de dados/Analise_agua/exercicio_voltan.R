
#######################################
#                                     #
# Entrega exercícios - Semana 8       #
#                                     #
# Aluno: José Luiz Neves Voltan       #
#                                     #
#######################################

#Considerando a base de dados SISAGUA, realize uma visualização gráfica apresentando:
#1 - O que o gráfico deve mostrar? Formule o enunciado do gráfico - note que você deverá preparar o conjunto de dados SISAGUA para refletir o que você quer mostrar no seu gráfico.
#2 - O código R necessário para preparar os dados
#3 - O código R necessário para fazer o gráfico

####################
# Sobre o Dataset  #
####################

#"A base de dados se refere ao monitoramente da qualidade da água realizado 
#pelo prestador de serviço em frequência inferior à mensal. 
# Ela está disponível no Portal Brasileiro de Dados Abertos".


#Importação do dataset e de bibliotecas
library(tidyverse)
analises_agua <- read_csv2("F:\\Ciencia de dados\\Ciencia de dados _ UTFPR\\1 - modulo1\\2-cienc dados\\semana 9\\5controle_mensal_resultado_analises_total.csv", locale = locale(encoding = "ISO-8859-1"))


#PERGUNTAS NORTEADORAS DO TRABALHO

#Q1
# PERGUNTAS: 
# Q-1 -No estado do Rio de Janeiro, considerando o ano maior que 2015, quais são os parâmetros ( Escherichia coli- Cryptosporidium- Giárdia- Vírus- Clorofila-a- Cianobactérias) médios que apareceram na água (RESULTADO)?
#Perceba que estamos interessados na quantidade média do resultado onde aquele parâmetro testou positivo
# Q1-2 Podemos ainda perguntar no sub conjunto mencionado qual a quantidade de cada Categoria do Manancial de Superfície, em relação a cada amostra coletada.


#Q2
#Preparação dos dados:

# Observação inicial do dataset
head(analises_agua)
tail(analises_agua)

#Quantidade de linhas e colunas
dim(analises_agua)
glimpse(analises_agua)


#Iremos filtrar nosso dataset para o universo proposto
analises_agua_RJ <- filter(analises_agua,SIGLA_UF=="RJ" & ANO>2015 )


#################################
# Tratamento da 1ª Análise
#################################

analises_agua_RJ$PARÂMETRO <- str_to_title(analises_agua_RJ$PARÂMETRO)

#Iremos agrupar nosso universo
analises_agua_RJ_agrupado <-group_by(analises_agua_RJ,PARÂMETRO)

#Iremos substituir a vírgula pelo ponto
analises_agua_RJ_agrupado$RESULTADO <-  str_replace(analises_agua_RJ_agrupado$RESULTADO,",",".")

#No dataset em alguns momentos a não presença do parâmetro aparece como 0, ou Ausente ou ainda AUSENTE
# Por isso, na converção para numeric, surgem NAs referentes as Strings Ausente
# Iremos tratar esses casos
analises_agua_RJ_agrupado$RESULTADO <- str_replace(analises_agua_RJ_agrupado$RESULTADO,str_to_upper("AUSENTE"),"0")
analises_agua_RJ_agrupado$RESULTADO <- as.numeric(analises_agua_RJ_agrupado$RESULTADO)
#Mesmo assim ainda existem outros casos de NA
sum(is.na(analises_agua_RJ_agrupado$RESULTADO))

#Sumarizando os dados
Analise_resultado <- summarise(analises_agua_RJ_agrupado,
                                  Média = mean(as.numeric(RESULTADO), na.rm = TRUE))
Analise_resultado 

# Apenas testando se o valor do ParÂmetro Vírus é zero
#Analise_resultado <- filter(analises_agua,SIGLA_UF=="RJ" & ANO>2015 & PARÂMETRO =="Vírus")


#Q3 referente a pergunta de pesquisa formulada em Q 1-1

ggplot(data = Analise_resultado) +
  geom_col(mapping = aes(x = PARÂMETRO, y = Média,
                         group = PARÂMETRO, fill = PARÂMETRO),
           position = position_dodge()) +
  labs(x = "Parâmetro", y = "Quantidade Média Resultado", title = "Resultado médio dos parâmetros no Estado do Rio de Janeiro posteriores a 2015") +
  theme_light() +
  scale_y_continuous(n.breaks = 10)


p <- ggplot(analises_agua_RJ_agrupado, aes(x=PARÂMETRO, y=RESULTADO)) + 
  geom_point(na.rm = TRUE,aes(colour = PARÂMETRO))+
  labs(x = "Parâmetro", y = "Resultado observado", title = "Análise Água no Estado do RJ a partir de 2015")
p
#O gráfico permite obervar os valores obtidos como resultado para os parâmetros apresentados 


#################################
# Tratamento 2ª Análise
#################################

#Parte 2 da 2ª análise
# 1º Iremos susbtituir os NA por "Desconhecido"


analises_agua_RJ$CATEGORIA_MANANCIAL_SUP[which(is.na(analises_agua_RJ$CATEGORIA_MANANCIAL_SUP))] <- "Desconhecido"

# Deixar a 1ª letra maiuscula
analises_agua_RJ$CATEGORIA_MANANCIAL_SUP <- str_to_title(analises_agua_RJ$CATEGORIA_MANANCIAL_SUP)

analises_agua_RJ$CATEGORIA_MANANCIAL_SUP <- factor(analises_agua_RJ$CATEGORIA_MANANCIAL_SUP)
str_to_title

analises_agua_RJ_manacial_agp <- count(analises_agua_RJ,CATEGORIA_MANANCIAL_SUP) %>%
                                  arrange(n)

analises_agua_RJ_manacial_agp$CATEGORIA_MANANCIAL_SUP <- factor(analises_agua_RJ_manacial_agp$CATEGORIA_MANANCIAL_SUP,
                                                 levels = analises_agua_RJ_manacial_agp$CATEGORIA_MANANCIAL_SUP)

# Q3 da 2ª análise
ggplot(data = analises_agua_RJ_manacial_agp) +
  geom_col(mapping = aes(x = CATEGORIA_MANANCIAL_SUP, y = n ,
                         group = CATEGORIA_MANANCIAL_SUP, fill = CATEGORIA_MANANCIAL_SUP),
           position = position_dodge()) +
  labs(x = "Categoria do Manancial", y = "Quantidade", title = "Quantidade de mananciais por tipo em relação as amostras coletadas - RJ") +
  theme_light() +
  scale_y_continuous(n.breaks = 10)

# O resultado nos revela que a maioria das amostras é coletada em rios 
