
#######################################
#                                     #
# Entrega exerc�cios - Semana 8       #
#                                     #
# Aluno: Jos� Luiz Neves Voltan       #
#                                     #
#######################################

#Considerando a base de dados SISAGUA, realize uma visualiza��o gr�fica apresentando:
#1 - O que o gr�fico deve mostrar? Formule o enunciado do gr�fico - note que voc� dever� preparar o conjunto de dados SISAGUA para refletir o que voc� quer mostrar no seu gr�fico.
#2 - O c�digo R necess�rio para preparar os dados
#3 - O c�digo R necess�rio para fazer o gr�fico

####################
# Sobre o Dataset  #
####################

#"A base de dados se refere ao monitoramente da qualidade da �gua realizado 
#pelo prestador de servi�o em frequ�ncia inferior � mensal. 
# Ela est� dispon�vel no Portal Brasileiro de Dados Abertos".


#Importa��o do dataset e de bibliotecas
library(tidyverse)
analises_agua <- read_csv2("F:\\Ciencia de dados\\Ciencia de dados _ UTFPR\\1 - modulo1\\2-cienc dados\\semana 9\\5controle_mensal_resultado_analises_total.csv", locale = locale(encoding = "ISO-8859-1"))


#PERGUNTAS NORTEADORAS DO TRABALHO

#Q1
# PERGUNTAS: 
# Q-1 -No estado do Rio de Janeiro, considerando o ano maior que 2015, quais s�o os par�metros ( Escherichia coli- Cryptosporidium- Gi�rdia- V�rus- Clorofila-a- Cianobact�rias) m�dios que apareceram na �gua (RESULTADO)?
#Perceba que estamos interessados na quantidade m�dia do resultado onde aquele par�metro testou positivo
# Q1-2 Podemos ainda perguntar no sub conjunto mencionado qual a quantidade de cada Categoria do Manancial de Superf�cie, em rela��o a cada amostra coletada.


#Q2
#Prepara��o dos dados:

# Observa��o inicial do dataset
head(analises_agua)
tail(analises_agua)

#Quantidade de linhas e colunas
dim(analises_agua)
glimpse(analises_agua)


#Iremos filtrar nosso dataset para o universo proposto
analises_agua_RJ <- filter(analises_agua,SIGLA_UF=="RJ" & ANO>2015 )


#################################
# Tratamento da 1� An�lise
#################################

analises_agua_RJ$PAR�METRO <- str_to_title(analises_agua_RJ$PAR�METRO)

#Iremos agrupar nosso universo
analises_agua_RJ_agrupado <-group_by(analises_agua_RJ,PAR�METRO)

#Iremos substituir a v�rgula pelo ponto
analises_agua_RJ_agrupado$RESULTADO <-  str_replace(analises_agua_RJ_agrupado$RESULTADO,",",".")

#No dataset em alguns momentos a n�o presen�a do par�metro aparece como 0, ou Ausente ou ainda AUSENTE
# Por isso, na conver��o para numeric, surgem NAs referentes as Strings Ausente
# Iremos tratar esses casos
analises_agua_RJ_agrupado$RESULTADO <- str_replace(analises_agua_RJ_agrupado$RESULTADO,str_to_upper("AUSENTE"),"0")
analises_agua_RJ_agrupado$RESULTADO <- as.numeric(analises_agua_RJ_agrupado$RESULTADO)
#Mesmo assim ainda existem outros casos de NA
sum(is.na(analises_agua_RJ_agrupado$RESULTADO))

#Sumarizando os dados
Analise_resultado <- summarise(analises_agua_RJ_agrupado,
                                  M�dia = mean(as.numeric(RESULTADO), na.rm = TRUE))
Analise_resultado 

# Apenas testando se o valor do Par�metro V�rus � zero
#Analise_resultado <- filter(analises_agua,SIGLA_UF=="RJ" & ANO>2015 & PAR�METRO =="V�rus")


#Q3 referente a pergunta de pesquisa formulada em Q 1-1

ggplot(data = Analise_resultado) +
  geom_col(mapping = aes(x = PAR�METRO, y = M�dia,
                         group = PAR�METRO, fill = PAR�METRO),
           position = position_dodge()) +
  labs(x = "Par�metro", y = "Quantidade M�dia Resultado", title = "Resultado m�dio dos par�metros no Estado do Rio de Janeiro posteriores a 2015") +
  theme_light() +
  scale_y_continuous(n.breaks = 10)


p <- ggplot(analises_agua_RJ_agrupado, aes(x=PAR�METRO, y=RESULTADO)) + 
  geom_point(na.rm = TRUE,aes(colour = PAR�METRO))+
  labs(x = "Par�metro", y = "Resultado observado", title = "An�lise �gua no Estado do RJ a partir de 2015")
p
#O gr�fico permite obervar os valores obtidos como resultado para os par�metros apresentados 


#################################
# Tratamento 2� An�lise
#################################

#Parte 2 da 2� an�lise
# 1� Iremos susbtituir os NA por "Desconhecido"


analises_agua_RJ$CATEGORIA_MANANCIAL_SUP[which(is.na(analises_agua_RJ$CATEGORIA_MANANCIAL_SUP))] <- "Desconhecido"

# Deixar a 1� letra maiuscula
analises_agua_RJ$CATEGORIA_MANANCIAL_SUP <- str_to_title(analises_agua_RJ$CATEGORIA_MANANCIAL_SUP)

analises_agua_RJ$CATEGORIA_MANANCIAL_SUP <- factor(analises_agua_RJ$CATEGORIA_MANANCIAL_SUP)
str_to_title

analises_agua_RJ_manacial_agp <- count(analises_agua_RJ,CATEGORIA_MANANCIAL_SUP) %>%
                                  arrange(n)

analises_agua_RJ_manacial_agp$CATEGORIA_MANANCIAL_SUP <- factor(analises_agua_RJ_manacial_agp$CATEGORIA_MANANCIAL_SUP,
                                                 levels = analises_agua_RJ_manacial_agp$CATEGORIA_MANANCIAL_SUP)

# Q3 da 2� an�lise
ggplot(data = analises_agua_RJ_manacial_agp) +
  geom_col(mapping = aes(x = CATEGORIA_MANANCIAL_SUP, y = n ,
                         group = CATEGORIA_MANANCIAL_SUP, fill = CATEGORIA_MANANCIAL_SUP),
           position = position_dodge()) +
  labs(x = "Categoria do Manancial", y = "Quantidade", title = "Quantidade de mananciais por tipo em rela��o as amostras coletadas - RJ") +
  theme_light() +
  scale_y_continuous(n.breaks = 10)

# O resultado nos revela que a maioria das amostras � coletada em rios 
