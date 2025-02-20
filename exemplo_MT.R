#  *******************************************************************************  
#              Exemplo - Programa R - Intervalo de Confiança           
#             e Testes de Hipóteses para Média, Proporção e Variância            
#  ******************************************************************************* 


# Importando dados
# Para ler arquivo de dados em formato txt
dados <- read.table(file.choose(), header = TRUE)
dados
# Para ler arquivo de dados em formato csv
dados1 <- read.csv2(file.choose(), header = TRUE)
dados1


# Para ler um "resumo" da base de dados, onde será 
# apresentado o nome das variáveis com alguns valores
head(dados1)


# Dimensão da base de dados
dim(dados) 

# Agora vamos ter acesso a cada variável individualmente
sexo <- factor(dados$Sexo)
idade <- dados$Idade
ginst <- factor(dados$g_Instru)
lresid <- factor(dados$l_Resid)
renda <- dados$Renda
renda


# **************************************************************** #
# Intervalo de Confiança e Teste para Media Populacional  #
# **************************************************************** #
summary(idade)
mean(idade)
t.test(idade)

# Indicando a confianca  #
t.test(idade, conf.level=0.9)

# Especificando o valor de mu0  #
t.test(idade, mu=30)

# Especificando a hipotese alternativa  #
# alternative = "two.sided" OU "less" OU "greater"  #
t.test(idade, mu=30, alternative =  "greater" )


# ******************************************************************** #
# Intervalo de Confiança e Teste para Variância Populacional  #
# ******************************************************************** #
summary(idade)
mean(idade)
t.test(idade)

# Indicando a confianca  #
var.test(x=idade, conf.level=0.9)

# Especificando o valor de mu0  #
var.test(idade, sigma2=36)

# Especificando a hipótese alternativa  #
# alternative = "two.sided" OU "less" OU "greater"  #
var.test(x=idade, null.value=36, alternative =  "greater" )



# ******************************************************************** #
# Intervalo de Confianca e Teste para Proporcao Populacinal  #
# ******************************************************************** #
tabsexo <- table(dados$Sexo)
tabsexo

prop.test(tabsexo)
#  ou  #
binom.test(tabsexo)

# Indicando a confianca  #
prop.test(tabsexo, conf.level=0.9)
#  ou  #
binom.test(tabsexo,conf.level=0.9)

# Especificando o valor de P0  #
prop.test(tabsexo, p=0.45)
#  ou  #
binom.test(tabsexo, p=0,45)

# Especificando a hipotese alternativa  #
# alternative = "two.sided" OU "less" OU "greater"  #
prop.test(tabsexo, p=0.45, alternative =  "less")
#  ou  #
binom.test(tabsexo, p=0,45, alternative =  "less")





# ******************************************************************** #
# Teste para Comparação de duas Variâncias Populacionais   #
# ******************************************************************** #

#   Dados  #    
amostra1 <- c(0.48, 0.39, 0.42, 0.52, 0.40, 0.48, 0.52, 0.52)
amostra2 <- c(0.38, 0.37, 0.39, 0.41, 0.38, 0.39, 0.40, 0.39)

var.test(amostra1, amostra2, null.value = 1)


# Especificando a hipótese alternativa  #
# alternative = "two.sided" OU "less" OU "greater"  #
var.test(amostra1, amostra2, null.value=1, alternative =  "two.sided" )


# ******************************************************************** #
#    Teste para Comparação de duas Médias Populacionais     #
#                      AMOSTRAS   INDEPENDENTES                      #
# ******************************************************************** #
#  A) - Variâncias populacionais são DESCONHECIDAS E IGUAIS  # 
# ************************************************************************** #

#   Dados  #    
amostra1 <- c(0.48, 0.39, 0.42, 0.52, 0.40, 0.48, 0.52, 0.52)
amostra2 <- c(0.38, 0.37, 0.39, 0.41, 0.38, 0.39, 0.40, 0.39)


# Especificando que variâncias são iguais #
# var.equal = TRUE  #
t.test(amostra1, amostra2, mu=0, var.equal=TRUE )


# Especificando a hipótese alternativa  #
# alternative = "two.sided" OU "less" OU "greater"  #
t.test(amostra1, amostra2, mu=0, alternative =  "greater" , var.equal=TRUE )


# ********************************************************************************** #
#  B) - Variâncias populacionais são DESCONHECIDAS E DIFERENTES  # 
# ********************************************************************************** #

# Especificando que variâncias são iguais #
# var.equal = FALSE  #
t.test(amostra1, amostra2, mu=0, var.equal=FALSE )




# Especificando a hipótese alternativa  #
# alternative = "two.sided" OU "less" OU "greater"  #
t.test(amostra1, amostra2, mu=0, alternative =  "greater", var.equal=FALSE )

# ******************************************************************** #
#    Teste para Comparação de duas Médias Populacionais     #
#            AMOSTRAS   DEPENDENTES ou PAREADAS          #
# ******************************************************************** #

#   Dados  #    
marcaA <- c(80, 72, 65, 78, 85)
marcaB <- c(75, 70, 60, 72, 78)

t.test(marcaA, marcaB, paired = TRUE )


# ************************************************************************** #
#    Teste Qui-quadrado de Independência ou de Homogeneidade   #
#                                                                                                          #
# ************************************************************************** #

# Para ler arquivo de dados em formato csv
dados1 <- read.csv2(file.choose(), header = TRUE)
dados1


# Agora vamos ter acesso a cada variável individualmente
sexo <- factor(dados1$Sexo)
idade <- dados1$Idade
ginst <- factor(dados1$g_Instru)
lresid <- factor(dados1$l_Resid)
renda <- dados1$Renda


# *** Tabelas de Contingência  ***
tabela0 <- table(dados1$Sexo,dados1$g_Instru)
tabela0 

     # ou... com os rótulos das categorias
tabela1 <- table(sexo,ginst)
tabela1

# ** Colocando rótulos na tabela - alternativa
rownames(tabela1)<-c("masculino","feminino")
colnames(tabela1)<-c("Fundamental Incompleto", "Fundamental Completo", "Médio")
tabela1

# ** Trocando a posição das variáveis na tabela 
tabela2 <- table(ginst,sexo)
tabela2


#   Totais marginais (junto com a tabela) 
addmargins(tabela2)


# Frequência Relativa 
prop.table(tabela2)     # em relação ao total geral (n)

prop.table(tabela2,1)   # em relação ao total da linha 

prop.table(tabela2,2)   # em relação ao total da coluna


# Teste Qui-quadrado 
chisq.test(ginst, sexo)




# ************************************************************************** #
#             Teste de Associação entre variáveis Quantitativas              #
#                    Coeficiente de correlação linear de Pearson                #                                                                      #                                                                                                          #
# ************************************************************************** #

cor.test(idade, renda, alternative="two.sided", method="pearson", conf.level=0.90)




