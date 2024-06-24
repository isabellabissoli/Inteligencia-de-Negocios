## Analisando dados abertos de viagens a serviço

# A proposta deste tópico é colocar em prática algumas das funções do 
# R trabalhando com a análise de dados abertos de viagens a serviço, 
# com o intuito de subsidiar a tomada de medidas mais eficientes 
# na redução dos gastos com os custos dessas viagens no setor público.
# Para alcançarmos o nosso objetivo, precisamos seguir algumas etapas 
# básicas:

## Definição do problema:

# Para resolver um problema, primeiramente temos que entendê-lo. 
# Assim, precisamos entender os gastos com viagens a serviço para 
# tomar medidas mais eficientes e, com isso, reduzir os custos
# dessas viagens.

# Vamos levantar algumas questões relevantes acerca dessa problemática:

# Qual é o valor gasto por órgão?
# Qual é o valor gasto por cidade?
# Qual é a quantidade de viagens por mês?

## Obtenção dos dados:

# Primeiramente, vamos acessar o Portal da Transparência do Governo 
# Federal por meio do endereço www.portaldatransparencia.gov.br

# Ao final da página inicial, há o link "Dados do Portal". Vamos 
# clicar nele.

# Em seguida, é exibida uma lista de bases separadas por temas.
# Vamos clicar na base "Viagens a Serviço", e depois no arquivo
# "Viagens a serviço".

# Na tela que se abre, há a opção de escolher qual exercício, ou seja,
# o ano que você deseja extrair os dados de viagens realizadas a 
# serviço.

# No campo "Exercícios Disponíveis", vamos selecionar o ano 2023 e 
# clicar em "Baixar".

# Ainda nesta tela, você também tem acesso ao dicionário de dados,
# que possui a descrição de cada campo das tabelas. Vamos então
# clicar no link "dicionário de dados".

# Em seguida, aparece na tela "Dicionários de Dados - Viagens a 
# Serviço - Pagamentos" com a descrição de cada um dos campos das 
# tabelas. Essas informações são muito úteis para o nosso estudo.

# Após fazer o download dos dados, é necessário carregá-los:

# Carregando os dados
?read.csv

viagens <- read.csv(
  file = "/Users/20211bsi0305/Documents/2023_Viagem.csv",
  sep = ';',
  dec = ',',
  fileEncoding='latin1',
  check.names=F
)

head(viagens)

# Outra forma de dispor os dados é usando a função "View". Ela
# permite uma melhor apresentação da tabela e possibilita verificar
# se os dados foram carregados corretamente:

View(viagens)

# Usando a função "dim" podemos verificar o número de observações 
# e colunas do dataset

dim(viagens)

# Para recuperar algumas informações do dataset, como valor mínimo
# máximo e média das viagens a serviço, utilizamos a função 
# "summary" e executamos em um data frame
?summary

summary(viagens)

summary(viagens$`Valor passagens`)

# Neste momento, vamos utilizar a função "glimpse" do pacote "dplyr"
# para verificar o tipo dos dados de cada coluna

#install.packages("dplyr")
library(dplyr)

glimpse(viagens)

## Transformação dos dados obtidos

# É importante ter em mente que, na linguagem R, diversas 
# transformações nos dados coletados podem ser realizadas, a 
# depender do tipo de dado e do objetivo da análise.

# Agora, vamos estudar a transformação dos dados obtidos,
# a terceira etapa da nossa análise dos dados abertos de
# viagens a serviço.

# Pode-se perceber que os campos "Período - Data de Início"
# e "Período - Data de fim" são strings, por isso será
# necessário transformar para o tipo data.

?as.Date

# Criamos uma coluna nova "data.inicio" para não alterar a coluna
# original

viagens$data.inicio <- as.Date(viagens$`Período - Data de início`,"%d/%m/%Y")

glimpse(viagens)

# Agora, vamos formatar o campo "data" para trabalhar apenas com o
# mês e ano

viagens$data.inicio.formatada <- format(viagens$data.inicio,"%Y-%m")
viagens$data.inicio.formatada

## Exploração dos dados

# Agora, vamos aprender como realizar a análise exploratória dos
# dados. Lembrando que podemos criar um histograma com a função
# "hist"

hist(viagens$`Valor passagens`)

# Após executar a função "summary" é possível perceber que o 
# valor máximo está muito distante da média, o que indica a 
# presença de outliers. Os outliers são valores que fogem da 
# normalidade dos dados, fazendo com que o resultado de uma
# análise não reflita a realidade

# Valores min, max, média ... da coluna valor
summary(viagens$`Valor passagens`)

# Visualizando os valores em um boxplot
boxplot(viagens$`Valor passagens`)

# calculando o desvio padrão
sd(viagens$`Valor passagens`)

# Vamos aprender mais 2 funções interessantes nessa fase de 
# exploração dos dados
?is.na # campos com valores não preenchidos
? colSums 

# quantidade de campos não preenchidos
colSums(is.na(viagens))

# Seguindo com a exploração dos dados, é possível verificar a 
# quantidade de ocorrências para cada categoria de uma determinada
# coluna, a quantidade de ocorrências para cada classe e o valor
# percentual

str(viagens$Situação)

table(viagens$Situação)

prop.table(table(viagens$Situação))*100

## Visualização dos resultados

# A visualização dos resultados é a etapa final do nosso estudo 
# acerca da análise dos dados de viagens a serviço. Assim, é o 
# momento de responder às questões levantadas na primeira fase:
# definição do problema. Vamos relembrá-las?
# Qual é o valor gasto por órgão? Vamos mudar para pessoas!!
# Qual é o valor gasto por cidade?
# Qual é a quantidade de viagens por mês?

# 1. Pessoas que estão gastando mais com passagens aéreas?

# desconsiderando os nomes não informados por questão de sigilo
viagens.sem.prot <- filter(viagens,Nome != "Informações protegidas por sigilo")

# criando um dataframe com os 15 nomes que gastam mais

p1 <- viagens.sem.prot %>%
  group_by(`Nome`) %>%
  summarise(n = sum(`Valor passagens`)) %>%
  arrange(desc(n)) %>%
  top_n(15)

names(p1) <- c("pessoa", "valor")

# Estranhamente não temos os órgãos solicitantes ???
p1

# plotando os dados com o ggplot
library(ggplot2)
# reorder - ordenar os valores / coord_flip - inverter o gráfico
ggplot(p1,aes(x=reorder(pessoa,valor),y=valor))+
  geom_bar(stat = "identity") + 
  coord_flip()+
  labs(x="Pessoa",y="Valor")

# Primeiro, vamos agrupar o conjunto de dados por destino das 
# viagens, usando a função "group_by"

p2 <- viagens %>%
  group_by(Destinos) %>%
  summarise(n = sum(`Valor passagens`)) %>%
  arrange(desc(n)) %>%
  top_n(15)

# Renomeando as colunas usando a função "names"

names(p2) <- c('destino','valor')

p2

# Criando o gráfico

# "fill" é usado para mudar a cor das barras
# usando a função "geom_text" um texto é inderido diretamente
# no gráfico
# a função "label" insere um retângulo atrás do texto a fim
# de facilitar a leitura
# as funções "vjust" e "size" servem para alterar o tamanho
# e posição dos rótulos das barras
ggplot(p2,aes(x = reorder(destino,valor),y = valor))+
  geom_bar(stat = "identity", fill = "#0ba791")+
  geom_text(aes(label=valor), vjust=0.3, size=3)+
  coord_flip()+
  labs(x="Destino", y="Valor")

# Se os valores exibidos no gráfico forem maiores que 999, exibirá no formato exponencial
options(scipen = 999)

# Agora, veremos como descobrir a quantidade de viagens realizadas
# por mês
p3 <- viagens %>%
  group_by(data.inicio.formatada) %>%
  summarise(qtd = n_distinct(`Identificador do processo de viagem`))

head(p3)

# Agora que os dados já estão preparados, vamos criar o gráfico
# utilizando a função "ggplot()" e inserindo data frame

ggplot(p3,aes(x=data.inicio.formatada, y=qtd, group=1))+
  geom_line()+
  geom_point()

# Visualização dos dados - vamos aprender como apresentar as
# análises por meio da ferramenta R Markdown
#install.packages("rmarkdown")
#install.packages('tinytex')

#tinytex::install_tinytex()

# Após a conclusão da instalação, vamos acessar, no menu superior
# a aba File, em seguida, New File, e por fim clicar em R Markdown

# Aparecerá uma nova janela. Vamos preencher o campo Title com
# Relatório, marcar a opção PDF, e clicar em OK

# Em seguida aparecerá uma nova aba denominada Relatório.Rmd

# O documento no formato R Markdown consiste em blocos de código 
# e blocos de texto utilizados para documentar a análise

# Para adicionar mais blocos de código é necessário acessar o menu
# insert, localizado à direita desta janela, e clicar na opção R

# Para gerar um documento referente ao seu relatório, é necessário
# selecionar Knit, localizado à esquerda desta janela, e clicar
# na opção Knit to PDF