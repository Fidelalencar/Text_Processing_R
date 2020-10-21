# Neste script, minha ideia é selecionar frases simples de um texto (legenda)
# e criar uma ferramenta em que eu insira um POS (artigo, pronome, verbo_to_be, preposicao)
# e ela me retorne um simples exercício de substituicao de algumas daquelas palavras


if(require(youtubecaption) == F) install.packages("youtubecaption"); require(youtubecaption)
if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
if(require(RSQLite) == F) install.packages("RSQLite"); require(RSQLite)
if(require(tm) == F) install.packages("tm"); require(tm)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(tokenizers) == F) install.packages("tokenizers"); require(tokenizers)
if(require(purrr) == F) install.packages("purrr"); require(purrr)

#### BAIXANDO A LEGENDA ----
Insira_Link_do_Video_aqui <- "https://www.youtube.com/watch?v=XRr1kaXKBsU&t=2s"

# SCHOOL OF LIFE  
Legendas <- get_caption(url = Insira_Link_do_Video_aqui, 
                        language = "en-GB", # ATENÇÃO PARA A ESCOLHA DA LEGENDA
               savexl = FALSE, openxl = FALSE, path = getwd())

# VERITASIUM
Legendas <- get_caption(url = Insira_Link_do_Video_aqui, language = "en", 
               savexl = FALSE, openxl = FALSE, path = getwd())
####

#### BUSCA/MATCH por verbo to be ----
a <- paste(Legendas$text, sep = " ")
bnumero <- grep("'m | am |'s | is |'re | are ", a, # verb to be affirmative present
               value = F)
aleat <- sample(bnumero, 5)
bfrase <- a[aleat]  #  seleciona aleatoriamente 5 das frases do match e extrai as frases

Legendas[aleat, 1:2]




#### 

# limpando, stemming e convertendo para o formato adequado
Legendas_string <- pull(Legendas, text) # converte o tibble em vetor
Legendas_corpus <- Corpus(VectorSource(Legendas_string)) # converte os vetores em corpus
Legendas_corpus_limpo <- tm_map(Legendas_corpus, removeWords, stopwords("english")) # Limpando as stopwords
Legendas_corpus_limpo <- tm_map(Legendas_corpus_limpo, removeNumbers)
Legendas_corpus_limpo <- tm_map(Legendas_corpus_limpo, stripWhitespace)
Legendas_corpus_limpo <- tm_map(Legendas_corpus_limpo, stemDocument, language = "english")
# inspect(Legendas_corpus_limpo[1])
Legendas_corpus_limpo <- as.data.frame(Legendas_corpus_limpo$content)
Legendas_corpus_limpo <- map(Legendas_corpus_limpo, tokenize_words) # convertendo o dataframe em lista
Legendas_corpus_limpo <- data.frame(matrix(unlist(Legendas_corpus_limpo), # Convertendo a lista em Data Frame
                                           nrow=length(Legendas_corpus_limpo), byrow=F), stringsAsFactors=FALSE)
Legendas_corpus_limpo <- map(Legendas_corpus_limpo, tokenize_words) # convertendo o dataframe em lista
Legendas_corpus_limpo <- data.frame(matrix(unlist(Legendas_corpus_limpo), # Convertendo a lista em Data Frame
                                           nrow=length(Legendas_corpus_limpo), byrow=F), stringsAsFactors=FALSE)
# mudando o nome da coluna
Legendas_corpus_limpo["words"] <- Legendas_corpus_limpo["matrix.unlist.Legendas_corpus_limpo...nrow...length.Legendas_corpus_limpo..."]
Legendas_corpus_limpo$matrix.unlist.Legendas_corpus_limpo...nrow...length.Legendas_corpus_limpo... <- NULL



# Carregando a Lista de 5000 palavras ----
setwd("C:/Users/Paren/Dropbox/Udacity/LearningCommunityConteudo/listas_palavras") 
FiveThouMostFreq <- read.csv2("5000MostFreqWords.csv", sep = ";",  header = T)
# limpando, stemming e convertendo no formato adequado
FiveThouMostFreq <- Corpus(VectorSource(as.vector(FiveThouMostFreq$Word)))
FiveThouMostFreq <- tm_map(FiveThouMostFreq, stemDocument, language = "english")
FiveThouMostFreq <- tm_map(FiveThouMostFreq, stripWhitespace)
FiveThouMostFreq <- data.frame(matrix(unlist(FiveThouMostFreq$content), # Convertendo a lista em Data Frame
                                      nrow=length(FiveThouMostFreq$content), byrow=F), stringsAsFactors=FALSE)
FiveThouMostFreq["words"] <- FiveThouMostFreq["matrix.unlist.FiveThouMostFreq.content...nrow...length.FiveThouMostFreq.content..."]
# mudando o nome da coluna
FiveThouMostFreq$matrix.unlist.FiveThouMostFreq.content...nrow...length.FiveThouMostFreq.content... <- NULL
FiveThouMostFreq$words <- str_replace(FiveThouMostFreq$words, " ", "") # ainda precisava limpar os espacos no inicio das strings


# Fazendo o match e identificando quantas palvras existem em comum entre as legendas e o dicionario de 5 mil palavras 
Common_words <- inner_join(Legendas_corpus_limpo, FiveThouMostFreq) # encontrando as palavras que estao nas legendas e no dicionario
Common_words <- unique(Common_words)  # retirando as duplicatas






##### tentando detectar as palavras mais frequentes em ordem 

# limpando, stemming e convertendo para o formato adequado
Legendas <- get_caption(url = Insira_Link_do_Video_aqui, savexl = FALSE, openxl = FALSE, path = getwd())
Legendas_string <- pull(Legendas, text) # converte o tibble em vetor
Legendas_corpus <- Corpus(VectorSource(Legendas_string)) # converte os vetores em corpus
Legendas_corpus_limpo <- tm_map(Legendas_corpus, removeWords, stopwords("english")) # Limpando as stopwords
Legendas_corpus_limpo <- tm_map(Legendas_corpus_limpo, removeNumbers)
Legendas_corpus_limpo <- tm_map(Legendas_corpus_limpo, removePunctuation)
Legendas_corpus_limpo <- tm_map(Legendas_corpus_limpo, stripWhitespace)
Legendas_corpus_limpo <- tm_map(Legendas_corpus_limpo, stemDocument, language = "english")

# transformando em document term matrix em ordem decrescente
textsplitWord_dtm <- DocumentTermMatrix(Legendas_corpus_limpo)
textsplitWordMatrix <- as.matrix(textsplitWord_dtm)
labels <- labels(textsplitWord_dtm)                   # lista das palavras
textsplitWordcolsums <- colSums(textsplitWordMatrix)  # quantidade de vezes que a palavra aparece
textsplitWordSorted <- sort(textsplitWordcolsums, decreasing = T) # ordenadas



labels(textsplitWordSorted[26]) == FiveThouMostFreq$words[48] # como esta acontecendo o match








##### C1. tentando criar funcao que retorna a quantidade de matches entre uma lista de sentenças e uma lista de termos ----

#POR FAZER
palavras <- list()
for(i in 1: length(sentencas) {
  x <- matches_na_sentenca(Insira_sentenca)
  grep(textsplitWordSorted[iteri], FiveThouMostFreq$words)
  
  
palavras <- list()
for(i in textsplitWordSorted) {
  iteri = 1
  for(e in FiveThouMostFreq$words) {
    itere = 1
    FiveThouMostFreq$words[itere]
  }
  
}


##### C2. Criando a funcao para matche de uma sentenca e lista de termos.
matches_na_sentenca <- function(Insira_sentenca) {

  ##### C2.1. Chamando pacotes ----
  if(require(youtubecaption) == F) install.packages("youtubecaption"); require(youtubecaption)
  if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
  if(require(RSQLite) == F) install.packages("RSQLite"); require(RSQLite)
  if(require(tm) == F) install.packages("tm"); require(tm)
  if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
  if(require(tokenizers) == F) install.packages("tokenizers"); require(tokenizers)
  if(require(purrr) == F) install.packages("purrr"); require(purrr)
  if(require(stringr) == F) install.packages("stringr"); require(stringr)
  
  ##### C2.2. Inserindo e limpando sentença ----
  
  Insira_sentenca <- str_to_lower(Insira_sentenca)
  
  
  ##### C2.3. Inserindo e limpando a lista de palavras - (CArregando apens verbos e nouns da lista das 5 mil palavras) ---- 
  setwd("C:/Users/Paren/Dropbox/Udacity/LearningCommunityConteudo/listas_palavras") 
  FiveThouMostFreq <- read.csv2("5000MostFreqWords.csv", sep = ";",  header = T)
  # ficando somente com verbos e adjetivos
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "a") # artigo / pronomes pessoais
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "c") # conjunções
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "p") # pronouns
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "d") # determines
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "m") # numeral
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "e") # there (existencial)
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "j") # adjetivo
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "u") # interjeição
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "x") # not, n't
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "r") # adverbs
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "i") 
  FiveThouMostFreq <- subset(FiveThouMostFreq, FiveThouMostFreq$Part.of.speech != "t")
  # "v") # 1001 verbos
  # "n") # 2542 nouns
  
  FiveThouMostFreq$Word <- str_trim(FiveThouMostFreq$Word)
  FiveThouMostFreq <- Corpus(VectorSource(as.vector(FiveThouMostFreq$Word)))
  # FiveThouMostFreq <- tm_map(FiveThouMostFreq, stemDocument, language = "english")
  FiveThouMostFreq <- tm_map(FiveThouMostFreq, stripWhitespace)
  FiveThouMostFreq <- data.frame(matrix(unlist(FiveThouMostFreq$content), # Convertendo a lista em Data Frame
                                        nrow=length(FiveThouMostFreq$content), byrow=F), stringsAsFactors=FALSE)
  FiveThouMostFreq["words"] <- FiveThouMostFreq["matrix.unlist.FiveThouMostFreq.content...nrow...length.FiveThouMostFreq.content..."]
  # mudando o nome da coluna
  FiveThouMostFreq$matrix.unlist.FiveThouMostFreq.content...nrow...length.FiveThouMostFreq.content... <- NULL
  lista <- unique(FiveThouMostFreq$words)
  
  
  ##### C2.4. criando o loop em que se insere a sentenca e a lista de termos.----
  vezes = length(lista) # "the" "be"  "and" "of"  "a" 
  vetor <- rep(NA, vezes) # É boa prática não criar objetos que expandem o tamanho. Crie objeto fixo e use indexação para preenchê-lo.
  i = 1
  for(it in 1:vezes) {
    palavra <- paste("(^| )", lista[it], "( |$)", sep = "", collapse = "")
    m <- gregexpr(palavra, Insira_sentenca)
    m1 <- unlist(m)
    if(m1 == "-1") {
      match = 0
    } else {
      match = length(m[[1]])
    }
    vetor[i] <- match # indexacao
    i = i+1
  }
  vetor
  soma <- sum(vetor)
  return(soma)
} 

##### C4.RODANDO A FUNCAO ----

Insira_sentenca <- "has R is programming has language and free software have environment for statistical may computing"
Insira_sentenca <- "the man in the house is my friend. there is no other one."
Insira_sentenca <- "A GNU package, have the official R software environment is written primarily in C, Fortran, and R itself (thus, it is partially self-hosting) and is freely available under the GNU General Public License. Pre-compiled executables are provided for various operating systems. Although R has a command line interface, there are several third-party graphical user interfaces, such as RStudio, an integrated development environment, and Jupyter, a notebook interface."

x <- matches_na_sentenca(Insira_sentenca)






##### D1. OUTRAS COISAS QUE PODEM SER UTEIS NOS PROXIMOS PASSOS ----

# 3 como manipular com characters para strings
length(FiveThouMostFreq$words[3])
length(toString(FiveThouMostFreq$words[3]))

if(require(stringr) == F) install.packages("stringr"); require(stringr)
FiveThouMostFreq$words[3][str_length(FiveThouMostFreq$words[3])]


# como usar funcoes repetidamente
if(require(purrr) == F) install.packages("purrr"); require(purrr)
tabela <- map(c("casa", "relogio"),
              "casa", grep)


# como adicionar elemento a lista
x <- list("a","b")
x <- append(letters[3], x)

























