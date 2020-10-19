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

#### BUSCA/MATCH ----
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



