# CAPTURA DE LEGENDAS do YouTube: youtubecaption E analise básica do numero de palavras
# Inclui analise usando como Referencia o dicionário de 5 mil palavras 
# ou seja, é como uma ferramenta para buscar textos de niveis de inglês mais baixo  ----

##### CRIANDO A FUNCAO ----
Dados_basicos_legenda_basico <- function(Insira_Link_do_Video_aqui) {
  
  # Captura da legenda:
  if(require(youtubecaption) == F) install.packages("youtubecaption"); require(youtubecaption)
  if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
  if(require(RSQLite) == F) install.packages("RSQLite"); require(RSQLite)
  if(require(tm) == F) install.packages("tm"); require(tm)
  if(require(tokenizers) == F) install.packages("tokenizers"); require(tokenizers)
  if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
  if(require(stringr) == F) install.packages("stringr"); require(stringr)

  
  
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
    
  
  
  #Insira_Link_do_Video_aqui <- "https://www.youtube.com/watch?v=fK2IJ43ppd0"
  
  
  Legendas <- get_caption(url = Insira_Link_do_Video_aqui, savexl = FALSE, openxl = FALSE, path = getwd())
  # video sobre "slow aging": "https://www.youtube.com/watch?v=QRt7LjqJ45k"
  # video sobre sabao e Covid-19: "https://www.youtube.com/watch?v=-LKVUarhtvE"
  

  Legendas_vec <- pull(Legendas, start) # essa linha converte o tibble em vetor
  TdurationLegendas <- max(Legendas_vec) # ___ segundos = ___ minutos
  
  Legendas_string <- pull(Legendas, text) # essa linha converte o tibble em vetor
  NcharLegendas <- nchar(Legendas_string) # numero de caracteres por linha
  
  Nwords_Legendas <- str_count(Legendas_string, boundary("word"))
  TpalavrasLegendas <-sum(Nwords_Legendas) # ___ palavras
  
  
  Legendas_corpus <- Corpus(VectorSource(Legendas_string)) # criando corpus
  
  Legendas_corpus <- tm_map(Legendas_corpus, tolower)
  Legendas_corpus <- tm_map(Legendas_corpus, removeNumbers)
  Legendas_corpus <- tm_map(Legendas_corpus, removePunctuation)
  Legendas_corpus <- tm_map(Legendas_corpus, removeWords, stopwords("english"))
  Legendas_corpus <- tm_map(Legendas_corpus, stripWhitespace)
  Legendas_corpus <- tm_map(Legendas_corpus, stemDocument, language = "english")
 
  
  # ajustando para comparar com o dicionario de 5 mil palavras
  Legendas_corpus_limpo <- as.data.frame(Legendas_corpus$content)
  Legendas_corpus_limpo <- map(Legendas_corpus_limpo, tokenize_words) # convertendo o dataframe em lista
  Legendas_corpus_limpo <- data.frame(matrix(unlist(Legendas_corpus_limpo), # Convertendo a lista em Data Frame
                                             nrow=length(Legendas_corpus_limpo), byrow=F), stringsAsFactors=FALSE)
  Legendas_corpus_limpo <- map(Legendas_corpus_limpo, tokenize_words) # convertendo o dataframe em lista
  Legendas_corpus_limpo <- data.frame(matrix(unlist(Legendas_corpus_limpo), # Convertendo a lista em Data Frame
                                             nrow=length(Legendas_corpus_limpo), byrow=F), stringsAsFactors=FALSE)
  # mudando o nome da coluna
  Legendas_corpus_limpo["words"] <- Legendas_corpus_limpo["matrix.unlist.Legendas_corpus_limpo...nrow...length.Legendas_corpus_limpo..."]
  Legendas_corpus_limpo$matrix.unlist.Legendas_corpus_limpo...nrow...length.Legendas_corpus_limpo... <- NULL
  
  # Fazendo o match e identificando quantas palvras existem em comum entre as legendas e o dicionario de 5 mil palavras 
  Common_words <- inner_join(Legendas_corpus_limpo, FiveThouMostFreq) # encontrando as palavras que estao nas legendas e no dicionario
  Common_words <- unique(Common_words)  # retirando as duplicatas
  

  
  # para saber o numero de palavras sem repetição
  textsplitWord_dtm <- DocumentTermMatrix(Legendas_corpus)
  textsplitWordMatrix <- as.matrix(textsplitWord_dtm)
  textsplitWordSorted <- sort(colSums(textsplitWordMatrix), decreasing = T)
  textsplitWordDF <- data.frame(word = names(textsplitWordSorted), freq = textsplitWordSorted)
  NwordsLegendas <- count(textsplitWordDF)  # ___ palavras diferentes
  
  
  #resultados : duracao / num de palavras / num de palavras diferentes / numero de palavras comuns ao dic de 5000
  TpalavrasLegendas  # ___ palavras
  NwordsLegendas     # ___  palavras diferentes
  TdurationLegendas  # ___ segundos
  ratioVocPorsec <- NwordsLegendas/TdurationLegendas # ___ palavras diferentes por segundo
  ratioPalavPorsec <- TpalavrasLegendas/TdurationLegendas # ___ palavras por segundo
  n_commonwords <- count(Common_words) # numero de palavras comuns ao dic de 5000
  
  N_de_Palavras <- round(as.numeric(TpalavrasLegendas), 1)
  Vocabulario <- round(as.numeric(NwordsLegendas), 1)
  Tempo_minutos <- round((TdurationLegendas/60), 1)
  Vocabulario_PorSeg <- round(as.numeric(ratioVocPorsec), 2)
  Palavras_PorSeg <- round(as.numeric(ratioPalavPorsec), 2)
  
  # 
  
  
  #rm("Insira_Link_do_Video_aqui", "Legendas", "Legendas_corpus", "Legendas_string", 
  #   "Legendas_vec", "N_de_Palavras", "NcharLegendas", "Nwords_Legendas", "NwordsLegendas", 
  #   "Palavras_PorSeg", "ratioPalavPorsec", "ratioVocPorsec", "TdurationLegendas", 
  #   "Tempo_minutos", "textsplitWord_dtm", "textsplitWordDF", "textsplitWordMatrix", "textsplitWordSorted", 
  #   "TpalavrasLegendas", "Vocabulario", "Vocabulario_PorSeg")
  
  tabela <- as.data.frame(cbind(N_de_Palavras, Vocabulario, Tempo_minutos, Vocabulario_PorSeg, Palavras_PorSeg, n_commonwords))
  colnames(tabela) <- c("Número de Palavras", "Vocabulario", 
                        "Tempo em minutos", "Vocabulario por segundo", 
                        "Palavras por segundo", "Palavras entre as 5 mil")
  return(tabela)
}

# ----

##### USANDO A FUNCAO PARA UM UNICO VIDEO ----
Insira_Link_do_Video_aqui <- "https://www.youtube.com/watch?v=fK2IJ43ppd0"
Dados_basicos_legenda_basico(Insira_Link_do_Video_aqui)



##### USANDO A FUNCAO REPETIDAMENTE PARA GERAR TABELA COM DADOS DE VARIOS VIDEOS!!! ----

videos <- c(
  "https://www.youtube.com/watch?v=HIdflecvQG8", 
  "https://www.youtube.com/watch?v=fK2IJ43ppd0", 
  "https://www.youtube.com/watch?v=JCTzbc76WXY", 
  "https://www.youtube.com/watch?v=QRt7LjqJ45k", 
  "https://www.youtube.com/watch?v=-LKVUarhtvE",
  "https://www.youtube.com/watch?v=XRr1kaXKBsU",
  "https://www.youtube.com/watch?v=3LopI4YeC4I"
)

if(require(purrr) == F) install.packages("purrr"); require(purrr)
tabela <- as.data.frame(
  matrix(unlist(
    map(videos, Dados_basicos_legenda)),
    nrow = length(videos),
    byrow = TRUE))

colnames(tabela) <- c("Número de Palavras", "Vocabulario", 
                      "Tempo em minutos", "Vocabulario por segundo", 
                      "Palavras por segundo", "Palavras entre as 5 mil")

###########################################################
