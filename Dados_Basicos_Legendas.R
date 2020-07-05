# CAPTURA DE LEGENDAS do YouTube e análise: com youtubecaption ----
# nesse primeiro script tento criar uma tabela com informações básicas 
# sobre a legenda de uma lista de videos no Youtube.



##### LISTA DE VIDEOS ----
videos <- c(
  "https://www.youtube.com/watch?v=HIdflecvQG8", 
  "https://www.youtube.com/watch?v=fK2IJ43ppd0", 
  "https://www.youtube.com/watch?v=JCTzbc76WXY", 
  "https://www.youtube.com/watch?v=QRt7LjqJ45k", 
  "https://www.youtube.com/watch?v=-LKVUarhtvE"
)

##### CRIANDO A FUNCAO ----
Dados_basicos_legenda <- function(Insira_Link_do_Video_aqui) {
  
  # Captura da legenda:
  if(require(youtubecaption) == F) install.packages("youtubecaption"); require(youtubecaption)
  
  
  #Insira_Link_do_Video_aqui <- "https://www.youtube.com/watch?v=fK2IJ43ppd0"
  
  
  Legendas <- get_caption(url = Insira_Link_do_Video_aqui, savexl = FALSE, openxl = FALSE, path = getwd())
  # video sobre "slow aging": "https://www.youtube.com/watch?v=QRt7LjqJ45k"
  # video sobre sabao e Covid-19: "https://www.youtube.com/watch?v=-LKVUarhtvE"
  
  
  if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
  if(require(RSQLite) == F) install.packages("RSQLite"); require(RSQLite)
  Legendas_vec <- pull(Legendas, start) # essa linha converte o tibble em vetor
  TdurationLegendas <- max(Legendas_vec) # ___ segundos = ___ minutos
  
  Legendas_string <- pull(Legendas, text) # essa linha converte o tibble em vetor
  NcharLegendas <- nchar(Legendas_string) # numero de caracteres por linha
  
  library(stringr)
  Nwords_Legendas <- str_count(Legendas_string, boundary("word"))
  TpalavrasLegendas <-sum(Nwords_Legendas) # ___ palavras
  
  
  if(require(tm) == F) install.packages("tm"); require(tm)
  Legendas_corpus <- Corpus(VectorSource(Legendas_string)) # criando corpus
  
  Legendas_corpus <- tm_map(Legendas_corpus, tolower)
  Legendas_corpus <- tm_map(Legendas_corpus, removeNumbers)
  Legendas_corpus <- tm_map(Legendas_corpus, removePunctuation)
  Legendas_corpus <- tm_map(Legendas_corpus, removeWords, stopwords("english"))
  Legendas_corpus <- tm_map(Legendas_corpus, stripWhitespace)
  Legendas_corpus <- tm_map(Legendas_corpus, stemDocument, language = "english")
  
  # para saber o numero de palavras sem repetição
  textsplitWord_dtm <- DocumentTermMatrix(Legendas_corpus)
  textsplitWordMatrix <- as.matrix(textsplitWord_dtm)
  textsplitWordSorted <- sort(colSums(textsplitWordMatrix), decreasing = T)
  textsplitWordDF <- data.frame(word = names(textsplitWordSorted), freq = textsplitWordSorted)
  NwordsLegendas <- count(textsplitWordDF)  # ___ palavras diferentes
  
  #resultados : duracao / num de palavras / num de palavras diferentes
  TpalavrasLegendas  # ___ palavras
  NwordsLegendas     # ___  palavras diferentes
  TdurationLegendas  # ___ segundos
  ratioVocPorsec <- NwordsLegendas/TdurationLegendas # ___ palavras diferentes por segundo
  ratioPalavPorsec <- TpalavrasLegendas/TdurationLegendas # ___ palavras por segundo
  
  N_de_Palavras <- round(as.numeric(TpalavrasLegendas), 1)
  Vocabulario <- round(as.numeric(NwordsLegendas), 1)
  Tempo_minutos <- round((TdurationLegendas/60), 1)
  Vocabulario_PorSeg <- round(as.numeric(ratioVocPorsec), 2)
  Palavras_PorSeg <- round(as.numeric(ratioPalavPorsec), 2)
  
  #rm("Insira_Link_do_Video_aqui", "Legendas", "Legendas_corpus", "Legendas_string", 
  #   "Legendas_vec", "N_de_Palavras", "NcharLegendas", "Nwords_Legendas", "NwordsLegendas", 
  #   "Palavras_PorSeg", "ratioPalavPorsec", "ratioVocPorsec", "TdurationLegendas", 
  #   "Tempo_minutos", "textsplitWord_dtm", "textsplitWordDF", "textsplitWordMatrix", "textsplitWordSorted", 
  #   "TpalavrasLegendas", "Vocabulario", "Vocabulario_PorSeg")
  
  tabela <- as.data.frame(cbind(N_de_Palavras, Vocabulario, Tempo_minutos, Vocabulario_PorSeg, Palavras_PorSeg))
  return(tabela)
}

# ----

##### USANDO A FUNCAO !!!

if(require(purrr) == F) install.packages("purrr"); require(purrr)
tabela <- as.data.frame(
  matrix(unlist(
    map(videos, Dados_basicos_legenda)),
    nrow = length(videos),
    byrow = TRUE))

colnames(tabela) <- c("Número de Palavras", "Vocabulario", 
                      "Tempo em minutos", "Vocabulario por segundo", 
                      "Palavras por segundo")



