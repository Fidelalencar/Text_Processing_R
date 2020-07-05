# Testando a limpeza de textos
# nesse script, testo operações de limpesa de texto

if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
if(require(RSQLite) == F) install.packages("RSQLite"); require(RSQLite)
if(require(youtubecaption) == F) install.packages("youtubecaption"); require(youtubecaption)
if(require(stringr) == F) install.packages("stringr"); require(stringr)
if(require(tm) == F) install.packages("tm"); require(tm)

V1 <- "https://www.youtube.com/watch?v=HIdflecvQG8" 
V1 <- "https://www.youtube.com/watch?v=fK2IJ43ppd0"
V1 <- "https://www.youtube.com/watch?v=JCTzbc76WXY"
V1 <- "https://www.youtube.com/watch?v=QRt7LjqJ45k" # video sobre "slow aging"
V1 <- "https://www.youtube.com/watch?v=-LKVUarhtvE" # video sobre sabao e Covid-19

Legendas <- get_caption(url = V1, savexl = FALSE, openxl = FALSE, path = getwd()) # Puxando a legenda

Legendas_vec <- pull(Legendas, text) # essa linha converte a coluna text de tibble para vetor
NcharLegendas <- nchar(Legendas_vec) # numero de caracteres por linha
Nwords_Legendas <- str_count(Legendas_vec, boundary("word")) # numero de palavras por linha
TpalavrasLegendas <-sum(Nwords_Legendas) # qntdd total de palavras

Legendas_corpus <- Corpus(VectorSource(Legendas_vec)) # transformando vetor em corpus

inspect(Legendas_corpus[22:27])


Legendas_corpus <- tm_map(Legendas_corpus, tolower) # Aparentemente, as legendas nao veem com letra maiuscula


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