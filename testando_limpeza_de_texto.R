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

Legendas_vec <- pull(Legendas4, text) # essa linha converte a coluna text de tibble para vetor
NcharLegendas <- nchar(Legendas_vec) # numero de caracteres por linha
Nwords_Legendas <- str_count(Legendas_vec, boundary("word")) # numero de palavras por linha
TpalavrasLegendas <- sum(Nwords_Legendas) # qntdd total de palavras

Legendas_corpus <- Corpus(VectorSource(Legendas_vec)) # transformando vetor em corpus
inspect(Legendas_corpus[22:27]) # para ler textos do corpos


# Legendas_corpus <- tm_map(Legendas_corpus, tolower) # Aparentemente, as legendas nao veem com letra maiuscula

# CONTINUAR ...


Legendas_corpus <- tm_map(Legendas_corpus, removeNumbers) # Limpeza do corpus
Legendas_corpus <- tm_map(Legendas_corpus, removePunctuation)
Legendas_corpus <- tm_map(Legendas_corpus, removeWords, stopwords("english")) 
# exemplos de diferença da redução de palavras com e sem stopwords: 1455 -> 760  / 2514 -> 1357 / 2449 -> 1414
Legendas_corpus <- tm_map(Legendas_corpus, stripWhitespace)
Legendas_corpus <- tm_map(Legendas_corpus, stemDocument, language = "english")
# Diferença de caracteres com e sem stemming: 5494 -> 4556  / 9881 -> 8271  / 9926 -> 8567


# devolvendo para a classe vetor:
legendas_limpas <- Legendas_corpus$content # vetor com as legendas limpas
sum(nchar(legendas_limpas))  
sum(str_count(legendas_limpas, boundary("word"))) 


# para fazer "wordcloud"
if(require(wordcloud) == F) install.packages("wordcloud"); require(wordcloud)
wordcloud(Legendas_corpus, # fazendo a wordcloud com base no corpus
          max.words = 100, # numero maximo de palavras da nuve,
          random.order = T,
          colors = rainbow(12), #numero de cores da paleta. testar colors = brewer.pal(8, "Dark2")
          rot.per = 0.35, # proporcao de palavras em cada eixo(vertical/horizontal)
          use.r.layout = T)



# Para obter uma lista das palavras mais frequentes:
# para ver como barras é preciso transformar em DTM, depois em matriz, depois em data.frame
textsplitWord_dtm <- DocumentTermMatrix(Legendas_corpus)
textsplitWordMatrix <- as.matrix(textsplitWord_dtm)
textsplitWordSorted <- sort(colSums(textsplitWordMatrix), decreasing = T)
textsplitWordDF <- data.frame(word = names(textsplitWordSorted), freq = textsplitWordSorted)
textsplitWordDF[1:30,] # Para saber as 30 mais freqentes.



# para saber o numero de palavras sem repetição
textsplitWord_dtm <- DocumentTermMatrix(Legendas_corpus)
textsplitWordMatrix <- as.matrix(textsplitWord_dtm)
textsplitWordSorted <- sort(colSums(textsplitWordMatrix), decreasing = T)
textsplitWordDF <- data.frame(word = names(textsplitWordSorted), freq = textsplitWordSorted)
NwordsLegendas <- count(textsplitWordDF)  # ___ palavras diferentes



