
# Normalizacao de Palavras

# Esse e um script para trazer a discussão sobre stemming s lemmatizing




##### D1. FUNCAO QUE CRIA UMA LISTA DAS FREQUENCIAS DE PALAVRAS. ----

Legendas_vec <- pull(Legendas_, text)   # Converte a coluna "text" de tibble para vetor

texto_ <- paste(Legendas_vec, sep = " ", collapse = " ") # retorna texto em string unico

Nwords_Legendas <- str_count(Legendas_vec, boundary("word")) # retorna numero de palavras por linha
TotPalavras <- sum(Nwords_Legendas)     # retorna qntdd total de palavras


## D2. Limpando: TRES processos possiveis: (1) usando Stemming ; (2) usando lemmatization com humspell (3) lemmatization con Lexicon ; (4) há ainda o wordnet e o (5) SnowballC

# (1) Stemming
Legendas_corpus <- Corpus(VectorSource(Legendas_vec))   # transformando vetor em corpus

Legendas_corpus <- tm_map(Legendas_corpus, removeNumbers)
Legendas_corpus <- tm_map(Legendas_corpus, removeWords, stopwords("english")) 
Legendas_corpus <- tm_map(Legendas_corpus, stemDocument, language = "english")
Legendas_vecClean <- Legendas_corpus$content # retornando de corpus para vetor
# contando
Nwords_Legendas_vecClean <- str_count(legendas_vecClean, boundary("word")) # numero de palavras por linha
TotPalavras_Legendas_vecClean <- sum(Nwords_Legendas_vecClean)     # qntdd total de palavras
# Para obter uma lista das palavras mais frequentes:
# para ver como barras é preciso transformar em DTM, depois em matriz, depois em data.frame
textsplitWord_dtm <- DocumentTermMatrix(Legendas_corpus)
textsplitWordMatrix <- as.matrix(textsplitWord_dtm)
textsplitWordSorted <- sort(colSums(textsplitWordMatrix), decreasing = T)
textsplitWordDF <- data.frame(word = names(textsplitWordSorted), freq = textsplitWordSorted) # Lista ordenada de palavras mais frequentes
# contando 
textsplitWordDF[1:30,] # Para saber as 30 mais freqentes.
NwordsLegendas <- count(textsplitWordDF) # numero de palavras diferentes
textsplitWordDF_filter <- subset(textsplitWordDF, textsplitWordDF$freq > 1)  # DF com palavras que aparecem mais de uma vez


# (2) Lemmatization  -  usando humspell Apesar de a funcao ser chamada de stemming é na pratica uma lemmatization
if(require(hunspell) == F) install.packages("hunspell"); require(hunspell)
words <- hunspell_parse(texto_, format = c("text", "man", "latex", "html", "xml"),  
                        dict = dictionary("en_US")) # criando lista de palavras do texto
words <- as.vector(words[[1]]) # transformando lista em vetor
words_lemm <- hunspell_stem(words) # apesar de ele chamar de stemming, na verdade é um lemmmatization
# O problema dessa funcao é que para algumas palavras ele retorna duas ....
#### PROBLEMA 

Legendas_corpus <- Corpus(VectorSource(Legendas_vec))   # transformando vetor em corpus

Legendas_corpus <- tm_map(Legendas_corpus, removeNumbers)
Legendas_corpus <- tm_map(Legendas_corpus, removeWords, stopwords("english")) 
Legendas_vecClean <- Legendas_corpus$content # retornando de corpus para vetor



Nwords_Legendas_vecClean <- str_count(legendas_vecClean, boundary("word")) # numero de palavras por linha
TotPalavras_Legendas_vecClean <- sum(Nwords_Legendas_vecClean)     # qntdd total de palavras


# Para obter uma lista das palavras mais frequentes:
# para ver como barras é preciso transformar em DTM, depois em matriz, depois em data.frame
textsplitWord_dtm <- DocumentTermMatrix(Legendas_corpus)
textsplitWordMatrix <- as.matrix(textsplitWord_dtm)
textsplitWordSorted <- sort(colSums(textsplitWordMatrix), decreasing = T)
textsplitWordDF <- data.frame(word = names(textsplitWordSorted), freq = textsplitWordSorted) # Lista ordenada de palavras mais frequentes

textsplitWordDF[1:30,] # Para saber as 30 mais freqentes.

NwordsLegendas <- count(textsplitWordDF) # numero de palavras diferentes

textsplitWordDF_filter <- subset(textsplitWordDF, textsplitWordDF$freq > 1)  # DF com palavras que aparecem mais de uma vez


# (3) Lemmatization usando o dicionario de lemmas do lexicon

if(require(lexicon) == F) install.packages("lexicon"); require(lexicon)

data(hash_lemmas)

######################



# hunspell
if(require(hunspell) == F) install.packages("hunspell"); require(hunspell)

text <- c("Trump called relaxing the restrictions his biggest decision as federal projections warn of a possible infection spike", 
          "As new federal projections warned of a spike in coronavirus infections if shelter-in-place orders were lifted after only 30 days, 
          President Trump said Friday that the question of when to relax federal social distancing guidelines was the biggest decision I'll ever make")


hunspell_suggest(text, dict = dictionary("en_US")) # cria lista de sugestoes para cada elemento do vetor

hunspell_parse(text, format = c("text", "man", "latex", "html", "xml"),
               dict = dictionary("en_US")) # retorna listas (de palavras "tokenizadas/parsed") dentro de listas (para cada elemento do vetor). Otimo para combinar com o hunspell_check

hunspell_check(     # Retorna T/F para a existencia (no dicionario) das palavras de um vetor 
  c("projections", "infections", "restrictions"),    # inserir vetor de palavras
  dict = dictionary("en_US"))  # lista de dicionarios => list_dictionaries() # "en_AU" "en_CA" "en_GB" "en_US"

hunspell(text) # cria lista com palavras erradas/nao detectadas # OTIMO PARA ACHAR ERROS DE ORTOGRAFIA! -> É a combinação de parse + check



hunspell_stem(text)


# lexicon - Tem que criar uma funcao para fazer o match e retornar a palavra da segunda coluna
if(require(lexicon) == F) install.packages("lexicon"); require(lexicon)

data(hash_lemmas)
lemmas <- hash_lemmas
lemmas[e,1]

length(lemmas)
dim(lemmas)[1]



# SnowballC # Usa o Porter ou o porter2, não sei.

if(require(SnowballC) == F) install.packages("SnowballC"); require(SnowballC)




# Krovetz stemmer 
# SObre stemming e lemmatizacao no stack : 
# https://stackoverflow.com/questions/24647400/what-is-the-best-stemming-method-in-python
#
# Personally I like the Krovetz stemmer which is a hybrid solution, 
# combing a dictionary lemmatizer and a light weight stemmer for out of vocabulary words. 
# Krovetz also "kstem" or "light_stemmer" option in Elasticsearch. 
# There is a python implementation on pypi 
# https://pypi.org/project/KrovetzStemmer/, though that is not the one that I have used.
# 








