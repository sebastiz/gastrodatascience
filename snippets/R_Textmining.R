######  Introduction ###### 
#Using the book Text mining with R Jan Wijfiels: jwijfiels@bnosac.be   R_Textmining.pdf

options(width=120)  # make the printing fit on the page
options(scipen=3)
oldwd <- getwd()


mystrings
Encoding(mystrings)

Sys.getlocale("LC_CTYPE")
localeToCharset()

iconvlist()

##### Encoding Detection #################### 
library(stringi)
stri_enc_detect(str = "I want to detect this encoding")
# You can easily convert your string to another encoding using iconv.
# If you know your data is in Latin-1 and want to convert it in UTF-8

mystring <- mystrings[1]
mystring
Encoding(mystring)
x <- iconv(mystring, from = "latin1", to = "UTF-8")
x 
Encoding(x)
#If you convert to another encoding, make sure the character exists

mystring <- mystrings[1:2]
mystring
iconv(mystring, from = "latin1", to = "ASCII", sub = "?")
iconv(mystring, from = "latin1", to = "ASCII")


# If you want to get rid of special characters and want to translate special
# characters to ASCII, you can use translate to ASCII. This will translate
# special characters from another encoding which do not exist in the ASCII
# encoding to the ASCII encoding.You can change the encoding of text as follows allowing you to either
# mess up your text data or set it correctly.

iconv(mystring, from = "latin1", to = "ASCII//TRANSLIT")

Encoding(mystrings)
mystrings
Encoding(mystrings) <- "UTF-8"
mystrings
Encoding(mystrings) <- "latin1"
# When importing data, know the Encoding of your le and you specify it
# I readLines has argument encoding to indicate latin1 or UTF-8
# I read.table has argument encoding to assume latin1 or UTF-8
# Languages can be identied by N-gram proles.
# Function textcat form the textcat package allows this.

library(textcat)
mystrings
textcat(mystrings)
textcat(mystrings, 
        p = TC_char_profiles[c("dutch","english","french","german")],
        method = "CT")

# Methodology explained at www.jstatsoft.org/v52/i06
# I Categorize texts by computing their n-gram proles, and nding the
# closest category n-gram prole.
# I N-gram prole db is constructed from a known corpus for a language
# and top 300 n-grams are taken
# I Next, compute n-gram on your own text
# I Calculate distance to the n-gram prole of the database
# I Default distance method CT: Cavnar and Trenkle approach which
# counts the number of rank inversions between the proles
textcat("Marieke gaat om water", p = textcat::ECIMCI_profiles)
textcat("Marieke gaat om water", p = textcat::TC_char_profiles)
textcat("Marieke gaat om water", p = textcat::TC_byte_profiles)

# Similar approach used by R package cld2 (Google's Compact Language
# Detector)
#                                          I Corpus was constructed based on scraped web pages where they
#                                          knew the language of the web page
#                                          I N-grams are constructed based on the HTML.
#                                          I A naive bayesian classier is constructed based on this data
#                                          I The model is applied to new data
#                                          I Advantage: faster and good if your data is html, disadvantage you
#                                          can not use your own corpus
library(cld2)
x <- c("To be or not to be?", "Ce n'est pas grave.", "Nou breekt mijn klomp!")
detect_language(x)



##### ??????????????? Basic text processing ################################################
# 2 functions that you ought to know already
# I paste: collapsing several strings in 1 or combining several strings
# I sprintf: inject a string into another
library(udpipe)
data("brussels_reviews", package = "udpipe")
brussels_reviews$feedback[1:2]


##### paste and sprintf ##### 
x <- c("abc", "def", "123", "go ahead")
paste(x, collapse = "_")

paste(x, 1:4, sep = "-")

sprintf("INSERT INTO customers VALUES(%s, '%s')", 
        1:2, 
        c("person1", "xyz"))


############## Regular expressions ########################################## 

# Detecting (search): grep
# I searches for matches to argument pattern within each element of a
# character vector.
# I returns the vector elements where it has found the pattern
# argument ignore.case allows you to match exactly (including upper
#                                                   case/lower case)
# I argument invert allows you to get the indexes where the match is
# not found
# I argument value will return the character strings with a match
# grep returns the index of the match in the character vector while grepl
# returns a logical vector of length(x) indicating the match location.
grep(pattern="B", x=c("A", "B", "C", "D"))

x <- c("LStat","lstat","Leuven Statistics Research Center",
       "BNOSAC", "Belgium", "Waffels in Belgium", "Manneken Pis @ Atomium")
grep(pattern="stat", x=x, ignore.case=TRUE)
grep(pattern="stat", x=x, ignore.case=TRUE, invert=TRUE)
# I argument ignore.case allows you to match exactly (including upper
#                                                     case/lower case)
# I argument invert allows you to get the indexes where the match is
# not found
# I argument value will return the character strings with a match
# grep returns the index of the match in the character vector while grepl
grep(pattern="lstat", x=x, ignore.case=TRUE, value=TRUE)
grepl(pattern="lstat", x=x, ignore.case=TRUE)


x <- c("LStat","lstat","Leuven Statistics Research Center",
       "BNOSAC", "Belgium", "Waffels in Belgium", "Manneken Pis @ Atomium")
grep(pattern="^Bel", x=x, value=TRUE)
grep(pattern="stat$", x=x, value=TRUE)
grep(pattern="^Bel|Stat$", x=x, value=TRUE)


grep(pattern="Go{4}l", x=c("Gooool","Goooool"), value=TRUE)
grep(pattern="Go+a", x=c("Gooooaaal","Goooooobbbl"), value=TRUE)
grep(pattern="^GG?o*a", x=c("Gooooaaal","GGooooaaal"), value=TRUE)
grep(pattern="G.*a", x=c("Gooooaaal","Gooaaal"), value=TRUE)
grep(pattern="(Ik haat){2}", x=c("Ik haat smurfen","Ik haatIk haat smurfen"), value=TRUE)


metacharacters = c("$","*","+",".","?","[","^","{","|","(","\\")
grep(pattern=".", "testing", value=TRUE)
grep(pattern="\\.", "testing", value=TRUE)
grep(pattern="\\.", "testing.out", value=TRUE)

input1 <- c("nose", "letter38", "window9", "apple0")
grep("[[:digit:]]", input1, value = TRUE)
grep("[nco]", input1, value = TRUE)
grep("[39]", input1, value = TRUE)

input2 <- c("abcdef", "ABCDEFG", "IJK")
grep("[a-cA-D]", input2, value = TRUE)
grep("[[:lower:]]", input2, value = TRUE)


gsub(pattern="Statistics", replacement="Statistiek", x="Leuven Statistics Research")
gsub(pattern="Sta|Research", replacement="", x="Leuven Statistics Research")
gsub(pattern=" +", replacement=" ", x="Leuven      Statistics")
sub(pattern="\\.",  replacement="", x="abc...def")
gsub(pattern="\\.", replacement="", x="abc...def")

x <- "I want to break free"
gsub(pattern = "(.+)(break)(.+)", replacement = "\\1", x)
gsub(pattern = "(.+)(break)(.+)", replacement = "\\2", x)
gsub(pattern = "(.+)(break)(.+)", replacement = "\\3", x)


gregexpr(pattern="\\.", text="abc...def")
txt <- "Leuven Statistics Research Center"
gregexpr(pattern="Sta|Research",  text=txt)
regexpr(pattern="Sta|Research",  text=txt)

# regexpr and gregexpr give detailed information where the
# matches are found.
# I regexpr gives the starting position of the rst match.
# I gregexpr the starting positions of every (disjoint) match is given.
txt <- "Leuven Statistics Research Center"
regmatches(x=txt, gregexpr(pattern="Sta.+ |Research",  text=txt), invert=FALSE)
regmatches(x=txt, gregexpr(pattern="Sta.+ |Research",  text=txt), invert=TRUE)
regmatches(x=txt, regexpr(pattern="Statis",  text=txt)) <- "Analy"
txt
# The result of a gregexpr can be fed to regmatches in order to extract
# or replace the text

###### strsplit #####
#strsplit splits text data by a character or a regular expression


strsplit(x=c("abc.   def", ""), split=" ")
strsplit(x=c("abc.   def", ""), split="\\.")

##### strtrim #####
#use substr and strtrim to extract element of character data
substr(x=c("abc. def", "123456789"), start=3, stop=4)
strtrim(x=c("abc", "123456789"), width=4)

d1 <- data.frame(id...of....patient = c(1, 2), patient....age = c(1, 2))
d <- data.frame(
  id = c(11, 22, 33, 44, 55, 66, 77), 
  drug = c("vitamin E", "vitamin ESTER-C", "  vitamin E ", "vitamin E(ointment)", "", "provitamin E\n", "vit E"), 
  text = c("", " ", "3 times a day after meal", "once a day", " ", "\t", "\n "), 
  stringsAsFactors = FALSE)

##### String distances #############################################  

# Package stringdist allows to compute distances
# between 2 strings.
# Application domains:
#   I replace strings with another string
# I fuzzy merging
# http://journal.r-project.org/archive/
#   2014-1/loo.pdf
# I edit-based distances: for short string matching
# I q-gram based distances: for very long text
# strings
# I heuristic distances: for matching
# human-typed, relatively short strings
# base package functions: agrep & adist
# I adist: allows to compute the generalized Levenshtein distance
# between strings.
# I agrep: allows to get approximate regulare expressions.
# Levenshtein distances: combination of transformations 1, 2, 3
# 1. Substitution of a character, as in 'belgium' >'belhium'.
# 2. Deletion of a character, as in 'belgium' >'elgium'.
# 3. Insertion of a character, as in 'belgium' >'beltgium'.
# 4. Transposition of two adjacent characters, as in 'belgium' >'beglium'.


wb <- c("Warner Bros Pictures", "Warner Bros.", "Warner Brothers", 
        "Warner Bro. Pictures", "Warners Bros. Pictures",
        "Universal / Warner Bros.")
adist(x = "Warner Bros Pictures", y = wb)
agrep(pattern = "Warner Bros Pictures", x = wb, 
      max.distance = 0.1, value=TRUE)

agrep(pattern = "Warner Bros Pictures", x = wb, 
      costs = list(insertions = 0.1, deletions = 0.5, substitutions = 1), 
      max.distance = 0.1, value=TRUE)

# Always based on UTF-8 encoding.
# I Hamming distance: only character substitutions
# I Levenshtein distance (weighted): counting the weighted number of
# insertions, deletions and substitutions necessary to turn one string
# into another.
# I Restricted Damerau-Levenshtein distance (weighted, a.k.a. Optimal
#                                            String Alignment): extension of the Levenshtein distance that allows
# for transpositions of adjacent characters
# I Longest Common Substring distance: counts the number of deletions
# and insertions necessary to transform one string into another

require(stringdist)
## Get Hamming/Levehnstein distances
stringdist(a = "Warner Bros Pictures", wb, method = "hamming")
stringdist(a = "Warner Bros Pictures", wb, method = "lv")
## Optimal String Alignment and Longest common substring distance
stringdist(a = "Warner Bros Pictures", wb, method = "osa")
stringdist(a = "Warner Bros Pictures", wb, method = "lcs")

#Approximate in operators based on string matching

wb %in% "Warner Bros Pictures"
ain(x=wb, table="Warner Bros Pictures", maxDist = 4, method = "lv")

# A q-gram is a string consisting of q consecutive characters.
# I Jaccard distance for q-gram count vectors (= 1-Jaccard similarity)
# where Jaccard similarity: listing unique q-grams in two strings and
# compare which ones they have in common
# I Q-gram distance: The q-gram distance is obtained by tabulating the
# q-grams occurring in the two strings and taking the absolute value
# of the row differences
qgrams(a = "leilali", b = "leelala", 
       q = 2)
stringdist('leilali', 'leelala', method='jaccard', q = 2) # 1 - ((3/8) in common)
stringdist('leilali', 'leelala', method='qgram', q = 2) # sum of abs(row 1 - row 2)
# Jaro, and Jaro-Winker distance:
#   Used to match name + address data:
#   I character mismatches and transpositions are caused by typing-errors
# I matches between remote characters are unlikely to be caused by a
# typing error
# I measures the number of matching characters between two strings
# that are not too many positions apart
# I adds a penalty for matching characters that are transposed
# I an extra penalty for character mismatches in the rst four characters
# dropped a letter at the end, low penalty
stringdist('Jan Wijffels', 'Jan Wijfel', method='jw')
# low penality on swapping
stringdist('Jan Wijffels', 'Jna Wijffesl', method='jw')
# mistyping higher penalty than swapping
stringdist('Jan Wijffels', 'Jen Wijffels', method='jw')

##### ??????????????? Graphical representation ################ 

# I frequencies
# I wordclouds
# I correlations
# I associations word network plots


#Working on a dataset with movies extracted using the omdbapi package

load("../data/mymovies.RData")
str(mymovies)
# n-grams

#I 1-gram: 1 word/term
# I bi-gram: one word/term in combination with another word
# I tri-gram: three words/terms in a row

oldpar <- par(no.readonly = FALSE)



library(tau)
out <- textcnt(x = mymovies$Plot, tolower = TRUE, 
               method = "string", n = 1, decreasing=TRUE)
barplot(rev(head(out, 20)), col = "lightblue", 
        horiz = TRUE, las = 2, main = "Frequency of terms")


##### Wordcloud #####
# Wordclouds are frequency statistics. Mark that data preparation is key
# (e.g. ltering irrelevant words)
# I scale: Set sizes of the words
# I max.words and min.freq: Limit the number of words plotted.
# I random.order: words with the highest frequency are plotted rst
# I rot.per: Fraction of words that are plotted vertically.
# I colors: color paletteslibrary(wordcloud)
wordcloud(names(out), freq = out, scale=c(10, .75), min.freq = 10, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6,"Dark2"))

##### Correlations and association plots #####

# These kinds of plots require a document-term matrix which needs some
# data preparation.


library(tm)
library(NLP)
mymovies$txt <- tolower(mymovies$tomatoConsensus)
mycorpus <- Corpus(x = VectorSource(mymovies$txt))  
mydtm <- DocumentTermMatrix(mycorpus, 
                            control = list(tokenize = "words"))
mydtm

## show some rows
as.matrix(mydtm[1:3, c("and", "the", "movie")])

# Mark: you can also get that document/term/matrix with other packages
# like udpipe, tidytext, quanteda

mymovies$doc_id <- 1:nrow(mymovies)

## Use the udpipe package to create a document/term/matrix
library(udpipe)
dtm <- document_term_frequencies(x = mymovies$txt, document = mymovies$doc_id, split = "[[:space:][:punct:]]+")
dtm <- document_term_matrix(dtm)

## Use the tidytext package to create a document/term/matrix
library(tidytext)
dtm <- unnest_tokens(mymovies, output = "term", input = "txt", token = "words")
dtm <- count(dtm, doc_id, term, sort = TRUE)
dtm <- ungroup(dtm)
dtm <- cast_dtm(dtm, document = doc_id, term = term, value = n)

## Use the quanteda package to create a document/term/matrix
library(quanteda)
dtm <- mymovies$txt
names(dtm) <- mymovies$doc_id
dtm <- dfm(dtm, what = "word")

#Correlation plots
library(corrplot)
interestedin <- c("action", "may", "enough", "special", "movie", "humor", 
               "effects", "will", "fans", "best", "one")
m <- cor(as.matrix(mydtm[, interestedin]), 
         method = "pearson", use = "pairwise.complete.obs")
corrplot(m, method="circle", type="lower", diag=TRUE, order = "original", 
         addCoef.col="black", 
         main = "\n\nCorrelation between words")

interestedin <- c("action", "may", "enough", "special", "movie", "humor", 
  "effects", "will", "fans", "best", "one", "boasts", "cast", "make", "funny", "story", "talented")
plot(mydtm, terms = interestedin, 
     corThreshold = 0.075, weighting = TRUE,
     attrs = list(node = list(shape = "circle", fontsize = 15), 
                  edge = list(color = "steelblue")))

par(oldpar)

# Calculate word co-occurences in each sentence (using cooccurrence
#                                                from the udpipe package)
# I How many times does each word occur in the same sentence with
# another word
library(udpipe)
load("../data/brussels_reviews_pos.RData")
x <- subset(brussels_reviews_pos, language %in% "french" & word.type %in% c("NN"))
head(x[, c("id", "sentence.id", "language", "word.lemma")], 3)
word_cooccurences <- cooccurrence(x, term="word.lemma", group=c("id", "sentence.id"))
head(word_cooccurences)

##### Network co-occurrences #####

# Visualise the word co-occurences as a graphlibrary(magrittr) #

library(ggraph)
library(igraph)
set.seed(123456789)
head(word_cooccurences, 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link0(aes(edge_alpha = cooc, edge_width = cooc)) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8, col = "darkgreen") +
  ggtitle("Co-occurrence of nouns") +
  theme_void()




########### Stemming ################################# 
# Stemming: reducing in
# ected (or sometimes derived) words to their word
# stem, base or root form-generally a written word form
# Snowball:
#   Snowball stemmer allows stemming of languages: danish, dutch, english, nnish,
# french, german, hungarian, italian, norwegian, porter, portuguese, romanian, russian, spanish,
# swedish, turkish
# Procedure for each language is explained in
# http://snowball.tartarus.org

library(SnowballC)
wordStem(c("winner", "winners", "winning", "won"), language = "english")
stemSentence <- function(x, language){
  x <- strsplit(x, "[[:blank:]]")[[1]]
  x <- wordStem(x, language)
  paste(x, collapse = " ")
}
mymovies$Plot[1]
stemSentence(mymovies$Plot[1], language = "english")

##### ??????????????? POS tagging ######## 

# POS tagging: also called grammatical tagging or word-category
# disambiguation
# I is the process of marking up a word in a text (corpus) as
# corresponding to a particular part of speech
# I based on both its denition, as well as its context-i.e. relationship
# with adjacent and related words in a phrase, sentence, or paragraph.
# Categories: noun, verb, article, adjective, preposition, pronoun, adverb,
# conjunction and interjection.
# Modelling: tagging is done based on a corpus and a learner (Hidden
#                                                             Markov models, Viterbi algorithm, Brill Tagger, the Baum-Welch
#                                                             forward-backward algorithm, SVM, Perceptron, Nearest Neighbour,
#                                                             Maximum Entropy)

# Possibility 1: UDPipe: Language-agnostic tokenisation, lemmatisation,
# POS tagging, dependency parsing
# I For more than 50 languages
# I Universal POS tags categories as dened at
# http://universaldependencies.org/u/pos/index.html


library(udpipe)
ud_model <- udpipe_download_model(language = "dutch")
ud_model <- udpipe_load_model(ud_model$file_model)
txt <- c("Ik ga op reis en ik neem mee, een tandenborstel, boeken en mijn goed humeur.",
         "Ik ga niet op reis. Mijn geld is op.")
x <- udpipe_annotate(ud_model, txt)
x <- as.data.frame(x)
str(x)

# UDPipe logic
# 1. Tokenisation: uses a deep learning model (GRU) to predict for each
# token if it is the last one in a sentence, the last one of a token, not
# the last one
# 2. POS/UPOS/Morphological Features tagging: guesser makes possible
# combinations of which one is chosen based on simple neural network
# 3. Details: http://dx.doi.org/10.18653/v1/K17-3009
# For a denition of the POS tags:library(NLP)
help(Universal_POS_tags, package = "NLP")
Universal_POS_tags
Penn_Treebank_POS_tags


# Possibility 2: openNLPmodels.en/da/de/es/nl/pt/se
# I A series of models written in Apache OpenNLP
# (http://opennlp.sourceforge.net/models-1.5)
# I available as R packages at https://datacube.wu.ac.at
# install.packages(
#   openNLPmodels.en
#   , repos = "http://datacube.wu.ac.at", type = "source")
# I Gives POS tags based on the treebanks tags
# (English: https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html)

library(NLP)
library(openNLP)
library(openNLPmodels.en)
## Make sentence/word/POS annotators
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
pos_annotator <- Maxent_POS_Tag_Annotator(language = "en")
## Annotate the text
s <- as.String(mymovies$Plot[1])
a1 <- NLP::annotate(s, sent_token_annotator)
a2 <- NLP::annotate(s, word_token_annotator, a=a1)
a3 <- NLP::annotate(s, pos_annotator, a=a2)
a3


#Get the type of POS as a data.frame
library(data.table)
taggedwords <- subset(a3, type == "word")
tags <- data.frame(term = s[taggedwords],
                   pos.tag = sapply(taggedwords$features, FUN=function(x) head(x$POS, 1)),
                   stringsAsFactors=FALSE)
head(tags)

##### ??????????????? Lemmatization #############################################
# Lemmatisation in linguistics is the process of grouping together the
# different inected forms of a word so they can be analysed as a single
# item
# Closely related to stemming. Stemmer operates on a single word without
# knowledge of the context, and therefore cannot discriminate between
# words which have different meanings depending on part of speech.
# https://en.wikipedia.org/wiki/Lemmatisation

# Possibility 1: UDPipe (MPL license) Logic: generates possible in
# ections (lemma rules + UPOS) based
# on last 4 letters and the word prex and lets a simple neural network
# decide on which is the best based on the training data
# I How: run udpipe annotate
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = mymovies$Plot[1:5])
x <- as.data.frame(x)
x[, c("token_id", "token", "lemma", "upos", "xpos", "dep_rel", "feats")]


# Possibility 2: Pattern (BSD license): Other option for Parts of Speech
# tagging + lemmatisation: use R package pattern.nlp
# I https://github.com/bnosac/pattern.nlp
# I POS tagging for Dutch, French, English, German, Spanish, Italian
# according to Penn Treebank
# I POS tagger is Brill tagger (classication tree)
# I Sentiment analysis for Dutch, French, English
library(pattern.nlp)
txt <- "Dus godvermehoeren met pus in alle puisten, 
 zei die schele van Van Bukburg en hij had nog gelijk ook.
 Er was toen dat liedje van tietenkonttieten kont tieten kontkontkont,
 maar dat hoefden we geenseens niet te zingen"
pattern_pos(x = txt, language = 'dutch', core = TRUE)



##### ??????????????? Dependency parsing ######################################## 

# Dependency is the notion that linguistic units, e.g. words, are connected
# to each other by directed links which have a specic relation.
# I For example in the sentence 'The economy is weak but the outlook
# is bright', the term 'economy' is related to the term 'weak' as
# economy is the subject of weak.
# I Every word in a sentence is linked to a head word with a certain
# relationship. This is called dependency parsing.
# I Type of relationships are dened at
# http://universaldependencies.org/u/dep/index.html
# I Use cases: question/answering, semantic labelling, chatbots.
library(udpipe)
x <- udpipe_annotate(ud_model, "The economy is weak but the outlook is bright")
x <- as.data.frame(x)
x[, c("token_id", "token", "head_token_id", "dep_rel")]

# If you want to visualise these dependencies in R, you can do as follows.
library(igraph)
edges <- subset(x, head_token_id != 0, select = c("token_id", "head_token_id", "dep_rel"))
edges$label <- edges$dep_rel
g <- graph_from_data_frame(edges,
                           vertices = x[, c("token_id", "token", "lemma", "upos")],
                           directed = TRUE)
plot(g, vertex.label = x$token)

##### ??????????????? Text summarisation #####

# If you have annotated text with POS tags, basic keyword detection
# algorithms are
# I Cooccurrences
# I RAKE (Rapid Automatic Keyword Extraction)
# I Parts of Speech phrase sequence detection
# I Textrank (Google Pagerank based on text)
# I Collocations (ordering based Pointwise Mutual Information)
# Examples shown below on annotated client feedback on movie. Keyword
# detection is part of the udpipe R package.
# 

load("../data/mymovies_consensus.RData")
head(mymovies_consensus[, c("doc_id", "sentence_id", "token", "lemma", "upos")])



#Co-occurrence analysis

# Co-occurrences show how many times word occur together
# I Within a sentence
# I Next to each other
# I Within a range of skipgram words
library(udpipe)
## Nouns in the same sentence
cooc <- cooccurrence(subset(mymovies_consensus, upos %in% "NOUN"),
                     group = c("doc_id", "sentence_id"), term = "lemma")

## Nouns/adverbs next to each other
cooc <- cooccurrence(mymovies_consensus$lemma, 
                     relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"))

## Nouns/adverbs next to each other even if we would skip 2 words
cooc <- cooccurrence(mymovies_consensus$lemma, 
                     relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"), 
                     skipgram = 2)

##skipgram allows you to see the co-occurrence between words which are commonly separated from each other
# RAKE:
#   I keywords are dened as a sequence of words following one another
# I among the words of the candidate keywords the algorithm looks
# I how many times each word is occurring
# I and how many times it co-occurs with other words
# I each word gets a score which is the ratio of the word degree (how
#                                                                 many times it co-occurs with other words) to the word frequency
# I a RAKE score for the full candidate keyword is calculated by
# summing up the
head(cooc)

stats <- keywords_rake(x = mymovies_consensus,
                       term = "lemma", group = c("xpos", "upos"),
                       relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"))
head(subset(stats, freq > 5))

# Phrases: To detect phrases (a sequence of POS tags) one can use
# keywords phrases.
# This merely looks based on a supplied regular expression to a sequence of
# POS tags. Allows to detect
# I Noun phrases
# I Verb phrases
mymovies_consensus$phrase_tag <- as_phrasemachine(mymovies_consensus$upos, type = "upos")
stats <- keywords_phrases(x = mymovies_consensus$phrase_tag,
                          term = mymovies_consensus$token, 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)
head(subset(stats, freq < 100 & ngram > 1))


# Textrank:
#   I the textrank algorithm constructs a word network.
# I A link is set up between two words if they follow one another
# I the link gets a higher weight if these 2 words occur more frequenctly
# next to each other in the text.
# I On top of the network apply Google's Pagerank to get the
# importance of each word.
# I The top 1/3 of all these words are kept and are considered relevant.
# I After this, a keywords table is constructed by combining the relevant
# words together if they appear following one another in the text.
library(textrank)
stats <- textrank_keywords(mymovies_consensus$token,
                           relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"))
stats <- subset(stats$keywords, freq > 5 & ngram > 1)
wordcloud(words = stats$keyword, freq = stats$freq, max.words = 15)


# Collocations
#   Theorem
# Collocations is a sequence of words or terms that co-occur more often
# than would be expected by chance
# https://en.wikipedia.org/wiki/Collocation
# Common measures of collocation are:
#   I PMI (pointwise mutual information): I(w1;w2) = log2( P(w1w2)
#                                                          P(w1)(w2) )
# where P(w) is the probability of a word
# I MD (mutual dependency): MD(w1;w2) == log2( P2(w1w2)
#                                              P(w1)(w2) )
# I LFMD (log-frequency biased mutual dependency):
#   DLF(w1;w2) = D(w1;w2) + log2P(w1w2)
# In R this can be accomplished with the udpipe R package or with the
# text2vec R package

stats <- keywords_collocation(x = mymovies_consensus, 
                              term = "lemma", group = c("doc_id", "sentence_id"))
head(subset(stats, freq > 5))

##  Textrank can also be used to summarise sentences:
#   I A graph is constructed where
# I the vertices of the graph represent each sentence in a document
# I and the edges between sentences are based on content overlap, by
# calculating the number of words that 2 sentences have in common
# I Use Google Pagerank to identify the most important sentences.
# I When we want to extract a summary of the text, we can now take
# only the most important sentences.

data(joboffer)
sentences <- unique(mymovies_consensus[, c("sentence_id", "sentence")])
terminology <- subset(joboffer, upos %in% c("NOUN", "ADJ"))
stats <- textrank_sentences(data = sentences,
                            terminology = terminology[, c("sentence_id", "lemma")])
stats

################### ???????????????  Entity recognition ########################################## 

# Named-entity recognition (NER) (also known as entity identication,
#                                 entity chunking and entity extraction) is a subtask of information
# extraction that seeks to locate and classify elements in text into
# pre-dened categories such as the names of persons, organizations,
# locations, expressions of times, quantities, monetary values, percentages,
# etc
# Can be done with the openNLP packages.
library(NLP)
library(openNLP)
library(openNLPmodels.en)


######### Named Entity Recognition ############

# Theorem
# Named-entity recognition (NER) (also known as entity identifcation,
#                                 entity chunking and entity extraction) is a subtask of information
# extraction that seeks to locate and classify elements in text into
# pre-defined categories such as the names of persons, organizations,
# locations, expressions of times, quantities, monetary values, percentages,
# etc
# Can be done with the openNLP packages


s <- as.String(mymovies$Plot[1])

## Need sentence and word token annotations before finding persons
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
personannotator <- Maxent_Entity_Annotator(language = "en", kind = "person", probs = FALSE)
a1 <- NLP::annotate(s, sent_token_annotator)
a2 <- NLP::annotate(s, word_token_annotator, a=a1)
a3 <- NLP::annotate(s, personannotator, a=a2)
a3

tags <- data.frame(term = s[a3],
                   is.person = sapply(a3$features, FUN=function(x){
                    "kind" %in% names(x) && x$kind %in% "person"
                   }),
                   stringsAsFactors=FALSE)
subset(tags, is.person == TRUE)

################### ???????????????  Sentiment analysis: ########################

# Sentiment analysis: identify and extract subjective
# information in source materials
# In it's basic form, this comes down to word
# frequencies and assigning a value to a word.
# Reference: www.cs.uic.edu/~liub/FBS/
# SentimentAnalysis-and-OpinionMining.pdf
# Generally 2 approaches:
# 1. supervised: predictive modelling where
# sentences were annotated
# 2. using a lexicon-based sentiment dictionary

library(qdap)
data(key.pol)
head(key.pol, 3)
table(key.pol$y)

# OpenNER lexicons: https://github.com/opener-project/public-sentiment-lexicons
# Sentometrics: https://github.com/sborms/sentometrics/tree/master/data-raw
# MPQA Subjectivity Lexicon: http://mpqa.cs.pitt.edu/lexicons/subj_lexicon -
#   GNU licensed
# SentiWordNet: http://sentiwordnet.isti.cnr.it - Non-commercial

# Computes sentiment scores between -1 and 1 by
# 1. identifying the words from a dictionary with
# positive/negative/ampliers/de-amplier terms
# 2. polarised words are weighted by the -1/+1.
# 3. If a valence shifter occurs in the neighbourhood, the word is
# weighted extra with the shifters
# 4. Summed up and divided by
# p
# wordcount
# 5. Constraint to put it into a -1/+1 scale:
#   ((1 􀀀 (1=(1 + exp(polarity))))  2) 􀀀 1



#Compute the sentiment score on a [􀀀1; 1] range.
oldpar <- par(no.readonly = FALSE)

mymovies2<-head(mymovies,100)

txt <- tolower(mymovies2$tomatoConsensus)
sentiments <- polarity(txt,
                       polarity.frame = qdapDictionaries::key.pol,
                       negators = qdapDictionaries::negation.words, n.before = 4, n.after = 2,
                       amplifiers = qdapDictionaries::amplification.words,
                       deamplifiers = qdapDictionaries::deamplification.words, amplifier.weight = 0.8,
                     constrain = TRUE)

## Add a column called sentiment to the data.frame
mymovies2$sentiment <- sentiments$all$polarity

## General output
sentiments$group

## Structure of the detailed output
str(sentiments$all)

# Distribution of the sentiment - standard a lot of zero's.
# Is the sentiment linked to the popularity - yes.
library(corrplot)
library(MASS)
par(mfrow = c(1, 2))
truehist(mymovies$sentiment, col = "lightblue", main = "Sentiment distribution")
m <- cor(mymovies[c("sentiment", "tomatoMeter", "tomatoRating", "imdbRating", "imdbVotes")],
    method = "pearson", use = "pairwise.complete.obs")
corrplot(m, method="number", main = "\n\nCorrelation sentiment vs movie metrics")

idx <- sample(which(sentiments$all$polarity != 0), 10)
sentiments$all[idx, c("all", "wc", "polarity", "pos.words", "neg.words")]

par(oldpar)
set.seed(123456789)

idx <- sample(which(sentiments$all$polarity != 0), 10)
sentiments$all[idx, c("all", "wc", "polarity", "pos.words", "neg.words")]

# For reference: other less advanced options for sentiment analysis with R
# 1. R package tidytext (just use a lookup dictionary, without valence
#                        shifters (ampliers/negators))
# 2. R package sentimentr https://github.com/trinker/sentimentr
# 3. R package pattern.nlp https://github.com/bnosac/pattern.nlp

####### To############ ??????????????? pic detection ########

# Objective: identify topics (combinations of words
#                             used in the text) which form a business-relevant
# topic.
# Models:
#   1. Latent Dirichlet Allocation (LDA) (package
#                                         topicmodels)
# 2. Correlated Topics Model (package
#                             topicmodels)
# 3. Other options:
#   I Non-negative matrix factorisation (package
#                                        NMF)
# I Other options: Structural Topic Model
# (package stm)
# Part of family of mixed-membership topic models where:
#   I documents are not assumed to belong to single topics, but to
# simultaneously belong to several topics
# I the topic distributions vary over documents.
# Starts with bag-of-words matrix of document X words/terms
# generative model/process for a document is dened as follows:
#   set the number of topics k as a xed a-priori
# Step 1: A topic emits words/terms ( is the term distribution of topics and
#                                      contains the probability of a word occurring in a given topic). The
# term distribution  is determined for each topic by
#  Dirichlet():
#   Step 2: To which topic does a document belong is being set by . The
# proportions  of the topic distribution for the document w are
# determined by
#   Dirichlet():
#   Step 3: For each of the N words wi
# Step 1:.1 Choose a topic zi  Multinomial().
# Step 2:.2 Choose a word wi from a multinomial probability distribution
# conditioned on the topic zi: p(wijzi; ).
# Common data preparation is
# I remove irrelevant terms (numbers, punctuation, stopwords, . . . )
# I focus on nouns or only adjectives
# I only positive/negative sentiments
# I only terms with enough frequency
# I term frequency inverse document frequency
# Where
# I tf = how many times does term appear in the document
# I idf = inverse document frequency = log(number of documents /
#                                            number of documents containing the term)
# The tf-idf value increases proportionally to the number of times a
# word appears in the document, but is oset by the frequency of the
# word in the corpus, which helps to adjust for the fact that some
# words appear more frequently in general.
# Data preparation for the document/term matrix

library(topicmodels)
library(udpipe)
library(tm)
library(slam)
txt <- tolower(mymovies$tomatoConsensus)
txt <- removeWords(txt, words = stopwords("english"))
mycorpus <- Corpus(x = VectorSource(txt))  
mycorpus <- tm_map(mycorpus, FUN=function(x) stripWhitespace(x))
## Build Document Term matrix
mydtm <- DocumentTermMatrix(mycorpus, 
                            control = list(wordLengths = c(2, Inf), 
                                           bounds = list(global = c(5, Inf))))
## tfidf
term_tfidf <- tapply(mydtm$v/row_sums(mydtm)[mydtm$i], mydtm$j, mean) * 
  log2(nrow(mydtm)/col_sums(mydtm > 0))
mydtm <- mydtm[, term_tfidf >= quantile(term_tfidf, 0.5)]
## Work only on the top 500 words if more words are occuring
termfreq <- col_sums(mydtm)
termfreq <- sort(termfreq, decreasing = TRUE)
termfreq <- head(termfreq, 500)
mydtm <- mydtm[, intersect(colnames(mydtm), names(termfreq))]
mydtm <- mydtm[, col_sums(mydtm) > 5]
mydtm <- mydtm[row_sums(mydtm) > 0, ]
dim(mydtm)

# Important parameter in the t is  which indicates if a document can
# belong to 1 topic or several ones.
# Output of LDA contains
# I 
# gamma is the posterior topic distribution of each document
# I beta is the log of the word distribution of each topic

set.seed(123456789)
m <- LDA(x = mydtm, k = 7, method = "VEM",
         control = list(alpha = 0.1))
m
slotNames(m)


# Posterior probability document belongs to topic

x <- posterior(m, newdata = mydtm)$topics
x[1:2, ]


#Words emitted by each topic

x <- posterior(m, newdata = mydtm)$terms
apply(x, MARGIN=1, FUN=function(x, top = 5){
  idx <- order(x, decreasing = TRUE)
  out <- data.frame(term = names(x)[idx], prob = x[idx])
  head(out, n = top)
})

# Evaluation of model fit + Visualisation of each topic


logLik(m)
topicmodels::perplexity(m)
terms(m, 5)

#Interactive visualisation of word emmittance.

library(LDAvis)
library(servr)
json <- createJSON(phi = posterior(m)$terms,
                   theta = posterior(m)$topics,
                   doc.length = row_sums(mydtm),
                   vocab = colnames(mydtm),
                   term.frequency = col_sums(mydtm))
serVis(json)



library(tm)
library(slam)
x <- subset(mymovies, !is.na(BoxOffice))
x$txt <- tolower(paste(x$Plot, x$tomatoConsensus, sep=" "))
xcorpus <- Corpus(VectorSource(x$txt))
mydtm <- DocumentTermMatrix(xcorpus) 
dim(mydtm)
mydtm <- mydtm[, col_sums(mydtm) > 5]
dim(mydtm)

##### Pr############## ??????????????? edictive modelling with text #####

# raw txt
# 2. data cleaning &
#   NLP
# 3. create DTM
# (document-term)
# matrix
# 4. reduce data
# 5. further modelling
# Reduction: with
# penalised regression
# Linear models estimated with least squares minimise the sum of the
# squares of the error of the t. Namely:
#   RSS =
#   Pn
# i=1(yi 􀀀 ^ yi)2
# RSS =
#   Pn
# i=1
# 
# yi 􀀀 0 􀀀
# Pp
# j=1 jxij
# !2
# Idea: add a penalty term called  to each  term
# 1. Ridge regression: penalty on the squared  values
# 2. Lasso (least absolute shrinkage): penalty on the absolute  values
# 3. Elastic net: penalty on both the squared  values and the absolute
# values

# Elastic Net:
#   Put a penalty on both the squared  values and the absolute  values
# Minimise
# Pn
# i=1
# 
# yi 􀀀0 􀀀
# Pp
# j=1 jxij
# !2
# +
# Pp
# j=1[ 1
#      2 (1􀀀)2
#      j +jj j]
# I  is the penalisation parameter
# I  is a mixing parameter between lasso and ridge.
# I The elastic net with  = 1 􀀀  for some small  > 0 performs much
# like the lasso, but removes any degeneracies and wild behavior
# caused by extreme correlations.

# In LARS/Ridge/Best subset selection.
# Minimising subject to a certain budget s for the  values.
# Best subset: no more than s coecients can be non-zero. But
# computationally infeasible
# 􀀀p
# s

# Lasso closer to best subset as it puts constraints on total  values.
# minimize
# 
# Pn
# i=1
# 
# yi 􀀀 0 􀀀
# Pp
# j=1 jxij
# !2
# subject to Ridge :
#   Pp
# j=1 2
# j  s
# Lasso :
#   Pp
# j=1 jj j  s
# Bestsubset :
#   Pp
# j=1 I(j 6= 0)  s
# Jan Wijels:
# Put a penalty on the absolute  values. This is called to the L1 norm.
# Minimise
# Pn
# i=1
# 
# yi 􀀀 0 􀀀
# Pp
# j=1 jxij
# !2
# + 
# Pp
# j=1 jj j
# Lasso tends to pick 1 of correlated variables.
# Lasso better in setting where relatively small nr of predictors with large
# coecients.
# Ridge regression better if many predictors with similar eects.
# Lasso: variable reduction. Ridge: shrinkage.
# Fit a penalised regression model

library(glmnet)
library(caret)
#hist(x$BoxOffice, col = "lightblue", breaks = 30)
mymodel <- cv.glmnet(y = log(x$BoxOffice), 
                     x = as.matrix(mydtm), 
                     family = "gaussian", nfolds = 10, alpha = 1)
plot(mymodel)

## Extract relevant predictive words
relevant <- predict(mymodel, s="lambda.min", type = "coefficients")[, 1]
relevant <- relevant[relevant != 0]
relevant <- sort(relevant)
relevant <- relevant[setdiff(names(relevant), "(Intercept)")]
## Plot relevant predictive words
par(las = 2,  mar=c(5, 7, 4, 2) + 0.1)
barplot(relevant, horiz=TRUE, cex.names = 0.75, col = "lightblue",
        main = "Regression coefficients at optimal lambda")
##Predict or benchmark new movie plots
## Use the model to predict based on an existing Document/Term matrix 
x$boxoffice.estimate <- predict(mymodel, newx = as.matrix(mydtm), 
                              s="lambda.min", type = "response")
hist(x$boxoffice.estimate, breaks = 30, col = "lightblue", xlab = "Predicted box office", 
         main = "Distribution of predicted log(box office)")
postResample(x$boxoffice.estimate, log(x$BoxOffice))


##### Text similarit############## ??????????????? y #####

# Types of analysis:
#   1. Plagiarism: Identify fraudulent (as in copy-pasted) thesis / papers
# 2. Align text where they are overlapping (like in DNA sequence
#                                           alignment)
# 3. Code comparison of your own R code to other R code
# 4. Duplicate document detection (emails, web pages) in corporate
# environments
# 5. How much do CV's match with Job descriptions
# Jaccard similarity of sets S and T is: jS \ Tj=jS [ Tj
# I You can also see how many of the words are in one text versus the
# other (ratio of matches)
# I The Jaccard bag similarity looks how many of the total number of
#  elements in S and T are overlapping.


library(textreuse)
jaccard_similarity(1:6, 3:10) # 3, 4, 5, 6 are overlapping of all 10 elements
ratio_of_matches(1:6, 3:10) # items in right which are also in left 3,4,5,6 out of 3,4,5,6,7,8,9,10
jaccard_bag_similarity(1:6, 3:10) # 4/14 elements are overlapping

textreuse:::jaccard_similarity.default
textreuse:::ratio_of_matches.default
textreuse:::jaccard_bag_similarity.default


align_local("The answer is blowin' in the wind.",
            "As the Bob Dylan song says, the answer is blowing in the wind.",
            match = 2, mismatch = -1, gap = -1,
            edit_mark = "#")


gpl2 <- readLines(url("https://www.gnu.org/licenses/gpl-2.0.txt"))
gpl3 <- readLines(url("https://www.gnu.org/licenses/gpl-3.0.txt"))
gpl2 <- paste(gpl2, collapse = " ")
gpl3 <- paste(gpl3, collapse = " ")


invisible(load("../data/mymovies.RData"))

library(text2vec)
tokens <- mymovies$tomatoConsensus %>% tolower %>% word_tokenizer
vocabulary <- create_vocabulary(it = itoken(tokens))
vocabulary <- prune_vocabulary(vocabulary, term_count_min = 8)
vectorizer <- vocab_vectorizer(vocabulary = vocabulary)
tcm <- create_tcm(it = itoken(tokens), vectorizer = vectorizer, skip_grams_window = 5)

dim(tcm)
class(tcm)

rownames(tcm)[1:5]
colnames(tcm)[1:5]

myglovemodel <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocabulary, 
                                  x_max = 10, alpha = 0.75)
myglovemodel$fit_transform(tcm, n_iter = 40)

wordvectors <- myglovemodel$components
dim(wordvectors)
head(wordvectors[, 1:5])


## Visualise evolution of cost fit over iterations
plot(myglovemodel$get_history()$cost_history)


wordvectors <- t(wordvectors)
cos_sim <- sim2(x = wordvectors, y = wordvectors["remake", , drop = FALSE], 
               method = "cosine", norm = "l2")
head(sort(cos_sim[, 1], decreasing = TRUE), 10)


cos_sim <- sim2(x = wordvectors, 
                y = wordvectors["action", , drop = FALSE] + 
                    wordvectors["zombie", , drop = FALSE], 
               method = "cosine", norm = "l2")
head(sort(cos_sim[, 1], decreasing = TRUE), 10)


