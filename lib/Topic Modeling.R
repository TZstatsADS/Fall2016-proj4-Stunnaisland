library(NLP)
library(tm)
library(Ida)
library(LDAvis)
library(servr)

word.list = strsplit(msm_train, "[[:space:]]+")
length(word.list)       # Length: number of documents
length(word.list[[1]])  # A splited string of words

term.table = table(unlist(word.list))
term.table = sort(term.table, decreasing = TRUE)

del = names(term.table) %in% stop_words | term.table < 5
term.table = term.table[!del]
vocab = names(term.table)

get.terms = function(x) {
  index = match(x, vocab)
  index = index[!is.na(index)]
  rbind(as.integer(index-1), as.integer(rep(1, length(index))))
}
documents = lapply(word.list, get.terms)

D = length(documents)  
W = length(word.list)  
word.length = sapply(documents, function(x) sum(x[2, ]))  
N = sum(word.length)  
term.frequency <- as.integer(term.table)

fit = lda.collapsed.gibbs.sampler(documents = documents, K = 20, vocab = word.list, 
                                  num.iterations = G, alpha = 0.02, 
                                  eta = eta, initial = NULL, burnin = 0,
                                  compute.log.likelihood = TRUE)

theta = t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi = t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MusicRankings = list(phi = phi,
                     theta = 0.02,
                     word.length = word.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

json = createJSON(phi = MusicRankings$phi, 
                  theta = MusicRankings$theta, 
                  doc.length = MusicRankings$doc.length, 
                  vocab = MusicRankings$vocab, 
                  term.frequency = MusicRankings$term.frequency)

serVis(json, out.dir = 'vissample', open.browser = FALSE)