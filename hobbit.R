library(XML)
stem <- "http://www.5novels.com/classics/u5688"
hobbit <- NULL
for(i in 1:74) {
  print(paste0(i, "\n"))
  if(i==1) { url <- paste0(stem, ".html") } 
  else { url <- paste0(stem, "_", i, ".html") }
  x <- htmlTreeParse(url, useInternalNodes=TRUE)
  xx <- xpathApply(x, "//p", xmlValue)
  hobbit <- c(hobbit, gsub("\r", "", xx[-length(xx)]))
  Sys.sleep(0.5)
}
hobbit = paste(hobbit, collapse=' ')


library(ngram)
ng2 <- ngram(hobbit, n=2)

print(babble(ng2, 150, seed=987654321))
