%This is a PCA plot I creates for 3 differnet species of Bull.





ass <- read.csv("ass1.csv")

cor(ass$V8,ass$V9)
cor(ass[-1]) 

var(ass[-1])

var(ass[,2,1])+var(ass[,3,2])+var(ass[,4,3])+var(ass[,5,4])+var(ass[,6,5])+var(ass[,7,6])+var(ass[,8,7])+var(ass[,9,8])+var(ass[,10,9])

pca <- princomp(ass[-1],cor = TRUE)
summary(pca)
fviz_pca_ind(pca, gem.ind = "point", pointshape = 21, pointsize = 3, fill.ind = as.factor(ass$X), 
             col.var = "red",
             repel = TRUE,
             palette = "rickandmorty", 
             legend.title = "observations") +
  ggtitle("First Two Principle Compenents") +
  theme(plot.title = element_text(hjust = 0.5)
  )



