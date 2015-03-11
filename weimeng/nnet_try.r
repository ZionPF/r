library(nnet)
library(playwith) 

irisdata <- read.csv(file="~/r/weimeng/Iris-eng.csv",head=TRUE,sep=",")

irisTrainData = sample(1:150,100)
irisValData = setdiff(1:150,irisTrainData)
ideal <- class.ind(irisdata$species)

irisANN = nnet(irisdata[irisTrainData,-5], ideal[irisTrainData,], size=10, softmax=TRUE)
predict(irisANN, irisdata[irisValData,-5], type="class")

#SVM:
irisSVM <- ksvm(species~.,data=irisdata[irisTrainData,],type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)
fitted(irisSVM)
predict(irisSVM, irisdata[irisValData,-5], type="probabilities")



#plotly

library("devtools")
#install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)
set_credentials_file("eaglezpf", "rh6atju642")
py <- plotly()  # open plotly connection
ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)

py$ggplotly(ggiris)


#animation
generate_bm_data = function(n = 20, grp = 4) {
  nmax = ani.options("nmax")
  bm.data = NULL
  x = rnorm(n * grp)
  y = rnorm(n * grp)
  for (i in 1:nmax) {
    bm.data = rbind(bm.data, cbind(x = x <- x + rnorm(n * grp), y = y <- y + rnorm(n * grp), 
                                   step = i, group = rep(1:grp, each = n), id = rep(1:n, grp)))
  }
  bm.data = as.data.frame(bm.data)
  bm.data$id = factor(bm.data$id)
  bm.data$group = factor(bm.data$group)
  bm.data
}

set.seed(123)
ani.options(nmax = 50)
n = 20; grp = 4
bm.data = generate_bm_data(n, grp)


saveHTML({
  for (i in unique(bm.data$step)) {
    print(qplot(x, y, facets = ~group, geom = "line", colour = id, alpha = I(0.1), 
                data = subset(bm.data, step <= i), main = paste("step", i)) + 
            xlim(range(bm.data$x)) + ylim(range(bm.data$x)) + 
            geom_point(aes(x = x, y = y, facets = ~group, size = I(rep(3, n * grp))), 
                       data = subset(bm.data, step == i)) + 
             opts(legend.position = "none"))
  }
}, interval = 0.2, movie.name = "ggplot2-brownian-motion.gif", ani.width = 600, ani.height = 600)

