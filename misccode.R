### misc code to test ###
x = sort(runif(10000))  #random data
f = c(4,8,16,32,64,128) #different freqs
spread = c(.1,.25,.5,1) #different noise
fspread = expand.grid(f= f,spread=spread)

#create ys
ys = apply(fspread, 1, function(u)  sin(u[1]*pi*x)+rnorm(10000)*u[2])
colnames(ys) = c(sapply(spread, function(q) paste('y', f,'_', q)))
head(ys); colMeans(ys)

#plot if desired
# par(mfrow=c(4,6))
# apply(ys, 2, function(y) plot(x,y, pch=19, cex=.25, col="#FF5500"))

#calculate correlation
cors = matrix(cor(x, ys), ncol=length(spread))
rownames(cors) = f; colnames(cors)=spread
cors


### misc code to test ###
x = sort(runif(1000))  #random data
f = c(4,8,16,32,64,128) #different freqs
spread = c(.1,.25,.5,1) #different noise
fspread = expand.grid(f= f,spread=spread)

#create ys
ys = apply(fspread, 1, function(u)  cos(u[1]*pi*x)+rnorm(1000)*u[2])
colnames(ys) = c(sapply(spread, function(q) paste('y', f,'_', q)))
head(ys)

#plot if desired
# par(mfrow=c(4,6))
# apply(ys, 2, function(y) plot(x,y, pch=19, cex=.25, col="#FF5500"))

cors = matrix(cor(x, ys), ncol=length(spread))
rownames(cors) = f; colnames(cors)=spread
cors

library(MASS)
MyPlot <- function(xy, xlim = c(-4, 4), ylim = c(-4, 4), eps = 1e-15) {
  title = round(cor(xy[,1], xy[,2]), 2)
  if (sd(xy[,2]) < eps) title = "" # corr. coeff. is undefined
  plot(xy, main = title, xlab = "", ylab = "",
       col = "#FF5500", pch = 16, cex = 0.2,
       xaxt = "n", yaxt = "n", bty = "n",
       xlim = xlim, ylim = ylim)
}
MvNormal <- function(n = 1000, cor = 0.8) {
  for (i in cor) {
    sd = matrix(c(1, i, i, 1), ncol = 2)
    x = mvrnorm(n, c(0, 0), sd, empirical=T)
    #MyPlot(x)
    return(x)
  }
}

dat = sapply(c(-.75,-.25,0,.25,.75), function(x) MvNormal(1000,x), simplify=F)
dat = data.frame(do.call(rbind, dat))
colnames(dat) = c("x","y")
dat$r = factor(rep(as.character(c(-.75,-.25,0,.25,.75)), e=1000), levels=as.character(c(-.75,-.25,0,.25,.75)))

library(ggplot2); library(reshape2)
ggplot(aes(x,y), data=dat) + geom_point(color="#FF5500", alpha=.5) + geom_smooth(method='lm', se=F) + facet_grid(r~.) + ggtheme


library(minerva)
mine(mydat, n.cores=3)$MIC

library(Hmisc)
hoeffd(mydat)

# example minerva
x = rnorm(10000, sd=.01)
y = rnorm(10000, sd=.01)
library(minerva)

mine(x,y, n.cores=3)

# duplicate fig S1 of Rashef 2011 SOM for n=204
datlist = vector('list',100)
datlist = lapply(datlist, function(x) data.frame(x=rnorm(204), y=rnorm(204)))
library(minerva)
mineout = sapply(datlist, function(x) mine(x)$MIC[2])
mean(mineout)
hist(mineout, "FD")
