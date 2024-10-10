### Not Run

getwd()

### Factor Analysis and PCA
### object named "uno" (assigned by the instructor)

### From stata, spss, and excel

install.packages("haven")
library(haven)

uno<- read_stata("uno.dta") 
uno<- read_spss("uno.sav") 
uno<- read_spss("uno.por")

install.packages("readxl")
library(readxl)
uno <- read_excel("uno.xlsx")

### From csv

uno <- read.csv("uno.csv")

### correlation matrix

install.packages("Hmisc")
library(Hmisc)

cor(uno)

### pvalues

rcorr(as.matrix(uno))


### Cronbach's Alpha

install.packages("ltm")
library(ltm)

cronbach.alpha(uno, standardized = T, CI = T, 
    probs = c(0.025, 0.975), B = 1000, na.rm = T)



library(stats) ### default

### Please select number of factors to be fitted

factanal(uno, factors=2,rotation = "varimax")

### storing in object fit

fit<-factanal(uno, factors=2,rotation = "varimax")


### results on the screen

print(fit, digits=3, cutoff=.1, sort=TRUE)

### When using scores ("Bartlett" for instance), to get scores...

fit$scores


### usage

factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA,
         subset, na.action, start = NULL,
         scores = c("none", "regression", "Bartlett"),
         rotation = "varimax", control = NULL, …)







### PCA -table 

prcomp(uno) # signs may depend on platform


### PCA -graph

install.packages("FactoMineR")
library(FactoMineR)

### all the elements
PCA(uno) 


### just the graph

result <- PCA(uno)	

### or all the elements

print(result)

result$eig  ### eigenvalues (sort of scores or factor loadings)
result$call ### summary stats  


### a myriad of other packages, for example:

install.packages("psych")
library(pysch)


### Plotting PCA with facto

install.packages("factoextra")
library(factoextra)

### an easy plot (variance and dimensions)

fviz_screeplot(result, addlabels = TRUE, ylim = c(0, 50))


### same PCA graph (just prettier, b&w)

fviz_pca_var(result, col.var = "black")

### PCA, just colored

fviz_pca_var(result, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping)

### End