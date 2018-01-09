gc()
# install.packages(c("psych", "lattice", "dplyr","MASS"))
library(psych)
library(lattice)
library(dplyr)
# setwd("C:/Users/User/Dropbox/Research proposals/Masters thesis")

library(readr)
whitebait <- read_csv("C:/Users/User/Dropbox/Research proposals/Masters thesis/whitebait.csv", 
                        col_types = cols(X14 = col_skip()
                                         , date = col_date(format = "%m/%d/%Y")
                                         )
                      )
summary(whitebait); names(whitebait); dim(whitebait)

View(whitebait)

###### data cleaning-----
class(whitebait$date)
# whitebait$date <- as.Date(whitebait$date, format = "%d/%m/%Y")

class(whitebait$fishID)
whitebait$fishID <- as.factor(whitebait$fishID)

class(whitebait$comments)
# whitebait$comments <- as.character(whitebait$comments)
########################
class(whitebait$sampleSize)
n<- length(whitebait$sampleSize)
summary(whitebait$sampleSize)

whitebait[250,]
for(i in 1:n){
  if(whitebait$sampleSize[i] == "2x200") whitebait$comments[i] <- paste0("2x200, ", whitebait$comments[i])
}
rm(i)
whitebait$comments[250]
for(i in 1:n){
  if(whitebait$sampleSize[i] == "2x200")
    whitebait$sampleSize[i] <- 200
}
rm(i)
whitebait$sampleSize[250]

whitebait$sampleSize == "1/2kg"
whitebait[4289,]
for(i in 1:n){
  if(whitebait$sampleSize[i] == "1/2kg")
    whitebait$comments[i] <- paste0("1/2kg, ", whitebait$comments[i])
  }
rm(i)
whitebait$comments[4289]

for(i in 1:n){
  if(whitebait$sampleSize[i] == "1/2kg")
    whitebait$sampleSize[i] <- 200
}
rm(i)
whitebait$sampleSize[4324]

whitebait$sampleSize == "200+"
whitebait[9745,]
for(i in 1:n){
  if(whitebait$sampleSize[i] == "200+")  whitebait$comments[i] <- paste0("200+, ", whitebait$comments[i])
}
rm(i)
whitebait$comments[9745]

for(i in 1:n){
  if(whitebait$sampleSize[i] == "200+")
    whitebait$sampleSize[i] <- 200
}
rm(i)
whitebait$sampleSize[9745]

whitebait$sampleSize == "1kg"
whitebait[5710,]
for(i in 1:n){
  if(whitebait$sampleSize[i] == "1kg")
    whitebait$comments[i] <- paste0("1kg, ", whitebait$comments[i])
}
rm(i)
whitebait$comments[5710]
for(i in 1:n){
  if(whitebait$sampleSize[i] == "1kg")
    whitebait$sampleSize[i] <- 200
}
rm(i)
whitebait$sampleSize[5710]
summary(whitebait$sampleSize)
whitebait$sampleSize <- as.character(whitebait$sampleSize)
summary(whitebait$sampleSize)
whitebait$sampleSize[5710]
whitebait$sampleSize <- as.numeric(whitebait$sampleSize)
summary(whitebait$sampleSize)

hist(whitebait$sampleSize)
### classes of vectors--------
class(whitebait$weightFro)
whitebait$weightFro<- as.numeric(whitebait$weightFro)
summary(whitebait$weightFro)

class(whitebait$weightFresh)
whitebait$weightFresh<- as.numeric(whitebait$weightFresh)
summary(whitebait$weightFresh)

class(whitebait$lengthFresh)
whitebait$lengthFresh<- as.numeric(whitebait$lengthFresh)
summary(whitebait$lengthFresh)

class(whitebait$lengthFro)
whitebait$lengthFro<- as.numeric(whitebait$lengthFro)
summary(whitebait$lengthFro)

class(whitebait$species)
whitebait$species<- as.factor(whitebait$species)
summary(whitebait$species)

# EDA----
names(whitebait)
# jpeg(filename = "pairs.panels.2.jpg", width = 1280, height = 1024)
pairs.panels(whitebait[c(1,5,6,7,8,9,13)], las = 1)
# dev.off()

describeBy(whitebait$lengthFro, whitebait$species)
describeBy(whitebait$weightFro, whitebait$species)
describeBy(whitebait$depth, whitebait$species)
describeBy(whitebait$weightFro, whitebait$select)
describeBy(whitebait$lengthFro, whitebait$select)
describeBy(whitebait$depth, whitebait$select)

# raw proportions of each species overall
summ.table<- summary(whitebait$species)
sum(summ.table)

prop.summ.table<- table(NA)
for (i in 1:length(summ.table)){
 prop.summ.table[i]<-  summ.table[i]/sum(summ.table)
}

# jpeg(filename = "frozen_wgtVlth.jpg", width = 798, height = 364)
plot(whitebait$lengthFro, whitebait$weightFro
     , bg = whitebait$species
     , xlab = "length (mm)"
     , ylab = "weight (gm)"
     , las = 1
     # , pch = c(1:15)[whitebait$region]
     , pch = 21
     , cex = 1.2
     , cex.lab = 1.5
     , cex.axis = 1.5
     )
legend("topleft"
       , legend = c('Bully'
                    , 'Inanga', 'Koaro', 'Banded Kokopu', 'Giant Kokopu', 'Short Jaw Kokopu'
                    , 'Smelt', 'NA')
       , pt.bg = c(1:8)
       , pch = 21
       , cex = 1.2
       , bty = "n"
)
# dev.off()

# jpeg(filename = "fresh_wgtVlth.jpg", width = 1280, height = 1024)
plot(whitebait$lengthFresh, whitebait$weightFresh
     , col = whitebait$species
     , xlab = "length of fresh fish (mm)"
     , ylab = "weight of fresh fish (gm)"
     , las = 1
     # , pch = c(1:15)[whitebait$region]
     , pch = 19
)
legend("topleft"
       , legend = c('Bully'
                    , 'Inanga', 'Koaro', 'Banded Kokopu', 'Giant Kokopu', 'Short Jaw Kokopu'
                    , 'Smelt', 'NA')
       , col = c(1:8)
       , pch = 19
)
# dev.off()

jpeg(filename = "fro_wgtVdepth.jpg", width = 798, height = 364)
par(mfrow = c(1,2))
plot(whitebait$weightFro, whitebait$depth
     , bg = whitebait$species
     , ylab = "depth (mm)"
     , xlab = "weight (gm)"
     , las = 1
     # , pch = c(1:15)[whitebait$region]
     , pch = 21
     , cex = 1.2
     , cex.lab = 1.5
     , cex.axis = 1.5
)
plot(whitebait$lengthFro, whitebait$depth
     , bg = whitebait$species
     , ylab = ""
     , xlab = "length (gm)"
     , las = 1
     # , pch = c(1:15)[whitebait$region]
     , pch = 21
     , cex = 1.2
     , cex.lab = 1.5
     , cex.axis = 1.5
)
legend("bottomleft"
       , legend = c('Bully'
                    , 'Inanga', 'Koaro', 'Banded Kokopu', 'Giant Kokopu', 'Short Jaw Kokopu'
                    , 'Smelt', 'NA')
       , pt.bg = c(1:8)
       , pch = 21
       , bty = "n"
)
dev.off()
par(mfrow = c(1,1))

model1<- lm(weightFro ~ lengthFro + depth + species , data = whitebait)
summary(model1)
par(mfrow = c(2,2)); plot(model1)

model2<- lm(log(weightFro) ~ log(lengthFro) + depth + species + region
            , data = whitebait
            )
summary(model2)
par(mfrow = c(2,2)); plot(model2)


# Classification Tree with rpart------
par(mfrow = c(1,1))
library(rpart)
# install.packages("party")
library(party)
# install.packages("rpart.plot")
library(rpart.plot)

# grow tree
fit <- rpart(species ~ depth + weightFro + lengthFro + weightFresh + lengthFresh
             , method = "class"
             , data = whitebait )

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
# jpeg(filename = "prp.jpg", width = 1280, height = 1024)
prp(fit) # requires rpart.plot library
# dev.off()

?rpart







# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html


# Plotting Classification Trees with the plot.rpart and rattle pckages

library(rpart)				        # Popular decision tree algorithm
install.packages("rattle")
library(rattle)					# Fancy tree plot
# install.packages("RGtk2")
# install.packages("rpart.plot")
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
# install.packages("partykit")
library(partykit)				# Convert rpart object to BinaryTree
# install.packages("caret")
library(caret)					# Just a data source for this script
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]

# Make big tree
form <- as.formula(Class ~ .)
tree.1 <- rpart(form,data=data,control=rpart.control(minsplit=20,cp=0))
# 
plot(tree.1)					# Will make a mess of the plot
text(tree.1)
# 
prp(tree.1)					# Will plot the tree
prp(tree.1,varlen=3)				# Shorten variable names

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle
#
#-------------------------------------------------------------------
# Plot a tree built with RevoScaleR
# Construct a model formula
sdNames <- names(segmentationData)
X <- as.vector(sdNames[-c(1,2,3)])
form <- as.formula(paste("Class","~", paste(X,collapse="+")))
# Run the model
rx.tree <- rxDTree(form, data = segmentationData,maxNumBins = 100,
                   minBucket = 10,maxDepth = 5,cp = 0.01, xVal = 0)
# Plot the tree						
prp(rxAddInheritance(rx.tree))
fancyRpartPlot(rxAddInheritance(rx.tree))


prp(fit)


# Random Forest prediction -----
library(randomForest)
fit2 <- randomForest(species ~ weightFro + lengthFro + region, data=whitebait)
print(fit2) # view results
importance(fit2) # importance of each predictor plot(fit2)
plot(fit2)


# Discriminant Analysis page 821 R book-------
library(MASS)
modelDA1<- lda(species ~ lengthFro + weightFro + depth
               , CV = TRUE
               , data = whitebait
               )
plot(modelDA1)
summary(modelDA1$class)


### LDA furtherings -----
# http://maths-people.anu.edu.au/~johnm/courses/dm/math3346/2008/pdf/r-exercisesVI.pdf
library(lattice)
densityplot(~Pima.hat$x, groups = Pima.tr$type)
# A function that calculates the confusion matrices and overall accuracy would be helpful:
confusion <- function(actual, predicted, names = NULL, printit = TRUE, prior = NULL) {
  if (is.null(names))
    names <- levels(actual)
  tab <- table(actual, predicted)
  acctab <- t(apply(tab, 1, function(x) x/sum(x)))
  dimnames(acctab) <- list(Actual = names, "Predicted (cv)" = names)
  if (is.null(prior)) {
    relnum <- table(actual)
    prior <- relnum/sum(relnum)
    acc <- sum(tab[row(tab) == col(tab)])/sum(tab)
    }
  else {
    acc <- sum(prior * diag(acctab))
    names(prior) <- names
    }
  if (printit)
    print(round(c("Overall accuracy" = acc, "Prior frequency" = prior), 4))
  if (printit) {
    cat("\nConfusion matrix", "\n")
    print(round(acctab, 4))
    }
  invisible(acctab)
  }

# 
# # Assess the accuracy of the prediction
# # percent correct for each category of G
# ct <- table(whitebait$species, modelDA1$class)
# diag(prop.table(ct, 1))
# # total percent correct
# sum(diag(prop.table(ct)))

modelDA2<- qda(species ~ lengthFro + weightFro + depth
               , CV = TRUE
               , data = na.omit(whitebait)
               )
plot(modelDA2)
summary(modelDA2$class)


# Exploratory Graph for LDA or QDA
install.packages("klaR")
library(klaR)
partimat(species ~ lengthFro + weightFro + depth
         , data = na.omit(whitebait)
         , method = "lda") 
partimat(species ~ lengthFro + weightFro + depth
         , data = na.omit(whitebait)
         , method = "qda") 

# The plotted variable need not be in the data
data(iris)
iris2 <- iris[ , c(1,3,5)]
plineplot(Species ~ ., data = iris2, method = "lda",
          x = iris[ , 4], xlab = "Petal.Width")

names(whitebait)
whiteb2 <- whitebait[ , c(8,9,5)]
plineplot(species ~ ., data = na.omit(whiteb2), method = "lda",
          x = na.omit(whitebait[ , 13]), xlab = "Depth")
