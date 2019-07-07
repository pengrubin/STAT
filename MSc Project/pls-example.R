install.packages("pls")
library(pls)
#do a PLSR on the gasoline data to illustrate the use of pls
data(gasoline)
options(digits = 4)
gasTrain <- gasoline[1:50,] #Row 1~50
gasTest <- gasoline[51:60,] #Row 51~60

#fit a PLSR model
gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")
#get an overview of the fit and validation results
summary(gas1)  #The validation results here are Root Mean Squared Error of Prediction (RMSEP). 
               #There are two cross-validation estimates: 
               #CV is the ordinary CV estimate, and adjCV is a bias-corrected CV estimate 

#Cross-validated RMSEP curves for the gasoline data
#Figure 2(simpler)
plot(RMSEP(gas1), legendpos = "topright")

#Figure 3
#the cross-validated predictions with two components versus measured values
plot(gas1, ncomp = 2, asp = 1, line = TRUE)

#Figure 4
#a pairwise plot of the score values for the three first components
plot(gas1, plottype = "scores", comps = 1:3)

#extract the explained variances
explvar(gas1)

#Figure 5
plot(gas1, "loadings", comps = 1:2, legendpos = "topleft",
             labels = "numbers", xlab = "nm")
abline(h = 0)

#predict the response values of new observations
predict(gas1, ncomp = 2, newdata = gasTest)
#calculate the test set RMSEP
RMSEP(gas1, newdata = gasTest)



#5 Fitting models
data(yarn)
dens1 <- plsr(density ~ NIR, ncomp = 5, data = yarn)

dim(oliveoil$sensory)
plsr(sensory ~ chemical, data = oliveoil)
trainind <- which(yarn$train == TRUE)
dens2 <- update(dens1, subset = trainind)
dens3 <- update(dens1, ncomp = 10)
live1 <- plsr(sensory ~ chemical, scale = TRUE, data = oliveoil)
gas2 <- plsr(octane ~ msc(NIR), ncomp = 10, data = gasTrain)
predict(gas2, ncomp = 3, newdata = gasTest)


#Choosing the number of components with cross-validation
ncomp.onesigma <- selectNcomp(gas2, method = "onesigma", plot = TRUE,ylim = c(.18, .6))
ncomp.permut <- selectNcomp(gas2, method = "randomization", plot = TRUE,ylim = c(.18, .6))
gas2.cv <- crossval(gas2, segments = 10)
plot(MSEP(gas2.cv), legendpos="topright")
summary(gas2.cv, what = "validation")
plot(gas1, plottype = "coef", ncomp=1:3, legendpos = "bottomleft",
     labels = "numbers", xlab = "nm")
#get predictions from the model built in Section 3, with two and three components
predict(gas1, ncomp = 2:3, newdata = gasTest[1:5,])
#get predictions from a model with only component 2
predict(gas1, comps = 2, newdata = gasTest[1:5,])
#drop the singleton dimensions explicitly
drop(predict(gas1, ncomp = 2:3, newdata = gasTest[1:5,]))
pls.options(plsralg = "oscorespls")