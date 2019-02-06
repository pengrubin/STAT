data("USArrests")
matplot(USArrests,type = "l")
pairs(USArrests)

class(OrchardSprays)
plot(OrchardSprays)
data(state)
class(state.division)
plot(state.division)
plot.default(xvalues,quadx)

xvalues <- -5:5
quadx   <- xvalues^2
plot(xvalues, quadx, type="b", main="This is the title",
sub="This is the subtitle", xlab="x", ylab="x squared",
col.main="green",col="blue",lty=2,pch="*",lwd=3,cex=3)
plot(xvalues,quadx,xlim=c(-10,10))
plot(xvalues,quadx,xlim=c(-10,10),axes=FALSE)
boxplot(decrease ~ treatment, data = OrchardSprays,log = "y", col="lightblue",notch=TRUE)

data(morley)
plot(morley$Expt,morley$Speed)
plot(jitter(morley$Expt),morley$Speed)
sunflowerplot(morley$Expt,morley$Speed)
