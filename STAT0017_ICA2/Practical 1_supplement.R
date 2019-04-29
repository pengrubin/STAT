install.packages("copula")
library("copula")

# normal Copula
normal<-normalCopula(0.7)
par(mfrow = c(1, 3))
persp(normal, dCopula, main ="Normal copula density")
persp(normal, pCopula, main = "Normal CDF")
contour(normal, pCopula, xlim = c(0, 1), ylim=c(0, 1), main = " Normal CDF Contour plot")

# t Copula
t<-tCopula(param = 0.2, dim = 2, df = 3)
par(mfrow = c(1, 3))
persp(t, dCopula, main ="t copula density")
persp(t, pCopula, main = "t CDF")
contour(t, pCopula, xlim = c(0, 1), ylim=c(0, 1), main = " t CDF Contour plot")

# Clayton Copula
clayton <- claytonCopula(param = 3)
par(mfrow = c(1, 3))
persp(clayton, dCopula, main ="Clayton copula density")
persp(clayton, pCopula, main = "Clayton CDF")
contour(clayton, pCopula, xlim = c(0, 1), ylim=c(0, 1), main = " Clayton CDF Contour plot")

# Gumbel Copula
gumbel <- gumbelCopula(param = 3)
par(mfrow = c(1, 3))
persp(gumbel, dCopula, main ="Gumbel copula density")
persp(gumbel, pCopula, main = "Gumbel CDF")
contour(gumbel, pCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Gumbel CDF Contour plot")


# Frank Copula
frank <- frankCopula(param = 3)
par(mfrow = c(1, 3))
persp(normal, dCopula, main ="Frank copula density")
persp(normal, pCopula, main = "Frank CDF")
contour(normal, pCopula, xlim = c(0, 1), ylim=c(0, 1), main = " Frank CDF Contour plot")
