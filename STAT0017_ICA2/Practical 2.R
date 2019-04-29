# Load uniform data
data(worldindices) 
u=worldindices
rm(worldindices)



cor(u[,1:3],method = c("kendall"))

u2u3u1=cbind(u[,2],u[,3],u[,1])
vinemodel=CDVineCopSelect(u2u3u1,type=2,familyset=c(1:10,13,14,23,24))
vinemodel

N=2000
u2u3u1_sim=CDVineSim(N, family=vinemodel$family, vinemodel$par,  vinemodel$par2, type=2)
cor(u2u3u1_sim,method = c("kendall"))
cor(u2u3u1,method = c("kendall"))
vinemodel_sim=CDVineCopSelect(u2u3u1_sim,type=2,familyset=c(1:10,13,14,23,24))
vinemodel_sim

# Vine selection "manually"
model_1 = BiCopSelect(u[,2],u[,3],familyset=c(1:10,13,14,16,23,24,26))
model_1
model_2 = BiCopSelect(u[,1],u[,3],familyset=c(1:10,13,14,16,23,24,26))
model_2
h1 = BiCopHfunc(u[,2],u[,3],model_1$family,model_1$par,model_1$par2)
h2 = BiCopHfunc(u[,1],u[,3],model_2$family,model_2$par,model_2$par2)
model_3 = BiCopSelect(h1$hfunc2,h2$hfunc2,familyset=c(1:10,13,14,16,23,24,26))


###########