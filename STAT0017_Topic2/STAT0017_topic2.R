mydataframe <- read.table("/Users/hongwei/Documents/GitHub/STAT/STAT0030_Lab9_GLMs/nino3.dat",header = TRUE)
gamma.glm <- glm(y ~ x1 + x2, family=Gamma, data=mydataframe)
