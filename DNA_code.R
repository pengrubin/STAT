library(data.table)
library(plyr)
library(dplyr)
library(glue)
library(zoo)
library(tidyr)

#设置工作路径
setwd('C:/Users/Pengr/Desktop')

#读取数据
rawdata <- read.csv("stable panel.csv",fileEncoding = "UTF-8",stringsAsFactors = F) %>% as.data.table
rawdata[,Company:=gsub('EDUCATION','Education',rawdata[,Company])]
rawdata[,Company:=gsub('education','Education',rawdata[,Company])]
rawdata <- rawdata[Version=='DB'|Version=='Report'|Version=='Stable panel',]

#基础清洗
rawdata <- rawdata[FQ.v2!=0&!is.na(FQ.v2),]

#加入for循环
list <- rawdata[,'Company'] %>% unique
#list <- list[-8]
#list <- list[-8]
#list <- list[-93]
list <- list[-94]
list <- list[-94]
final <- list %>% as.data.table
final[,c('R~DB','R~stable 1','R~stable 2','co_sheet$DB','co_sheet$`Stable panel 1`','co_sheet$`Stable panel 2`','correlation~DB','correlation~stable 1','correlation~stable 2'):='test']
final[,c('coverage rate~DB','coverage rate~stable'):='test']
ex_data <- data.table

for (i in 1:dim(list)[1]) {
  #i=16
  
  #选取公司
  co_data <- rawdata[Company==list[i],]
  co_data <- co_data[,c("Version","FQ.v2","amt",'amt..12.')]
  co_data$amt..12.[is.na(co_data$amt..12.)] <- 0
  #stable panel分出支路处理
  #二号方案处理
  panel <- rawdata[Company==list[i],][Version=='Stable panel',][order(CalendarYearMonth)][,'amt/amt1':=amt/amt..1.]
  panel$`amt/amt1`[is.na(panel$`amt/amt1`)] <- 1
  panel[,'V':=cumprod(panel$`amt/amt1`)]
  panel <- panel[,mean(V,na.rm = T),by=c('Version','FQ.v2')]
  #二号方案修正
  test <- panel[,c('FQ.v2','V1')]
  test$FQ.v2 <- gsub('19','20',test$FQ.v2)
  test$FQ.v2 <- gsub('18','19',test$FQ.v2)
  test$FQ.v2 <- gsub('17','18',test$FQ.v2)
  test$FQ.v2 <- gsub('16','17',test$FQ.v2)
  test$FQ.v2 <- gsub('15','16',test$FQ.v2)
  test$FQ.v2 <- gsub('14','15',test$FQ.v2)
  test$FQ.v2 <- gsub('13','14',test$FQ.v2)
  test$FQ.v2 <- gsub('12','13',test$FQ.v2)
  test$FQ.v2 <- gsub('11','12',test$FQ.v2)
  panel <- merge(panel,test,all.x = T,by='FQ.v2')
  panel[,'V1':=V1.x/V1.y][,V1.y:=NULL][,V1.x:=NULL]
  panel[,Version:='Stable panel 2']
  #一号方案处理
  stable <- co_data[Version=='Stable panel','V':=amt/amt..12.][Version=='Stable panel',]
  stable$V[is.infinite(stable$V)] <- NA
  stable <- stable[,mean(V,na.rm = T),by=c('Version','FQ.v2')]
  #stable <- co_data[Version=='Stable panel',sum(amt)/sum(amt..12.),by=c('Version','FQ.v2')]
  stable[,Version:='Stable panel 1']
  others <- co_data[Version=='DB'|Version=='Report',sum(amt)/sum(amt..12.),by=c('Version','FQ.v2')]
  #分支处理完毕
  co_data <- rbind(stable,panel,others)
  colnames(co_data)[3] <- 'amt'
  co_data[,amt:=amt-1]
  co_data$amt[is.infinite(co_data$amt)] <- NA
  
  #加入coverage rate
  cov_data <- rawdata[Company==list[i],]
  cov_data <- cov_data[,c("Version","FQ.v2","amt",'amt..12.')]
  #cov_data$amt[is.na(cov_data$amt..12.)] <- NA
  #cov_data$amt..12.[is.na(cov_data$amt)] <- NA
  cov_data <- cov_data[,sum(amt),by=c('Version','FQ.v2')]
  colnames(cov_data)[3] <- 'amt'
  
  #高表变宽表
  co_sheet <- spread(co_data, Version, amt, fill = NA, convert = FALSE, drop = TRUE)
  cov_sheet <- spread(cov_data, Version, amt, fill = NA, convert = FALSE, drop = TRUE)
  
  #coverage rate逻辑处理
  cov_sheet$Report[is.na(cov_sheet$DB)] <- NA
  cov_sheet$`Stable panel`[is.na(cov_sheet$DB)] <- NA
  cov_sheet$DB[is.na(cov_sheet$`Stable panel`)] <- NA
  cov_sheet$Report[is.na(cov_sheet$`Stable panel`)] <- NA
  cov_sheet$`Stable panel`[is.na(cov_sheet$Report)] <- NA
  cov_sheet$DB[is.na(cov_sheet$Report)] <- NA
  
  #建立模型
  final[Company==list[i],'R~DB':=summary(lm(co_sheet$Report~co_sheet$DB-1))$r.squared]
  final[Company==list[i],'R~stable 1':=summary(lm(co_sheet$Report~co_sheet$`Stable panel 1`-1))$r.squared]
  final[Company==list[i],'R~stable 2':=summary(lm(co_sheet$Report~co_sheet$`Stable panel 2`-1))$r.squared]
  final[Company==list[i],'co_sheet$DB':=lm(co_sheet$Report~co_sheet$DB)$coefficients[2]]
  final[Company==list[i],'co_sheet$`Stable panel 1`':=lm(co_sheet$Report~co_sheet$`Stable panel 1`)$coefficients[2]]
  final[Company==list[i],'co_sheet$`Stable panel 2`':=lm(co_sheet$Report~co_sheet$`Stable panel 2`)$coefficients[2]]
  final[Company==list[i],'correlation~DB':=cor(co_sheet$Report,co_sheet$DB,use="complete.obs")]
  final[Company==list[i],'correlation~stable 1':=cor(co_sheet$Report,co_sheet$`Stable panel 1`,use="complete.obs")]
  final[Company==list[i],'correlation~stable 2':=cor(co_sheet$Report,co_sheet$`Stable panel 2`,use="complete.obs")]
  
  #写入overage rate
  final[Company==list[i],'coverage rate~DB':=sum(cov_sheet$DB,na.rm = TRUE)/sum(cov_sheet$Report,na.rm = TRUE)/1000000]
  final[Company==list[i],'coverage rate~stable':=sum(cov_sheet$`Stable panel`,na.rm = TRUE)/sum(cov_sheet$Report,na.rm = TRUE)/1000000]
  
  #写入加工后原始数据
  if (i==1) {
    ex_data <- co_data[,'Company':=list[i]]
  }
  else {
    ex_data <- rbind(ex_data,co_data[,'Company':=list[i]])
  }
  
  
}



#写入文件
write.csv(final,'stable_panel_VS_DB.csv',row.names = F,fileEncoding = "UTF-8")
write.csv(ex_data,'stable_panel_VS_DB_exdata.csv',row.names = F,fileEncoding = "UTF-8")
