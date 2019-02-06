##################################################################################################################################
# 使用R TCGA 下载数据
# Data 2018-01-30
# Author Howard MENG
# E-mail meng_howard@126.com
##################################################################################################################################

# install
source("http://bioconductor.org/biocLite.R")
biocLite("RTCGAToolbox")

# load package 
library(RTCGAToolbox)

# check the running date and analysis date 
getFirehoseAnalyzeDates()
getFirehoseRunningDates()

# check the dataset type (cancer type)
cancer_type_list = getFirehoseDatasets()

# READ mutation data and clinical data
brcaData <- getFirehoseData(dataset="READ", runDate="20160128",forceDownload=TRUE, RNASeq2GeneNorm = T,miRNAArray = T)
brcaData

RNAseq = getData(brcaData,type = "RNASeq2GeneNorm")

# download all data 
TCGA_FIRE_ALL_DATA = list()

for(index in 1:length(cancer_type_list)){
  cancer_type = cancer_type_list[index]
  print(cancer_type)
  print("------------------------------------------------")
  
  collect_data = getFirehoseData(dataset=cancer_type, runDate="20160128",forceDownload=TRUE, RNASeq2GeneNorm = T,miRNAArray = T)
  TCGA_FIRE_ALL_DATA[[cancer_type]] = collect_data
}


# make expression matrix
names(TCGA_FIRE_ALL_DATA)

