library(data.table)
library(plyr)
library(dplyr)
library(glue)
options(stringsAsFactors = F)
source('/data1/trident/liuzhi/projects/common.files/liuzhi.functions.R')
##### marco #####
data.time = '201804'
data.time.last = "201803"

rawdata.path = '/data1/trident/liuzhi/data/personalcare/{data.time}/UTF-8/' %>% glue
#rawdata.list = list.files(rawdata.path, pattern = 'jd|tmall')
rawdata.list = list.files(rawdata.path, pattern = 'jd|tmall|yhd')
output.path <- '/data1/trident/hongwei/project/haircare/output/{data.time}/' %>% glue
if(!dir.exists(output.path)){dir.create(output.path)}
pla.list = c('jd', 'tmall','yhd')

last.output.path <- '/data1/trident/hongwei/project/haircare/output/{data.time.last}/' %>% glue

dict.path <- '/home/liuzhi/R/x86_64-pc-linux-gnu-library/3.4/jiebaRD/dict/'
func.path <- '/data1/trident/hongwei/project/haircare/coding/'
research.path <- '/home/liuzhi/storecode/haircare/'
sku.std.file <- 'haircare.research.file.20170324.csv'




##### (1) 抽数 #####
{
  total_sum = data.table()
  for (i in 1:length(rawdata.list)) {
    cat('\nnow extracting', rawdata.list[i], '\n')
    setwd(rawdata.path)
    rawdata = fread(
      rawdata.list[i],
      header = T,
      stringsAsFactors = F,
      colClasses = 'character',
      showProgress = T
    )
    
    newdata = rawdata[rawdata$to2 == "洗发护发" ,]     ###天猫需要去除to列和match.brand列
    newdata = subset(newdata, select = c(1:to6))
    newdata$Month = data.time
    newdata$Site = pla.list[i]
    setwd(output.path)
    to_sum <- newdata[, c('Site', 'to3')]
    total_sum <- rbindlist(list(total_sum, to_sum))
    write.csv(
      newdata,
      paste('haircare', pla.list[i], data.time, 'match.raw.csv', sep = '.'),
      row.names = F,
      fileEncoding = 'gb18030'
    )
  }
}

{
category_count=total_sum[,.N,.(Site,to3)]
category_count=spread(category_count,to3,N,fill = 0)
category_count$sum=rowSums(category_count[,2:dim(category_count)[2]])

category_count=category_count[order(Site)]

write.csv(category_count,paste('category_check',data.time,'csv',sep='.'),row.names = F, fileEncoding = 'gb18030')

category.this.month=colnames(category_count[2:dim(category_count)[2]-1])

category_count.last.month=read.csv(paste0(last.output.path,paste('category_check',data.time.last,'csv',sep='.')),fileEncoding = 'gb18030',header=T) %>% setDT
category.last.month=colnames(category_count.last.month[2:dim(category_count.last.month)[2]-1])

category.to.be.checked=intersect(category.last.month,category.this.month)

cthis=category_count[,category.to.be.checked,with=F]
clast=category_count.last.month[,category.to.be.checked,with=F]
}



##### (2) match #####

source(paste0(func.path, 'match.sku.functions.R'), encoding = 'utf8')
data.file <- paste('haircare', pla.list, data.time, 'match.raw.csv', sep = '.')
output.file <- paste('haircare', pla.list, data.time, 'match.output.csv', sep = '.')
sub.file <- 'gsub.csv'
productType <- NULL

sep = ','
work.path= output.path

source(paste0(func.path, 'match.sku.functions.R'), encoding = 'utf8')



for (i in 1:length(data.file)){
  cat('now', data.file[i], '\n')
  
  rlt <- match.sku(data.file = data.file[i], sku.std.file = sku.std.file, output.file = output.file[i],
                   sub.file = sub.file, productType = productType, sep = sep, regard.as.name = c('skuname'),
                   work.path = work.path, dict.path = dict.path, func.path = func.path, research.path = research.path, fileEncoding = 'gb18030',
                   if.clean = T , if.match = T , if.return = T, if.clean.size = T, last.research = 'name5')
  assign(paste0('rlt_', letters[i]), rlt)
}


##### (3) combind匹配后的数据 #####

source(paste0(func.path, 'match.k_functions.R'), encoding = 'utf8')
haircare.all = c()
for(i in 1:length(pla.list)){
  cat('\n now ', pla.list[i], '\n')
  temp = fread(paste('haircare', pla.list[i], data.time, 'match.output.csv', sep = '.'), header = T, stringsAsFactor = F, colClasses = 'character', showProgress = T)
  
  # 重算yhd的salescount
  if (pla.list[i] =='yhd') {
    temp <- as.data.table(temp)
    n_skuid <- temp[,list(n_skuid=length(skuid)),by=list(spuid)]#根据spuid数有多少个skuid
    temp$nsku_spu = rep(n_skuid$n_skuid, n_skuid$n_skuid)
    temp$salescount <- as.numeric(temp$salescount) / as.numeric(temp$nsku_spu)
    temp[,nsku_spu:=NULL]
  }
  haircare.all = rbind(haircare.all, temp, fill=T)
}
write.csv(haircare.all, paste('haircare', 'all', data.time, 'match.output.csv', sep = '.'), row.names = F)
# haircare.all<-read.csv(paste('haircare', 'all', data.time, 'match.output.csv', sep = '.'),header = TRUE,stringsAsFactors = FALSE,fileEncoding = 'utf8')


##### (4) 用有12列的research file文件匹配variant 和funciton #####

# haircare.all = read.csv(paste('haircare', 'all', data.time, 'match.output.csv', sep = '.'), header=T, stringsAsFactors = F, colClasses = 'character') %>% setDT
line.file = read.csv(paste0(research.path,sku.std.file), header=T, stringsAsFactors = F, colClasses = 'character',fileEncoding = 'gb18030')
line.file$spu.matched = do.call('paste0', line.file[, c("brand", "brand.en", "name1", "name2", "name3", "name4", "name5")]) ##实现phoneix加spu.matched功能
line.file = line.file[, c("spu.matched","Function", "Variant")] 

# 改动的spu.matched要与后面一致
haircare.all$spu.matched[grepl('欧莱雅.*(沙龙|专业)',haircare.all$skuname) & !grepl('欧莱雅沙龙',haircare.all$brand.matched)] = 'brand changed'
haircare.all$spu.matched[grepl('男',haircare.all$skuname) & !grepl('女|沙龙|专业',haircare.all$skuname) & grepl('欧莱雅[^男]',haircare.all$brand.matched)] = 'brand changed'
haircare.all$spu.matched[haircare.all$brand.matched=='欧莱雅男士lorealmen' & (grepl('男.*女|女.*男',haircare.all$skuname)|!grepl('男',haircare.all$skuname))] = 'brand changed'

haircare.all = left_join(haircare.all, line.file, by = "spu.matched")
haircare.all$Variant = ifelse(is.na(haircare.all$Variant), haircare.all$brand.matched, haircare.all$Variant)


##### (5) 综合计算 #####
{
haircare.data = haircare.all
haircare.data$imported = ifelse(grepl('YES|TRUE', haircare.data$is_global), 'YES', 
                                ifelse(grepl('NO|FALSE', haircare.data$is_global), 'NO', 'others'))
##colnames(haircare.data)[colnames(haircare.data) == 'sku.matched'] = 'spu.matched'


# 算gift,promotionType #
haircare.data$gift = ifelse(grepl('(?<![直发配])送', haircare.data$skuname, perl =T), 'YES', 'NO')

haircare.data$salesinfo_new = paste(haircare.data$salesinfo1, haircare.data$salesinfo2, haircare.data$salesinfo3, sep = '')
haircare.data$promotionType = ifelse(grepl('减|省|折', haircare.data$salesinfo_new), '满减',
                                     ifelse(grepl('赠|送', haircare.data$salesinfo_new), '满赠',
                                            ifelse(grepl('包邮', haircare.data$salesinfo_new), '包邮',
                                                   ifelse((!grepl('[减省折赠送]|包邮', haircare.data$salesinfo_new))&nchar(haircare.data$salesinfo_new)>0, '其他', '无'))))



# 分套装 #
key.shampoo = '(洗[0-9]+)|洗发|(头皮.*(清洁|净化))|去角质啫喱|洗头|去屑|防脱生发|发水|(洗|护)发二合一|发露|香波|([洁洗爽][发髮头頭](粉|泡|精|浆|皂|水|液|乳|露|香波|喷雾|啫喱)?)|([水干]洗喷雾)|((去|头)(皮|屑)(调理|深透)?(净化|活化|洁净|净洁|清洁)(乳|啫喱|凝露|泥|霜|酵母))|((修)?护(色)?香波)|发宝洗剂|洁发宝|泡泡发露|平衡乳|润黑露|头发干洗|头垢清洁露|洗发精华素'
key.conditioner = '(护[发]?[0-9]+)|(润(发)?[0-9]+)|结构素|护发原液|((护|护理|润|修护)[发髮头頭]?(油|素|乳|精油|霜|膏|乳))|头油|(头皮.*[按摩|营养])|((营|滋)养水)|精油|精华素|护发露|脱发喷雾|锁色液|护发精华液|护色液|胶结物精华液|修护液|护理剂|育发液|([育乌]发膏)|滋养液|毛滋'
key.mask = '(膜[0-9]+)|([发髮](面)?(膜|霜))|(精华(膜|霜))|(修[复护](膜|霜))|倒膜|([局焗锔]油膏)|护效霜|调理霜'
key.others = '([沐浴][0-9]+)|(沐?(浴|澡)(露|液|乳|油|波|盐|剂|啫喱|者哩))|(搓泥浴(宝|露))|((护|润|香|舒|紧|美|身)(肤|体)(霜|乳|油))|洗手液|护手霜|(?<!头发)面膜'

haircare.data$setType = ifelse(haircare.data$productType == '套装'
                               & grepl(key.others, haircare.data$skuname, perl = T)
                               & !grepl('洗[发]?护[发]?(润|养)?(套装|套组|旅行装|([大]?礼包)|补充装|组合|(.件套))|洗护系列套装组合|洗一护|洗护特惠套装|(防脱[生育]发套装)', haircare.data$skuname), '其他套装', 
                               ifelse(haircare.data$productType == '套装' 
                                      & ((grepl(key.shampoo, haircare.data$skuname) 
                                          & grepl(key.conditioner, haircare.data$skuname))
                                         | grepl('洗[发]?护[发]?(润|养)?(套装|套组|旅行装|([大]?礼包)|补充装|组合|(.件套))|洗护系列套装组合|洗一护|洗护特惠套装|(防脱[生育]发套装)', haircare.data$skuname))
                                      & grepl(key.mask, haircare.data$skuname), '洗护膜套装', 
                                      ifelse((haircare.data$productType == '套装' 
                                              & grepl(key.shampoo, haircare.data$skuname) 
                                              & grepl(key.conditioner, haircare.data$skuname))
                                             | grepl('洗[发]?护[发]?(润|养)?(套装|套组|旅行装|([大]?礼包)|补充装|组合|(.件套))|洗护系列套装组合|洗一护|洗护特惠套装|(防脱[生育]发套装)', haircare.data$skuname), '洗护套装',
                                             ifelse(haircare.data$productType == '套装' 
                                                    & grepl(key.shampoo, haircare.data$skuname) 
                                                    & grepl(key.mask, haircare.data$skuname), '洗膜套装', 
                                                    ifelse(haircare.data$productType == '套装' 
                                                           & grepl(key.conditioner, haircare.data$skuname) 
                                                           & grepl(key.mask, haircare.data$skuname), '护膜套装', 
                                                           ifelse(haircare.data$productType == '套装', '其他套装', NA))))))

haircare.data$id.std = as.numeric(as.factor(haircare.data$brand.matched))
test = my.sep(haircare.data$brand.matched)
haircare.data$en.std = test$en
haircare.data$chn.std = test$chn
haircare.data$en.site = test$en
haircare.data$chn.site = test$chn
haircare.data$brand = haircare.data$brand.matched

# 改brand #
haircare.data$Variant[grepl('欧莱雅.*(沙龙|专业)',haircare.data$skuname) & !grepl('欧莱雅沙龙',haircare.data$brand.matched)] = '欧莱雅沙龙专属lorealprofessionnel'
haircare.data$brand.matched[grepl('欧莱雅.*(沙龙|专业)',haircare.data$skuname) & !grepl('欧莱雅沙龙',haircare.data$brand.matched)] = '欧莱雅沙龙专属lorealprofessionnel'
haircare.data$Variant[grepl('男',haircare.data$skuname) & !grepl('女|沙龙|专业',haircare.data$skuname) & grepl('欧莱雅[^男]',haircare.data$brand.matched)] = '欧莱雅男士lorealmen'
haircare.data$brand.matched[grepl('男',haircare.data$skuname) & !grepl('女|沙龙|专业',haircare.data$skuname) & grepl('欧莱雅[^男]',haircare.data$brand.matched)] = '欧莱雅男士lorealmen'
haircare.data$Variant[haircare.data$brand.matched=='欧莱雅男士lorealmen' & (grepl('男.*女|女.*男',haircare.data$skuname)|!grepl('男',haircare.data$skuname))] = '欧莱雅巴黎lorealparis'
haircare.data$brand.matched[haircare.data$brand.matched=='欧莱雅男士lorealmen' & (grepl('男.*女|女.*男',haircare.data$skuname)|!grepl('男',haircare.data$skuname))] = '欧莱雅巴黎lorealparis'

# 加function #
haircare.data$function.temp = ifelse(grepl('止痒|[头去净]屑', haircare.data$skuname), 'BeautyAD',
                                     ifelse(grepl('清[新爽凉]|[控去]油', haircare.data$skuname), 'TotalClean',
                                            ifelse(grepl('防脱|[脱掉健韧]发', haircare.data$skuname), 'AHF',
                                                   ifelse(grepl('[水滋莹盈养]润|丝[滑质]|[润滋]养', haircare.data$skuname), 'Dry',
                                                          ifelse(grepl('损伤|护[理养]|[理修]护|修复', haircare.data$skuname), 'Damage',
                                                                 ifelse(grepl('[护锁炫绚]色|莹彩|彩护|乌[黑发]|黑亮', haircare.data$skuname), 'Color',
                                                                        ifelse(grepl('亮泽|[炫莹柔]亮', haircare.data$skuname), 'Shine',
                                                                               ifelse(grepl('丰盈|胶原蛋白|氨基酸|维他命|丝质蛋白', haircare.data$skuname), 'ShapeControl',
                                                                                      ifelse(grepl('垂坠|垂顺|卷发|柔顺|顺滑', haircare.data$skuname), 'ShapeCreation',
                                                                                             ifelse(grepl('香[水氛芬熏]|花语|coco|[留花芳熏]香|舒[缓眠]', tolower(haircare.data$skuname)), 'Perfume',
                                                                                                    ifelse(grepl('婴儿|宝宝', haircare.data$skuname), 'Baby', 'TBD')))))))))))
haircare.data$functionType = ifelse(is.na(haircare.data$Function), haircare.data$function.temp, haircare.data$Function)

# 算gender #
haircare.data$gender = ifelse(haircare.data$functionType == 'Baby', 'baby', 
                              ifelse(grepl('男', haircare.data$skuname) & !grepl('女', haircare.data$skuname), 'male', 
                                     ifelse(grepl('女', haircare.data$skuname) & !grepl('男', haircare.data$skuname), 'female', 'unisex')))

# top3 sizes #
size.list = strsplit(haircare.data$newSize, split = ' ')
size.list = lapply(size.list, function(x) {x = as.numeric(x); x = x[order(-x)]; return(x)})
haircare.data$size1 = sapply(size.list, function(x) x[1])
haircare.data$size2 = sapply(size.list, function(x) x[2])
haircare.data$size3 = sapply(size.list, function(x) x[3])

# 算newSize_2 #
cha_split = strsplit(as.character(haircare.data$newSize), split = ' ')
cha_split = sapply(cha_split, function(x) max(as.numeric(x)))
haircare.data$newSize_2 = cha_split
haircare.data$newSize_2[is.infinite(haircare.data$newSize_2)] = ''

write.csv(haircare.data,'haircare.data.temp.csv',row.names = F,fileEncoding = 'UTF-8')
}

{
##### --> 用上个月的productType和functionType捞底 #####
# 第一个月请注释掉
olddata = fread(paste0('/data1/trident/hongwei/project/haircare/output/',data.time.last,'/','haircare.',data.time.last,'.AttributeTable.csv'), header = T, stringsAsFactors = FALSE,colClasses = 'Character')
# olddata = as.data.table(olddata)
# haircare.data = as.data.table(haircare.data)
setDT(haircare.data)

# colnames(olddata)[colnames(olddata) == 'spuId']='spuid'##改列名
# colnames(olddata)[colnames(olddata) == 'skuId']='skuid'##改列名
# colnames(olddata)[colnames(olddata) == 'site']='Site'##改列名
# colnames(olddata)[colnames(olddata) == 'productType']='V.productType'##改列名
# colnames(olddata)[colnames(olddata) == 'featureType']='V.functionType'##改列名

setnames(olddata,c('spuId','skuId','site','productType','featureType'),c('spuid','skuid','Site','V.productType','V.functionType'))


output = olddata[, c('skuid','spuid','Site','V.productType','V.functionType'),with=F]

haircare.data = left_join(haircare.data,output,by = c("skuid","spuid","Site"))

haircare.data$productType = ifelse(haircare.data$productType == 'TBD' & !is.na(haircare.data$V.productType), haircare.data$V.productType, haircare.data$productType)
haircare.data$functionType = ifelse(haircare.data$functionType == 'TBD' & !is.na(haircare.data$V.functionType), haircare.data$V.functionType, haircare.data$functionType)
haircare.data = haircare.data[, setdiff(colnames(haircare.data), c('V.productType', 'V.functionType'))]

# 套装、染发、烫发、造型的function为TBD #
haircare.data$functionType = ifelse(haircare.data$productType %chin% c('套装', '染发产品', '烫发产品', '造型产品'), 'TBD', haircare.data$functionType)

# 算洗发水bounded #
key.boundedv2='([二两三四五六七八九十][支|只|瓶|件|套|盒|件])|([2-9][支|瓶|件|只|套|盒|件])|((10|11|12|13|14|15)[支|只|瓶|件|套|盒|件])|(洗发.*[+*xX×加])|(洗头.*[+*xX×加])|(洗发.*组合)|(送|赠)'
key.boundedv3='(洗头椅)|(洗头勺)|(洗头杯)|(单瓶)|(一瓶装)|(单支)|([*xX×]1)|(增加)|(无添加)|(套装之)|(1只)|(一只)|(日本直送)|(赠品)|(京东配送)'

haircare.data$bounded.promotion=ifelse(haircare.data$productType == '洗发产品'
                                       & grepl(key.boundedv2, haircare.data$skuname)
                                       & !grepl(key.boundedv3, haircare.data$skuname),'bounded.shampoo', NA)

write.csv(haircare.data, paste0('haircare.all.',data.time,'.finaloutput.csv'), row.names = F, fileEncoding = 'gb18030')

# 去重排序
#T1=read.csv(paste0('haircare.all.',data.time,'.finaloutput.csv'), header=T, colClasses = 'character', fileEncoding = 'gb18030') ##先将nskuid去除再去重
T1 = haircare.data
T1=T1[, setdiff(colnames(T1), 'nskuid')]
T1=unique(T1)       ##去重,再给三个平台加nskuid
#head(T1)
table(T1$Site)
T1 = T1[order(T1$Site), ]
#head(T1)
#sequence(3:5)
#test = sequence(unlist(table(T1$Site)))
#head(test)
#table(test)
T1$nskuid = sequence(unlist(table(T1$Site)))
#View(`T1`)
write.csv(T1, paste0('unique_haircare.all.',data.time,'.finaloutput.csv'), row.names = F, fileEncoding = 'gb18030')


}
##### (6) 出属性表 #####

rawdata = T1
cat('\n 生成属性表\n') 
## Price tier各层级名称
tier1 = "Economy"
tier2 = "Mass"
tier3 = "P"
tier4 = "SP-2"
tier5 = "SP-1"
tier6 = "Masstige"

## 按顺序贴对应列 
attribute.table = data.frame(dateVersion = rawdata$Month, 
                             site = rawdata$Site,
                             skuName = rawdata$skuname, 
                             skuUrl = rawdata$skuurl, 
                             spuId = rawdata$spuid, 
                             skuId = rawdata$skuid,
                             imported = rawdata$is_global,
                             productType = rawdata$productType, 
                             factory.matched = rawdata$factory.matched, 
                             brand.matched = rawdata$brand.matched,
                             sku.matched = rawdata$spu.matched, 
                             featureType = rawdata$functionType, 
                             size = rawdata$newSize,
                             netVolume = rawdata$newVolumn,
                             originalPrice = rawdata$yuanjia,
                             salesPrice = rawdata$salesprice,
                             Variant = rawdata$Variant,
                             bundle = rawdata$bounded.promotion)



## 计算uniprice
attribute.table$netVolume = as.numeric(attribute.table$netVolume)
temp = data.frame(unitPrice.original = as.numeric(attribute.table$originalPrice)/attribute.table$netVolume,
                  unitPrice.sale = as.numeric(attribute.table$salesPrice)/attribute.table$netVolume)
attribute.table = cbind(attribute.table, temp)
attribute.table$unitPrice.original[is.infinite(attribute.table$unitPrice.original)] = NA
attribute.table$unitPrice.sale[is.infinite(attribute.table$unitPrice.sale)] = NA
attribute.table$unitPrice.original = as.numeric(attribute.table$unitPrice.original)
# attribute.table$uniprice.original = ifelse(attribute.table$uniprice.original < quantile(attribute.table$uniprice.original, 0.01, na.rm = T)
#                                            |attribute.table$uniprice.original > quantile(attribute.table$uniprice.original, 0.99, na.rm = T), 
#                                            NA, attribute.table$uniprice.original)
attribute.table$unitPrice.original = round(attribute.table$unitPrice.original, digits = 3)
attribute.table$unitPrice.sale = as.numeric(attribute.table$unitPrice.sale)
# attribute.table$uniprice.sale = ifelse(attribute.table$uniprice.sale < quantile(attribute.table$uniprice.sale, 0.01, na.rm = T)
#                                        |attribute.table$uniprice.sale > quantile(attribute.table$uniprice.sale, 0.99, na.rm = T), 
#                                        NA, attribute.table$uniprice.sale)
attribute.table$unitPrice.sale = round(attribute.table$unitPrice.sale, digits = 3)


## 计算price tier
temp = data.frame(API.OriginalPrice = attribute.table$unitPrice.original/0.091*100,
                  API.salesPrice = attribute.table$unitPrice.sale/0.091*100)
attribute.table = cbind(attribute.table, temp)
temp = data.frame(priceTier = ifelse(attribute.table$API.OriginalPrice <= 80, tier1,
                                     ifelse(attribute.table$API.OriginalPrice <= 120, tier2,
                                            ifelse(attribute.table$API.OriginalPrice <= 150, tier3,
                                                   ifelse(attribute.table$API.OriginalPrice <= 180, tier4,
                                                          ifelse(attribute.table$API.OriginalPrice <= 200, tier5, tier6))))))                                                                                   
attribute.table = cbind(attribute.table, temp)

## 按顺序贴对应列
temp = data.frame(priceTier.unified = NA,
                  salesCount = rawdata$salescount,
                  commentCount = rawdata$koubei, 
                  gift = rawdata$gift,
                  promotionType= rawdata$promotionType,
                  gender = rawdata$gender,
                  bundleType = rawdata$setType,
                  size1 = rawdata$size1,
                  size2 = rawdata$size2,
                  size3 = rawdata$size3)
attribute.table = cbind(attribute.table, temp)

# 算中位数 #
Median.API.originalPrice = tapply(attribute.table$API.OriginalPrice, attribute.table$Variant, function(x) median(x, na.rm = T))
Median.API.originalPrice = data.frame(Variant = names(Median.API.originalPrice), Median.API.originalPrice)

Median.API.salesPrice = tapply(attribute.table$API.salesPrice, attribute.table$Variant, function(x) median(x, na.rm = T))
Median.API.salesPrice = data.frame(Variant = names(Median.API.salesPrice), Median.API.salesPrice)

attribute.table = join(attribute.table, Median.API.originalPrice, by = "Variant", type = "left")
attribute.table = join(attribute.table, Median.API.salesPrice, by = "Variant", type = "left")

attribute.table$priceTier.variant.original = ifelse(attribute.table$Median.API.originalPrice <= 80, tier1,
                                                    ifelse(attribute.table$Median.API.originalPrice <= 120, tier2,
                                                           ifelse(attribute.table$Median.API.originalPrice <= 150, tier3,
                                                                  ifelse(attribute.table$Median.API.originalPrice <= 180, tier4,
                                                                         ifelse(attribute.table$Median.API.originalPrice <= 200, tier5, tier6)))))

attribute.table$priceTier.variant.sales = ifelse(attribute.table$Median.API.salesPrice <= 80, tier1,
                                                 ifelse(attribute.table$Median.API.salesPrice <= 120, tier2,
                                                        ifelse(attribute.table$Median.API.salesPrice <= 150, tier3,
                                                               ifelse(attribute.table$Median.API.salesPrice <= 180, tier4,
                                                                      ifelse(attribute.table$Median.API.salesPrice <= 200, tier5, tier6)))))

##### 算中位数(新增产品维度) #####
attribute.table$new.vaiant.product = paste(attribute.table$Variant, attribute.table$productType, sep = "_")

Median.ProductType.API.originalPrice = tapply(attribute.table$API.OriginalPrice, attribute.table$new.vaiant.product, function(x) median(x, na.rm = T))
Median.ProductType.API.originalPrice = data.frame(new.vaiant.product = names(Median.ProductType.API.originalPrice), Median.ProductType.API.originalPrice)

Median.ProductType.API.salesPrice = tapply(attribute.table$API.salesPrice, attribute.table$new.vaiant.product, function(x) median(x, na.rm = T))
Median.ProductType.API.salesPrice = data.frame(new.vaiant.product = names(Median.ProductType.API.salesPrice), Median.ProductType.API.salesPrice)

attribute.table = join(attribute.table, Median.ProductType.API.originalPrice, by = "new.vaiant.product", type = "left")
attribute.table = join(attribute.table, Median.ProductType.API.salesPrice, by = "new.vaiant.product", type = "left")

attribute.table.P1 = attribute.table[attribute.table$productType == "洗发产品", ]
attribute.table.P1$PriceTier.ProductType.Original = ifelse(attribute.table.P1$Median.ProductType.API.originalPrice <= 80, tier1,
                                                           ifelse(attribute.table.P1$Median.ProductType.API.originalPrice <= 120, tier2,
                                                                  ifelse(attribute.table.P1$Median.ProductType.API.originalPrice <= 150, tier3,
                                                                         ifelse(attribute.table.P1$Median.ProductType.API.originalPrice <= 180, tier4,
                                                                                ifelse(attribute.table.P1$Median.ProductType.API.originalPrice <= 200, tier5, tier6)))))
attribute.table.P1$PriceTier.ProductType.Salesprice = ifelse(attribute.table.P1$Median.ProductType.API.salesPrice <= 80, tier1,
                                                             ifelse(attribute.table.P1$Median.ProductType.API.salesPrice <= 120, tier2,
                                                                    ifelse(attribute.table.P1$Median.ProductType.API.salesPrice <= 150, tier3,
                                                                           ifelse(attribute.table.P1$Median.ProductType.API.salesPrice <= 180, tier4,
                                                                                  ifelse(attribute.table.P1$Median.ProductType.API.salesPrice <= 200, tier5, tier6)))))


attribute.table.P2 = attribute.table[attribute.table$productType == "水洗护发素" | attribute.table$productType == "免洗护发素" | attribute.table$productType == "水洗发膜" | attribute.table$productType == "免洗发膜", ]
attribute.table.P2$PriceTier.ProductType.Original = ifelse(attribute.table.P2$Median.ProductType.API.originalPrice <= 80, tier1,
                                                           ifelse(attribute.table.P2$Median.ProductType.API.originalPrice <= 150, tier2,
                                                                  ifelse(attribute.table.P2$Median.ProductType.API.originalPrice <= 200, tier3,
                                                                         ifelse(attribute.table.P2$Median.ProductType.API.originalPrice <= 250, tier4,
                                                                                ifelse(attribute.table.P2$Median.ProductType.API.originalPrice <= 300, tier5, tier6)))))
attribute.table.P2$PriceTier.ProductType.Salesprice = ifelse(attribute.table.P2$Median.ProductType.API.salesPrice <= 80, tier1,
                                                             ifelse(attribute.table.P2$Median.ProductType.API.salesPrice <= 150, tier2,
                                                                    ifelse(attribute.table.P2$Median.ProductType.API.salesPrice <= 200, tier3,
                                                                           ifelse(attribute.table.P2$Median.ProductType.API.salesPrice <= 250, tier4,
                                                                                  ifelse(attribute.table.P2$Median.ProductType.API.salesPrice <= 300, tier5, tier6)))))

attribute.table.P3 = attribute.table[attribute.table$productType != "洗发产品" & attribute.table$productType != "水洗护发素" & attribute.table$productType != "免洗护发素" & attribute.table$productType != "水洗发膜" & attribute.table$productType != "免洗发膜", ]
attribute.table.P3$PriceTier.ProductType.Original = "NA"
attribute.table.P3$PriceTier.ProductType.Salesprice = "NA"

attribute.table = rbind(attribute.table.P1, attribute.table.P2, attribute.table.P3)

# 输出
write.csv(attribute.table, paste('haircare', data.time, "AttributeTable.csv", sep = "."), row.names = F)
setwd('/data1/trident/hongwei/java/')
write.csv(attribute.table, paste('haircare', data.time, "AttributeTable.csv", sep = "."), row.names = F)
#####################################

library(glue)
# java config
java.config='storecode_path= storecode_v{sc}.txt
property_path = {input.file}
out_file_path= {output.file}
segment_sign=","
url_index={url.index}
id_index={id.index}
site_index={site.index}'

sc='3.1.4'
input.file = 'haircare.{data.time}.AttributeTable.csv' %>% glue()
output.file = 'haircare.{data.time}.AttributeTable.sc314.csv' %>% glue()
url.index = '4'
id.index = '2'#不改
site.index = '3'#不改

java.config.all = java.config %>% glue()

write_file(java.config.all,'/data1/trident/hongwei/java/config/config.properties')

# done



#### run storecode java####
system('cd /data1/trident/hongwei/java;
       java -cp idlinkurl.jar project.idlinkurl.StoreCodeGetIDMain')

#### done ####



##### 跑完storecode改一波 423 #####
setwd('/data1/trident/hongwei/java/')
sc <- 'sc314'
#data.time <- '201803'
data <- fread(paste('haircare',data.time,'AttributeTable',sc,'csv',sep='.'), header=T, stringsAsFactors = F, colClasses = 'character',showProgress = T)
colnames(data)[2] <- 'storecode.id'
colnames(data)[3] <- 'storecode.site'
#data <- as.data.table(data)
data <- unique(data, by=c('skuUrl','skuId'), fromLast = T)
temp <- data[is.na(data$storecode.id),]

final.path = paste0(output.path,'/final')
if(!dir.exists(final.path)){dir.create(final.path)}
setwd(final.path)

write.csv(data,paste('hair',data.time,'AttributeTable',sc,'gb18030','csv',sep='.'),row.names = F,fileEncoding = 'gb18030',quote=F)
# write.csv(data,paste('hair',data.time,'AttributeTable',sc,'new','csv',sep='.'),row.names = F,fileEncoding = 'gb18030')
write.csv(data,paste('hair',data.time,'AttributeTable',sc,'utf8','csv',sep='.'),row.names = F,fileEncoding = 'UTF-8',quote = F)


#### 检查hair的size有没有 ####


##### 上传ftp #####
# lftp jiuqian2:meritco\@0531@publicftp.meritco-group.com
# cd /files2/data_sxb/Project.cutomized.version/Unilever_Xiaoli.hong&Stephanie.yang_201611/201712/
# put /data1/trident/liuzhi/data/haircare/output/201712/hair.201712.AttributeTable.sc306.csv
command = "lftp jiuqian2:Meritco\\!0128@publicftp.meritco-group.com:2121 -e 'set verify-certificate false;cd /files2/data_sxb/Project.cutomized.version/Unilever_Xiaoli.hong\\&Stephanie.yang_201611/;mkdir {data.time};cd {data.time};put /data1/trident/hongwei/project/haircare/output/{data.time}/final/hair.{data.time}.AttributeTable.{sc}.gb18030.csv;put /data1/trident/hongwei/project/haircare/output/{data.time}/final/hair.{data.time}.AttributeTable.{sc}.UTF-8.csv;ls -al'" %>% glue
system(command)

