library(data.table)
library(tidyverse)
library(mlr)


# read data
# 将路径指定为data所在的文件夹
sw <- setwd("..") 
path <- paste(sw, "data", sep = "/")
filepath <- list.files(path = path, pattern = ".csv", full.names = TRUE)
filename <- gsub(pattern = ".csv", replacement = "", 
                 list.files(path = path, pattern = ".csv"))
  
for (i in 1:length(filepath)) {
  assign(filename[i], fread(input = filepath[i]) %>% as_tibble())
}


########################################################################
####################### T2Q1: basic knowlwdge ##########################
########################################################################
# app_data
# ID应该是唯一的，经检查发现，有8个ID出现了2次，其中有5条数据是完全一样的，
# 其余3条在AMT_INCOME_TOTAL有差异，均是相差一个0,应该是数据录入时出现了问题。
# 查看重复值
dup_id <- app_data %>% group_by(ID) %>% mutate(count = n()) %>% filter(count > 1) %>% ungroup()
dup_id
dup_data <- dup_id[!duplicated(dup_id),]
# puzzle_idx <- which(duplicated(dup_data$ID,))

# 因为已经grouop过，所以每一个ID重复的数据都和它的上一条数据做对比
diff_col <- list()
for (i in which(duplicated(dup_data$ID,))) {
  diff_col <- append(diff_col, colnames(dup_data)[which(dup_data[i, ] != dup_data[(i-1), ])])
}

# 其余3条ID重复的数据均在在AMT_INCOME_TOTAL上有差异，查看发现都是少了一位0，判断为数据采集或者录入错误，删除重复数据。
app_data <- app_data[!duplicated(app_data$ID), ]

summary_app <- summarizeColumns(app_data)
cate_feature_app <- summary_app$name[summary_app$type == "character"]
FLAG <- colnames(app_data)[grep("FLAG", colnames(app_data))]
cate_feature_app <- union(cate_feature_app, FLAG)

for (fea in cate_feature_app) {
  sink(paste(sw, "summary_cate.txt", sep = "/"), append = TRUE)
  cat("The distribution of", fea, ": \n")
  count <- table(app_data[[fea]], useNA = "ifany")
  tab <- cbind(count = count, 
               freq  = round(count / nrow(app_data), 3))
  tab <- as.data.frame(tab)
  print(tab)
  cat("\n")
  sink()
}

cont_feature_app <- setdiff(colnames(app_data), c(cate_feature_app, "ID")) # exclude ID
summary_app[summary_app$name %in% cont_feature_app, ]
write.csv(summary_app[summary_app$name %in% cont_feature_app, ], 
            file = paste(sw, "summary_cont.csv", sep = "/"), 
            row.names = FALSE)

# trans
trans <- trans[!duplicated(trans), ]
nrow(trans)

summary_trans <- summarizeColumns(trans)
length(unique(trans$account_id)) 
# 4491个账户发起了1056214次交易
# 两个字段意义不明， account & bank 

# 观察分类变量的类别分布
cate_feature_trans <- summary_trans$name[summary_trans$type == "character"]

for (fea in cate_feature_trans) {
  sink(paste(sw, "summary_cont.txt", sep = "/"), append = TRUE)
  cat("The distribution of", fea, ": \n")
  count <- table(trans[[fea]], useNA = "ifany")
  tab <- cbind(count = count, 
               freq  = round(count / nrow(trans), 3))
  tab <- as.data.frame(tab)
  print(tab)
  cat("\n")
  sink()
}

cont_feature_trans <- setdiff(colnames(trans), c(cate_feature_trans, "trans_id", "account_id", "date")) 
summary_trans[summary_trans$name %in% cont_feature_trans, ]
write.csv(summary_trans[summary_trans$name %in% cont_feature_trans, ], 
          file = paste(sw, "summary_cont_trans.csv", sep = "/"), 
          row.names = FALSE)



########################################################################
####################### T2Q2: account_id match #########################
########################################################################
# 参照link_table做表格内连接
app_join_link <- inner_join(app_data, link_table, by = "ID") %>% select(ID, account_id, everything())

# 在客户表中出现且没有在交易表中出现的account_id,说明只有客户号，没有交易过。
app_no_trans <- setdiff(app_join_link$account_id,trans$account_id)
length(app_no_trans)

# 在交易表中出现但没有在申请表中出现的account_id,说明没有开账户就进行了交易。
trans_no_app <- setdiff(trans$account_id, app_join_link$account_id)
length(trans_no_app)

# 在两张表中都出现的account_id数量
trans_with_app <- intersect(trans$account_id, app_join_link$account_id)
length(trans_with_app)




########################################################################
########################### T2Q3: 异常值检测  ##########################
########################################################################
find_outliers <- function(data, fea) {
  
  data_mean  <- mean(data[[fea]])
  lower_rule <- data_mean - sd(data[[fea]]) * 3
  upper_rule <- data_mean + sd(data[[fea]]) * 3
  outliers_ind <- (data[[fea]] > upper_rule) | (data[[fea]] < lower_rule)

  return(which(outliers_ind))
}

# 针对app_data
app_cont_outlier <- list()
for (f in 1:length(cont_feature_app)) {
  app_cont_outlier[[f]] <- find_outliers(app_data, cont_feature_app[f])
} 
names(app_cont_outlier) <- cont_feature_app

## 理论上说CNT_CHILDREN多的，家庭成员数也要多，但是1632号小孩多家庭成员却不异常 
app_data$CNT_CHILDREN[app_cont_outlier[["CNT_CHILDREN"]]]
app_data$CNT_FAM_MEMBERS[app_cont_outlier[["CNT_FAM_MEMBERS"]]]
app_data$AMT_INCOME_TOTAL[app_cont_outlier[["AMT_INCOME_TOTAL"]]]


library(ggplot2)
# ggplot(data = app_data) +  geom_density(mapping = aes(x = AMT_INCOME_TOTAL)) 
ggplot(data = app_data) + 
  geom_boxplot(mapping = aes(x = NAME_INCOME_TYPE , y = AMT_INCOME_TOTAL), width=.2, 
               outlier.size = .8, outlier.alpha = 0.8) + 
  xlab("收入类型") +
  ylab("年总收入")

app_data %>% filter(AMT_INCOME_TOTAL != 6750000) %>% 
  group_by(NAME_INCOME_TYPE) %>% 
  mutate(AVG_INCOME = mean(AMT_INCOME_TOTAL)) %>% 
  mutate(MED_INCOME = median(AMT_INCOME_TOTAL)) %>% 
  select(NAME_INCOME_TYPE, AVG_INCOME, MED_INCOME) %>%
  unique()





# 针对trans
trans_cont_outlier <- list()
for (f in 1:length(cont_feature_trans)) {
  trans_cont_outlier[[f]] <- find_outliers(trans, cont_feature_trans[f])
} 
names(trans_cont_outlier) <- cont_feature_trans

## 理论上说CNT_CHILDREN多的，家庭成员数也要多，但是1632号小孩多家庭成员却不异常 
trans$amount_num[trans_cont_outlier[["amount_num"]]]
trans$balance_num[trans_cont_outlier[["balance_num"]]]


plot_data <- tibble(value = c(trans[["amount_num"]], trans[["balance_num"]]), var = c(rep("amount_num", nrow(trans)), 
                                                                             rep("balance_num", nrow(trans))))
ggplot(plot_data, aes(x = var, y = value)) + 
  geom_boxplot(outlier.size = 0.3) + xlab("continuous variable")

#操作金额为正数，集中在2000-5000


# 账户余额的一个明显的最大值点。查询该客户最大值发生的半月之间的交易数据。
trans[trans$account_id == trans[[which.max(trans$balance_num), "account_id"]], ] %>% 
  filter(date > as.Date("1997-05-01") & date < as.Date("1997-05-15")) %>% 
  select(-c(bank, account))


########################################################################
###################### T3Q1: RFM by operation ##########################
########################################################################
# 在进行模型建立前我们首先删除非正常交易的449个样本(只在交易表中出现而没有在客户表中出现)，
trans_obj <- trans[!trans$account_id %in% trans_no_app, ] %>% 
  filter(date > as.Date("1998-01-01") & date < as.Date("1998-06-30"))

# 查看一共有多少账户在统计周期内进行了多少次交易
sprintf("一共有 %d 账户在统计周期内进行了 %d 交易", 
        length(unique(trans_obj$account_id)), nrow(trans_obj))

### Q1. 每一个客户的某种交易的R，F，M指标
# 分别提取不同交易类型的数据
trans_rfm <- trans_obj %>% group_by(account_id, operation) %>% 
  mutate(recency = max(date)) %>% mutate(frequency = n()) %>%
  mutate(monetary = sum(amount_num)) %>%
  select(account_id, operation, recency, frequency, monetary)
trans_rfm <- trans_rfm[!duplicated(trans_rfm), ]

rTime <- trans_obj %>% group_by(operation) %>% mutate(refertime = max(date)) %>%
  select(operation, refertime)
rTime <- rTime[!duplicated(rTime), ]

trans_rfm <- inner_join(trans_rfm, rTime, by = "operation")  
trans_rfm$rInternal <- as.integer(as.Date(trans_rfm$recency) - as.Date(trans_rfm$refertime))
trans_rfm <- select(trans_rfm, -c(refertime))

# 对于6类交易类型，我们分别计算了相应的R，F，M指标，并保存在相应文件中。
operation <- unique(trans_obj$operation)
for (op in operation) {
  assign(paste0("trans_", op), trans_rfm[trans_rfm$operation == op, ])
}


# 定义一个绘图主题
mytheme <- theme(axis.text.x = element_text(angle = 30, vjust= 1, hjust = 1, size = 10),
                 axis.title.x = element_text(size=8),
                 axis.title.y = element_text(size=8),
                 panel.background = element_rect(fill = "white", color = "black"),
                 panel.grid.major.y = element_line(color = "grey", linetype= 1),
                 panel.grid.minor.y = element_line(color = "grey", linetype= 2),
                 panel.grid.minor.x = element_blank(), 
                 legend.position = "bottom")

## 绘制loan交易对应的recency, frequency, monetary的直方图，观察其分布
r <- ggplot(trans_loan, aes(rInternal)) +
  geom_histogram(bins = 10) + mytheme
f <- ggplot(trans_loan, aes(frequency)) +
  geom_histogram(bins = 10) + mytheme
m <- ggplot(trans_loan, aes(monetary)) +
  geom_histogram(bins = 10) + mytheme
gridExtra::grid.arrange(r, f, m, ncol = 3)


########################################################################
###################### T3Q2: set grade by RFM ##########################
########################################################################
### Q2. 对三个维度，分别划分等级，列明等级划分标准，说明等级划分的理由。
# 对于原始R,F,M的取值，大于平均数认为“高等级”，小于平均数认为“低等级”。
trans_loan$rankR <- ifelse(trans_loan$rInternal > mean(trans_loan$rInternal), "高", "低")
trans_loan$rankF <- ifelse(trans_loan$frequency > mean(trans_loan$frequency), "高", "低")
trans_loan$rankM <- ifelse(trans_loan$monetary > mean(trans_loan$monetary), "高", "低")

label <- vector("character", nrow(trans_loan))
trans_loan_lab <- within(trans_loan, {
  
  label[rankR == "高" & rankF == "高" & rankM == "高"] = "重要价值客户"
  label[rankR == "高" & rankF == "低" & rankM == "高"] = "重要发展客户"  
  label[rankR == "低" & rankF == "高" & rankM == "高"] = "重要保持客户"
  label[rankR == "低" & rankF == "低" & rankM == "高"] = "重要挽留客户"
  
  label[rankR == "高" & rankF == "高" & rankM == "低"] = "一般价值客户"
  label[rankR == "高" & rankF == "低" & rankM == "低"] = "一般发展客户"  
  label[rankR == "低" & rankF == "高" & rankM == "低"] = "一般保持客户"
  label[rankR == "低" & rankF == "低" & rankM == "低"] = "一般挽留客户"
  
})

trans_loan_lab %>% group_by(label) %>% 
  summarize(rmean = mean(rInternal), rmedian = median(rInternal), rsd = sd(rInternal),
            fmean = mean(frequency), fmedian = median(frequency), fsd = sd(frequency),
            mmean = mean(monetary),  mmedian = median(monetary),  msd = sd(monetary),
            .groups = "drop")


########################################################################
################### T3Q3: the number of applications ###################
########################################################################
### Q3. 计算每个单元用户数和占比情况：
loan_group <- cbind(count = table(trans_loan_lab$label), 
                    prop = round(table(trans_loan_lab$label) / nrow(trans_loan_lab), 4))
loan_group

# 绘制loan交易对应的客户分层条形图
ggplot(data = trans_loan_lab) + geom_bar(mapping = aes(x = label)) + 
  xlab("Customer stratification of loan operation") + mytheme

# 下面按照类似的操作，将其余5种交易类型的客户分层情况分别汇总

### define a function
getLabel <- function(data) {
  
  data$rankR <- ifelse(data$rInternal > mean(data$rInternal), "高", "低")
  data$rankF <- ifelse(data$frequency > mean(data$frequency), "高", "低")
  data$rankM <- ifelse(data$monetary  > mean(data$monetary), "高", "低")
  
  label <- vector("character", nrow(data))
  data_lab <- within(data, {
    
    label[rankR == "高" & rankF == "高" & rankM == "高"] = "重要价值客户"
    label[rankR == "高" & rankF == "低" & rankM == "高"] = "重要发展客户"  
    label[rankR == "低" & rankF == "高" & rankM == "高"] = "重要保持客户"
    label[rankR == "低" & rankF == "低" & rankM == "高"] = "重要挽留客户"
    
    label[rankR == "高" & rankF == "高" & rankM == "低"] = "一般价值客户"
    label[rankR == "高" & rankF == "低" & rankM == "低"] = "一般发展客户"  
    label[rankR == "低" & rankF == "高" & rankM == "低"] = "一般保持客户"
    label[rankR == "低" & rankF == "低" & rankM == "低"] = "一般挽留客户"
    
  })
  
  data_group <- cbind(count = table(data_lab$label), 
                      prop = round(table(data_lab$label) / nrow(data_lab), 4))
  return(data_group)
  
}

## cash
print("cash交易的客户分层表为：")
getLabel(trans_cash)

## remit
print("remit交易的客户分层表为：")
getLabel(trans_remit)

## collection
print("collection交易的客户分层表为：")
getLabel(trans_collection)

##cc_debit
print("debit交易的客户分层表为：")
getLabel(trans_cc_debit)

## unknown
print("未知交易的客户分层表为：")
getLabel(trans_)



### 两两交叉分析

# 针对loan交易R，F，M中的两两指标进行分析，观察其是否存在某种规律。绘制量两两指标对应的散点图：

# 交叉分析图
rm <- ggplot(trans_loan_lab,aes(monetary,rInternal)) +
  geom_point(shape = 21, fill = 'steelblue',size = 1)
fm <- ggplot(trans_loan_lab,aes(monetary,frequency)) +
  geom_point(shape = 21, fill = 'steelblue',size = 1)  
rf <- ggplot(trans_loan_lab,aes(frequency,rInternal)) +
  geom_point(shape = 21, fill = 'steelblue',size = 1)  
gridExtra::grid.arrange(rm,fm,rf, ncol=1)


## 附录
#' 这里给出其他的等级划分标准，以便对比不同等级划分标准下的分层结果。
#' 以loan交易为例进行分析，其余5种交易类型的等级划分结果可以类似给出。

#' 等级划分标准2: 采用分位数方法进行客户分层。根据分位数分别对最近一次交易距离天数rInternal，
#' 交易频次frequency，交易总金额monetary进行打分，记为新的R，F，M指标。
#' 具体来说，rInternal表示某客户最近一次交易日期距离参照日期的天数，为负值，因此rInternal越大，得分越高； 
#' frequency越大，得分越高；monetary越大，得分越高。

qr <- unique(quantile(trans_loan$rInternal))
qf <- unique(quantile(trans_loan$frequency))
qm <- unique(quantile(trans_loan$monetary))

trans_loan_rfm <- trans_loan %>% 
  mutate(Rece = as.integer(cut(rInternal, breaks = c(-Inf, qr, Inf), labels = 1:(length(qr)+1)))) %>%
  mutate(Freq = as.integer(cut(frequency, breaks = c(-Inf, qf, Inf), labels = 1:(length(qf)+1)))) %>%
  mutate(Mone = as.integer(cut(monetary, breaks = c(-Inf, qm, Inf), labels = 1:(length(qm)+1)))) %>%
  select(-c(rankR, rankF, rankM, operation))

# 根据R，F，M的得分对客户进行分类，高于平均值判定为“高”，低于平均值判定为“低”。
trans_loan_rfm$rankR <- ifelse(trans_loan_rfm$Rece > mean(trans_loan_rfm$Rece), "高", "低")
trans_loan_rfm$rankF <- ifelse(trans_loan_rfm$Freq > mean(trans_loan_rfm$Freq), "高", "低")
trans_loan_rfm$rankM <- ifelse(trans_loan_rfm$Mone > mean(trans_loan_rfm$Mone), "高", "低")

label <- vector("character", nrow(trans_loan_rfm))
rfm_lab <- within(trans_loan_rfm, {
  
  label[rankR == "高" & rankF == "高" & rankM == "高"] = "重要价值客户"
  label[rankR == "高" & rankF == "低" & rankM == "高"] = "重要发展客户"  
  label[rankR == "低" & rankF == "高" & rankM == "高"] = "重要保持客户"
  label[rankR == "低" & rankF == "低" & rankM == "高"] = "重要挽留客户"
  
  label[rankR == "高" & rankF == "高" & rankM == "低"] = "一般价值客户"
  label[rankR == "高" & rankF == "低" & rankM == "低"] = "一般发展客户"  
  label[rankR == "低" & rankF == "高" & rankM == "低"] = "一般保持客户"
  label[rankR == "低" & rankF == "低" & rankM == "低"] = "一般挽留客户"
  
})

rfm_lab


# 汇总统计各个客户组的人数和占比情况，绘制相应的条形图：
app_group <- cbind(count = table(rfm_lab$label), 
                   prop = round(table(rfm_lab$label) / nrow(rfm_lab), 4))

ggplot(data = rfm_lab) + geom_bar(mapping = aes(x = label)) + mytheme

# 汇总各个客户组的recency，frequency，monetary的均值和中位数
rfm_lab %>% group_by(label) %>% 
  summarize(rmean = mean(rInternal), rmedian = median(rInternal), rsd = sd(rInternal),
            fmean = mean(frequency), fmedian = median(frequency), fsd = sd(frequency),
            mmean = mean(monetary),  mmedian = median(monetary),  msd = sd(monetary),
            .groups = "drop")

#' 等级划分标准3：采用聚类方法进行客户分层。这里做一个简单的例子提供参考，由于数据本身比较简单，
#' 根据RFM三个指标的话使用均值或者分位数的分层方法就足够。进行聚类前的基本操作有,可选：
#' 1. R，F，M三个指标标准化;
#' 2. R，F，M三个指标加权; 
#' 3. 加权综合RFM指标;
#' 根据以上操作得到的新指标进行K-Means聚类并作结果分析。

# 首先标准化R，F，M三个指标，根据R，F，M三个指标对客户分层的重要程度加权得到新的RFM指标，
# 这里我们举例对R赋权0.2，对F赋权0.3，对M赋权0.5。
loan_model <- trans_loan[, c("rInternal", "frequency", "monetary")]
loan_new <- apply(loan_model, 2, scale) %>% as_tibble() %>% mutate(WZR = 0.2*rInternal) %>%
  mutate(WZF = 0.3*frequency) %>% mutate(WZM = 0.5*monetary) %>% select(WZR, WZF, WZM)

# 利用加权后的特征进行聚类
if(!require(factoextra)) {install.packages("factoextra"); library(factoextra)}

km_loan <- kmeans(loan_new, 8)
loan_clust <- tibble(loan_new, clust = km_loan$cluster)

##对比每类客户的RFM均值与总的RFM均值，大于总平均值记为“高”， 小于总平均值记为“低”
clust_center <- as_tibble(km_loan$centers)
clust_center <- clust_center %>% 
  mutate(rankR = ifelse(clust_center[["WZR"]] > apply(loan_new, 2, median)[1], "高", "低")) %>%
  mutate(rankF = ifelse(clust_center[["WZF"]] > apply(loan_new, 2, median)[2], "高", "低")) %>%
  mutate(rankM = ifelse(clust_center[["WZM"]] > apply(loan_new, 2, median)[3], "高", "低")) 

custom <- vector("character", length = 8)
clust_center <- within(clust_center, {
  custom[rankR == "高" & rankF == "高" & rankM == "高"] = "重要价值客户"
  custom[rankR == "高" & rankF == "低" & rankM == "高"] = "重要发展客户"  
  custom[rankR == "低" & rankF == "高" & rankM == "高"] = "重要保持客户"
  custom[rankR == "低" & rankF == "低" & rankM == "高"] = "重要挽留客户"
  
  custom[rankR == "高" & rankF == "高" & rankM == "低"] = "一般价值客户"
  custom[rankR == "高" & rankF == "低" & rankM == "低"] = "一般发展客户"  
  custom[rankR == "低" & rankF == "高" & rankM == "低"] = "一般保持客户"
  custom[rankR == "低" & rankF == "低" & rankM == "低"] = "一般挽留客户"
})

clust_refer <- tibble(clust = 1:8, custom = clust_center$custom)

loan_clust <- inner_join(loan_clust, clust_refer, by = "clust")

ggplot(data = loan_clust) +
  geom_bar(mapping=aes(x = custom)) + mytheme
