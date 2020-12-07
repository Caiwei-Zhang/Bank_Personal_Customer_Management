library(data.table)
library(tidyverse)
library(mlr)


# read data
path <- "E:/实习/信用卡管理/Task 2/data"
filepath <- list.files(path = path, pattern = ".csv", full.names = TRUE)
filename <- gsub(pattern = ".csv", replacement = "", 
                 list.files(path = path, pattern = ".csv"))
  
for (i in 1:length(filepath)) {
  assign(filename[i], fread(input = filepath[i]) %>% as_tibble())
}


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
  sink("E:/实习/信用卡管理/Task 2/summary_cate_trans.txt", append = TRUE)
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
          file = "E:/实习/信用卡管理/Task 2/summary_cont_trans.csv", 
          row.names = FALSE)




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
  sink("E:/实习/信用卡管理/Task 2/summary_cate.txt", append = TRUE)
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
            file = "E:/实习/信用卡管理/Task 2/summary_cont.csv", 
            row.names = FALSE)


########################################################################
######################## Q2: account_id match ##########################
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
########################### Q3: 异常值检测  ############################
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