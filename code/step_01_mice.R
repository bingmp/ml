if (!require("tidyverse")) installed.packages("tidyverse")
if (!require("dplyr")) installed.packages("dplyr")
if (!require("magrittr")) installed.packages("magrittr")
if (!require("mice")) installed.packages("mice")

rm(list = ls())
df_fct <- readxl::read_excel("./rawData/bpd_final.xlsx", 1) %>%
  as_tibble() %>%
  mutate(across(.cols = -c(id, gender, ethnicity, province), .fns = as.numeric)) %>%
  mutate(across(.cols = -id, .fns = as.factor))

df_num <- merge(readxl::read_excel("./rawData/bpd_final.xlsx", 2),
  readxl::read_excel("./rawData/bpd_final.xlsx", 3),
  by = "id"
) %>%
  select(id, name, everything()) %>%
  mutate(across(.cols = -c(id, name), .fns = as.numeric))

## bpd
# df0 <- merge(df_fct, df_num, by = "id") %>%
#   rename_all(tolower) %>%
#   select(id, name,severity_of_bpd, everything())
# df1 <- df0[!is.na(df0$severity_of_bpd),]

## ph
df1 <- merge(df_fct, df_num, by = "id") %>%
  rename_all(tolower) %>%
  select(id, name, severity_of_bpd, everything())

# df_over_10_na <- df1[df1 %>% apply(1, FUN = function(x){is.na(x) %>% sum()}) <= 10,]
# write.csv(df_over_10_na,file = 'csv/over_10_na.csv',row.names = F,fileEncoding = "GB18030")

df1 <- df1[df1 %>% apply(1, FUN = function(x) {
  is.na(x) %>% sum()
}) <= 10, ]
# write.csv(df1,file = 'csv/filter_10_na.csv',row.names = F,fileEncoding = "GB18030")

## 缺失值情况
# is.na(df1) %>% sum()
# is.na(df1[,45:47]) %>% sum()
# is.na(df1) %>% mean()
# is.na(df1[,45:47]) %>% mean()


imp_data <- mice(df1[, -(1:2)], # 数据集
  method = "rf", # 采用随机森林插补
  m = 5, # 5次插补
  printFlag = FALSE # 不显示历史记录
)
imp_data

# imp_data$imp
# imp_data$method
# stripplot(imp_data,pda~.imp,pch=20,cex=2)#查看bmi插补效果
# stripplot(imp_data)
# densityplot(imp_data,~pda_maximum_diameter ) # 插值完成 bmi密度图
# densityplot(imp_data)

# df_mice_na <- complete(imp_data, action = 2) %>%
#   select(severity_of_bpd, everything())
df_mice_na <- complete(imp_data, action = 2) %>%
  select(severity_of_ph, everything())


colnames(df_mice_na)[1] <- "group"

# df_mice_na$group <- recode(df_mice_na$group, '0' = 'Mild', '1' = 'Severe') # bpd
df_mice_na$group <- recode(df_mice_na$group,
  "0" = "Mild", "1" = "Mild",
  "2" = "Severe", "3" = "Severe"
) ## ph

table(df_mice_na$vsd_shunting)
df_mice_na$vsd_shunting <- recode(df_mice_na$vsd_shunting,
                           "0" = "0", "1" = "1",
                           "3" = "2", "4" = "3",'5'='3'
)

# saveRDS(df_mice_na,file = 'rds/df_mice_na.RDS')
# saveRDS(df_mice_na,file = 'rds/df_mice_na_ph.RDS')

# write.csv(df_mice_na,file = 'csv/df_mice_na.csv',row.names = F,fileEncoding = "GB18030")
