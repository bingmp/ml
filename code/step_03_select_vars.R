if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
if (!require("corrplot")) install.packages("corrplot")

rm(list = ls())

## 3.1 remove high cor vars ----------------
# df_all <- readRDS("./rds/df_mice_na.RDS")
df_all <- readRDS("./rds/df_mice_na_ph.RDS")

# 相关系数矩阵
tab_cor <- df_all %>%
  select_if(is.numeric) %>%
  cor()

# df_all %>%  select_if(is.numeric) %>% cor() %>%  findCorrelation(cutoff = 0.75) 

# 相关性热图
plot.new()
# pdf("result/bpd/step_03_cor.pdf")
pdf("result/ph/step_03_cor.pdf")
tab_cor %>%
  corrplot::corrplot(order = "hclust", tl.col = "black")
dev.off()

# 去除高相关性变量 函数
rm_high_corr <- function(data, cutoff_value = 0.75) {
  tab_corr <- data %>%
    select_if(is.numeric)
  
  highCorr_col <- tab_corr %>%
    cor() %>%
    findCorrelation(cutoff = cutoff_value)
  if (length(highCorr_col) > 0) {
    tab_corr %>%
      subset(select = -highCorr_col) %>%
      return()
  } else {
    return(tab_corr)
  }
}

# 去除高相关性变量
df_all %>%
  rm_high_corr() %>%
  head()

# 查看每一列的偏度
# df_all %>% rm_high_corr() %>% apply(2, e1071::skewness)


## 3.2 分类变量筛选 ------------------------

### 3.2.1 randomForest 方法 -----------------
if (!require("randomForest")) install.packages("randomForest")
df_select <- cbind(
  df_all %>% rm_high_corr(),
  df_all %>% select_if(is.factor)
)

set.seed(1)
fit <- randomForest(group ~ ., data = df_select, importance = TRUE)


# pdf(file = "result/bpd/step_03_rf_error.pdf", width = 6, height = 4)
pdf(file = "result/ph/step_03_rf_error.pdf", width = 6, height = 4)
plot(fit, main = "randomForest error plot")
dev.off()

# which.min(fit$err.rate[, 1])
# out.importance <- round(randomForest::importance(fit), 2)

fit$importance

# pdf(file = "result/bpd/step_03_rf_importance.pdf", width = 12, height = 6.5)
pdf(file = "result/ph/step_03_rf_importance.pdf", width = 12, height = 6.5)
varImpPlot(fit, sort = T, main = "Variable Importance Plot")
dev.off()

if(F){
  res <- rfcv(
    trainx = df_select[, -1], trainy = df_select$group,
    cv.fold = 10,
    recursive = T
  )
  
  res$n.var # 变量个数
  
  res$error.cv # 错误率
  
  nvar <- which(res$error.cv == res$error.cv %>% min()) %>%
    names() %>%
    as.numeric() %>%
    min()
  
  with(res, plot(n.var, error.cv, type = "o", lwd = 2))
  
  var <- fit$importance[, 4] %>%
    sort(decreasing = T) %>%
    head(22) %>%
    names()
}

### 3.2.2 Boruta -----------------
if (!require("Boruta")) install.packages("Boruta")
set.seed(23)

fs <- Boruta(group ~ ., data = df_select, doTrace=1)
table(fs$finalDecision)
var <- getSelectedAttributes(fs)
var
# saveRDS(c("group",var),file = 'rds/step_03_mlr3_var.RDS')
# saveRDS(c("group",var),file = 'rds/step_03_mlr3_var_ph.RDS')

