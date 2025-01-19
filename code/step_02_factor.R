if (!require("tidyverse")) install.packages("tidyverse")

rm(list = ls())

# df0 <- readRDS("./rds/df_mice_na.RDS") # bpd
df0 <- readRDS("./rds/df_mice_na_ph.RDS") # ph

# 选择 分组 与 连续变量
df_fct <- df0 %>% select_if(is.factor)

tab_fct_fun <- function(data, group = "group") {
  setdiff(colnames(data), group) %>% # 选择除分组外的变量名
    lapply(function(x) {
      tab_df <- table(data[, c(x, group)]) %>% as.data.frame.array() # 分组计数 数据框
      
      # 所有 变量名 及 分组计数
      tab_count <- c(
        vars = x, vars_group = paste0(rownames(tab_df), collapse = ", "), # 变量名
        apply(tab_df, 2, function(y) {
          paste0(y, collapse = ", ")
        }) # 分组计数
      )
      
      # 自动选择 chisq 或 fisher 方法进行差异检验
      tryCatch(
        {
          # 正常情况，使用 chisq 检验
          c(tab_count, tab_df %>% rstatix::chisq_test())
        },
        warning = function(w) {
          # 出现 warning 状态时，使用 fisher 检验
          c(tab_count, method = "Fisher test", tab_df %>% rstatix::fisher_test(simulate.p.value=TRUE))
        }
      )
    }) %>%
    bind_rows() %>% # 列表按行整合为数据框
    return()
}

tab_fct <- tab_fct_fun(data = df_fct, group = "group")
# write.csv(tab_fct, file = "result/step_02_tab_fct.csv", fileEncoding = "GB18030", row.names = F)
# write.csv(tab_fct, file = "result/ph/step_02_tab_fct.csv", fileEncoding = "GB18030", row.names = F)

