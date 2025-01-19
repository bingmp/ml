# 二、连续变量 基本分布 -----------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("reshape2")) install.packages("reshape2")
if (!require("rstatix")) install.packages("rstatix")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("ggsci")) install.packages("ggsci")

rm(list = ls())
# df0 <- readRDS("./rds/df_mice_na.RDS") # bpd
df0 <- readRDS("./rds/df_mice_na_ph.RDS") # ph

# 选择 分组 与 连续变量
df_num <- tibble(
  group = df0$group,
  df0 %>% select_if(is.numeric)
)

## 2.1 基本统计 -------------

# 宽数据变长数据
df_num_long <- reshape2::melt(df_num,
                              id.vars = "group", # 需保留的不参与聚合的变量列名
                              measure.vars = colnames(df_num)[-1], # 需要聚合的变量s1-s10
                              variable.name = c("vars"), # 聚合变量的新列名
                              value.name = "value" # 聚合值的新列名
)

# 连续变量 统计信息
tab_num <- df_num_long %>%
  group_by(group, vars) %>%
  dplyr::summarize(
    mean = mean(value),
    sd = sd(value),
    max = max(value),
    p75 = quantile(value, probs = c(0.75)),
    median = median(value),
    p25 = quantile(value, probs = c(0.25)),
    min = min(value),
    .groups = "drop_last"
  )
# 保留小数位数
tab_num[, -c(1:2)] <- tab_num[, -c(1:2)] %>% apply(2, round, digits = 3)

tab_num
# write_excel_csv(tab_num,file = 'result/bpd/step_02_tab_num.csv')
# write_excel_csv(tab_num,file = 'result/ph/step_02_tab_num.csv')

## 2.2 分组箱图 -------------


# 连续变量箱图
# 分组 t检验
tab_t_test <- reshape2::melt(df_num_long) %>%
  group_by(vars) %>%
  # rstatix::t_test(value ~ group, paired = F) %>% # t 检验
  rstatix::wilcox_test(value ~ group) %>% # 秩和检验
  rstatix::add_significance("p") %>%
  # rstatix::adjust_pvalue(method = "fdr") %>% # 矫正 p 值
  # rstatix::add_significance("p.adj") %>%
  rstatix::add_xy_position(x = "group")

tab_t_test %>% head()
# write_excel_csv(tab_t_test,file = 'result/bpd/step_02_tab_number_test.csv')
# write_excel_csv(tab_t_test,file = 'result/ph/step_02_tab_number_test.csv')


# ggplot 画图设置
mytheme <- list(
  ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = element_text(size = 30, hjust = 0.5),
      axis.title = element_text(size = 20),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15)
    ),
  ggsci::scale_color_lancet(),
  ggsci::scale_fill_lancet()
)

# 箱图
ggpubr::ggboxplot(df_num_long,
                  x = "group", y = "value", color = "group",
                  palette = "jco", facet.by = "vars"
) +
  ggpubr::stat_pvalue_manual(tab_t_test, label = "{p.signif}") +
  # ggpubr::stat_pvalue_manual(tab_t_test, label = "{p.adj.signif}") +
  # labs(fill = "BPD", color = "BPD") +
  labs(fill = "BPD-PH", color = "BPD-PH") +
  xlab("") + ylab("") + mytheme

# ggsave(filename = 'result/bpd/step_02_number.pdf',width = 32,height = 18, units = 'cm',dpi = 300)
# ggsave(filename = 'result/ph/step_02_number.pdf',width = 32,height = 18, units = 'cm',dpi = 300)

# 连续变量标准化后的箱图
df_num_scale <- tibble(
  group = df_num$group,
  df_num %>% select_if(is.numeric) %>% apply(2, scale) %>% as_tibble() # 标准化
) %>%
  # 转换为长数据
  reshape2::melt(
    id.vars = "group", # 需保留的不参与聚合的变量列名
    measure.vars = colnames(df_num)[-1], # 需要聚合的变量s1-s10
    variable.name = c("vars"), # 聚合变量的新列名
    value.name = "value" # 聚合值的新列名
  )

tab_t_test_scale <- reshape2::melt(df_num_scale) %>%
  group_by(vars) %>%
  # rstatix::t_test(value ~ group, paired = F) %>% # t 检验
  rstatix::wilcox_test(value ~ group) %>% # 秩和检验
  rstatix::add_significance("p") %>%
  # rstatix::adjust_pvalue(method = "fdr") %>% # 矫正 p 值
  # rstatix::add_significance("p.adj") %>%
  rstatix::add_xy_position(x = "group")

# tab_t_test_scale %>% head()

# 箱图
ggpubr::ggboxplot(df_num_scale,
                  x = "group", y = "value", color = "group",
                  palette = "jco", facet.by = "vars"
) +
  ggpubr::stat_pvalue_manual(tab_t_test_scale, label = "{p.signif}") +
  # ggpubr::stat_pvalue_manual(tab_t_test_scale, label = "{p.adj.signif}") +
  # labs(fill = "BPD", color = "BPD") +
  labs(fill = "BPD-PH", color = "BPD-PH") +
  xlab("") + ylab("") + mytheme

# ggsave(filename = 'result/bpd/step_02_number_scale.pdf',width = 28,height = 16, units = 'cm',dpi = 300)
# ggsave(filename = 'result/ph/step_02_number_scale.pdf',width = 28,height = 16, units = 'cm',dpi = 300)

