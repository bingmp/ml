if (!require("tidyverse")) install.packages("tidyverse")
if (!require(mlr3verse)) install.packages("mlr3verse")
if (!require(ranger)) install.packages("ranger")
if (!require(kknn)) install.packages("kknn")
if (!require(ggsci)) install.packages("ggsci")
showtext::showtext_auto()

rm(list = ls())
# 1 数据 ------------

## 1.1 读取数据 --------
# var <- readRDS("./rds/step_03_mlr3_var.RDS")
var <- readRDS("./rds/step_03_mlr3_var_ph.RDS")

# df_all <- readRDS("./rds/df_mice_na.RDS") %>% subset(select=var)
df_all <- readRDS("./rds/df_mice_na_ph.RDS") %>% subset(select=var)

## 1.2 确定任务 --------
task <- as_task_classif(df_all, target = "group")
# task$select(cols = var) # task
# autoplot(task, type = "target")


# 2 机器学习---------------
## 2.1 图学习器 -------------
graph_preb <- po("filter",
                 filter = mlr3filters::flt("find_correlation"), filter.cutoff = 0.3
) %>>% # 去除高度相关的列
  po("scale", center =T, scale = F) %>>% # 中心化
  po("removeconstants") %>>% # 去掉零方差变量
  po("encode") %>>%
  po("imputemedian")

## 2.2 学习算法 ------------
if (T) {
  # 随机森林
  rf_glr <- as_learner(graph_preb %>>% lrn("classif.ranger", predict_type = "prob"))
  rf_glr$id <- "randomForest"
  
  # 逻辑回归
  log_glr <- as_learner(graph_preb %>>% lrn("classif.log_reg", predict_type = "prob"))
  log_glr$id <- "logistic"
  
  # 决策树
  tree_glr <- as_learner(graph_preb %>>% lrn("classif.rpart", predict_type = "prob"))
  tree_glr$id <- "decisionTree"
  
  # k近邻
  kknn_glr <- as_learner(graph_preb %>>% lrn("classif.kknn", predict_type = "prob"))
  kknn_glr$id <- "kknn"
}

## 2.3 数据划分------------------
# 75% 训练集
set.seed(1)
split_task <- partition(task, ratio = 0.75)

task_train <- task$clone()$filter(split_task$train)
task_test <- task$clone()$filter(split_task$test)

# 3 训练模型 ----------------

## 3.1 建立多个模型 ----------------
learners <- list(rf_glr, log_glr, tree_glr, kknn_glr)

cv10 <- rsmp("cv", folds = 5)

if (F) {
  # 加速
  library(future)
  plan("multisession", workers = 24)
  
  # 减少屏幕输出
  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")
}

design <- benchmark_grid(tasks = task, learners = learners, resampling = cv10)
# design

bmr <- benchmark(design) # 执行基准测试
# saveRDS(bmr,file = "rds/step_04_bmr_bpd.rds")
# saveRDS(bmr,file = "rds/step_04_bmr_ph.rds")

## 3.2 可视化: 对比性能 ----------------
autoplot(bmr, type = "roc") + # ROC 曲线
  ggtitle("")+
  labs(fill = "Learner", color = "Learner") +
  ggsci::scale_color_aaas()+
  ggsci::scale_fill_aaas()+
  theme_classic()+
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 10)
  ) 

# ggsave(filename = 'result/bpd/step_04_learner.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
ggsave(filename = 'result/ph/step_04_learner.pdf',width = 28,height = 16,dpi = 300,units = 'cm')

autoplot(bmr, measure = msr("classif.auc")) + # AUC 箱线图
  theme(
    text = element_text(size = 0),
    plot.title = element_text(size = 30, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 20),
    legend.text = element_text(size =15)
  ) +
  ggtitle("AUC of Learner") +
  ylab("AUC")

# ggsave(filename = 'result/bpd/step_04_auc.pdf',width = 20,height = 10,dpi = 300,units = 'cm')
ggsave(filename = 'result/ph/step_04_auc.pdf',width = 20,height = 10,dpi = 300,units = 'cm')

# bmr$aggregate() # 默认输出
bmr_msrs <- c("classif.acc", "classif.auc", "classif.npv") %>%
  msrs() %>% bmr$aggregate() %>% 
  subset(select=c('learner_id', # 'resampling_id', 'iters', 
                  'classif.acc', 'classif.auc', 'classif.npv'))

# write_excel_csv(bmr_msrs,file = 'result/bpd/step_04_bmr_msrs.csv')
write_excel_csv(bmr_msrs,file = 'result/ph/step_04_bmr_msrs.csv')

## 4 选择最好的模型用于测试集 ---------------

# 4.1 训练 ---------------
rf_glr$train(task, row_ids = split_task$train)

if (F) {
  train_pred <- rf_glr$predict(task, row_ids = split_task$train)
  
  autoplot(train_pred, type = "roc") +
    ggtitle("ROC of tarin Data") +
    theme(
      text = element_text(size = 0),
      plot.title = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    # ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  # ggsave(filename = 'result/bpd/step_04_mlr3_train_roc.pdf',width = 12,height = 10,dpi = 300,units = 'cm')
  ggsave(filename = 'result/ph/step_04_mlr3_train_roc.pdf',width = 12,height = 10,dpi = 300,units = 'cm')
  
}

# 4.2 测试 ---------------
test_pred <- rf_glr$predict(task_test)
# test_pred %>% as.data.table() %>% head()
# rf_glr$model

test_pred$confusion

# 4.3 可视化 ---------------
autoplot(test_pred) +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20)
  ) +
  ggsci::scale_color_aaas() +
  ggsci::scale_fill_aaas()

ggsave(filename = 'result/ph/step_04_mlr3_hist_test.png',width = 10,height = 10,dpi = 300,units = 'cm')

autoplot(test_pred, type = "roc") +
  ggtitle("ROC of Test Data") +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20)
  ) +
  # ggsci::scale_color_aaas()+
  ggsci::scale_fill_aaas()

# ggsave(filename = 'result/bpd/step_04_mlr3_test_auc.pdf',width = 12,height = 10,dpi = 300,units = 'cm')
ggsave(filename = 'result/ph/step_04_mlr3_test_auc.pdf',width = 12,height = 10,dpi = 300,units = 'cm')

test_pred$score(msrs(c("classif.auc", "classif.acc", "classif.npv")))
