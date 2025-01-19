
if(!require("mlr3verse")) install.packages("mlr3verse")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")

rm(list = ls())

## 准备 -------------------
if(T){
  rm(list = ls())
  # 读取数据
  # var <- readRDS("./rds/step_03_mlr3_var.RDS")
  var <- readRDS("./rds/step_03_mlr3_var_ph.RDS")
  
  # df_all <- readRDS("./rds/df_mice_na.RDS") %>% subset(select=var)
  df_all <- readRDS("./rds/df_mice_na_ph.RDS") %>% subset(select=var)
  
  # colnames(df_all) <- make.names(names(df_all) )
  # 建立任务
  task <- as_task_classif(df_all, target="group")
  
  # 数据划分
  split_task <- partition(task, ratio=0.75)
  
  task_train <- task$clone()$filter(split_task$train)
  task_test <- task$clone()$filter(split_task$test)
  
  # 数据预处理
  graph_preb <- po("filter",
                   filter = mlr3filters::flt("find_correlation"),
                   filter.cutoff = 0.3
  ) %>>% # 去除高度相关的列
    po("scale", center =T, scale = F) %>>% # 中心化
    po("removeconstants") %>>% # 去掉零方差变量
    po("encode") %>>%
    po("imputemedian")
}

## 5.1 随机森林模型 -----------------
if(T){
  # 选择随机森林模型
  rf_glr <- as_learner(graph_preb %>>% lrn("classif.ranger", predict_type="prob"))
  rf_glr$id <- "randomForest"
  
  # 折交叉验证
  rr <- resample(task = task,
                 learner = rf_glr,
                 resampling = rsmp("cv",folds = 5),
                 store_models = T
  )
  
  
  # 先看看在训练集中的表现。
  # 混淆矩阵：
  rr$prediction()$confusion
  df_pred <- rr$prediction()
  
  # 查看其他结果：
  c("classif.auc","classif.acc","classif.npv","classif.bbrier") %>%
    msrs() %>%
    rr$aggregate() %>%
    as.data.frame()
  
  autoplot(rr,type = "roc") +
    ggtitle("Train Data of randomForest") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_rf_train_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # prc曲线：
  autoplot(rr, type = "prc") +
    ggtitle("Test Data PRC") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  
  # 混淆矩阵可视化：
  autoplot(rr$prediction()) +
    theme(
      text = element_text(size = 20),
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20),
      legend.title = element_text(size = 25),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  
  # 测试集 ------------------
  rf_glr$train(task,row_ids = split_task$train) # 训练集上训练
  
  cv_pred <- rf_glr$predict(task_test)
  cv_pred$confusion
  
  cv_pred$score(msrs(c("classif.auc", "classif.acc", "classif.npv","classif.bbrier")))
  
  autoplot(cv_pred,type = "roc") +
    ggtitle("Test Data of randomForest") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_rf_test_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # 混淆矩阵可视化：
  autoplot(cv_pred) +
    ggtitle("Test Data of randomForest") +
    theme(
      plot.title =  element_text(size = 20,hjust = 0.5),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_rf_test_hist.pdf',width = 20,height = 15,dpi = 300,units = 'cm')
  
  cv_pred$score(msrs(c("classif.auc","classif.acc","classif.npv","classif.bbrier")))
  
  cv_pred_df <- as.data.table(cv_pred)
  head(cv_pred_df)
  
}

## 5.2 选择 logistics 模型 ---------------
if(T){
  
  # 选择 logistics 模型
  rf_glr <- as_learner(graph_preb %>>% lrn("classif.log_reg", predict_type="prob"))
  rf_glr$id <- "logistic"
  
  # 折交叉验证
  rr <- resample(task = task,
                 learner = rf_glr,
                 resampling = rsmp("cv",folds = 5),
                 store_models = T
  )
  
  
  # 先看看在训练集中的表现。
  # 混淆矩阵：
  rr$prediction()$confusion
  df_pred <- rr$prediction()
  
  # 查看其他结果：
  c("classif.auc","classif.acc","classif.npv","classif.bbrier") %>%
    msrs() %>%
    rr$aggregate() %>%
    as.data.frame()
  
  autoplot(rr,type = "roc") +
    ggtitle("Train Data of logistic") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_logis_train_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # prc曲线：
  autoplot(rr, type = "prc") +
    ggtitle("Test Data PRC") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  
  # 混淆矩阵可视化：
  autoplot(rr$prediction()) +
    theme(
      text = element_text(size = 20),
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20),
      legend.title = element_text(size = 25),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  
  # 测试集 ------------------
  rf_glr$train(task,row_ids = split_task$train) # 训练集上训练
  
  cv_pred <- rf_glr$predict(task_test)
  cv_pred$confusion
  
  cv_pred$score(msrs(c("classif.auc", "classif.acc", "classif.npv","classif.bbrier")))
  
  autoplot(cv_pred,type = "roc") +
    ggtitle("Test Data of logistic") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_logis_test_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # 混淆矩阵可视化：
  autoplot(cv_pred) +
    ggtitle("Test Data of logistic") +
    theme(
      plot.title =  element_text(size = 20,hjust = 0.5),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_logis_test_hist.pdf',width = 20,height = 15,dpi = 300,units = 'cm')
  
  cv_pred$score(msrs(c("classif.auc","classif.acc","classif.npv","classif.bbrier")))
  
  cv_pred_df <- as.data.table(cv_pred)
  head(cv_pred_df)
}

## 5.3 决策树 ------------
if(T){
  # 决策树
  rf_glr <- as_learner(graph_preb %>>% lrn("classif.rpart", predict_type="prob"))
  rf_glr$id <- "decisionTree"
  
  # 折交叉验证
  rr <- resample(task = task,
                 learner = rf_glr,
                 resampling = rsmp("cv",folds = 5),
                 store_models = T
  )
  
  
  # 先看看在训练集中的表现。
  # 混淆矩阵：
  rr$prediction()$confusion
  df_pred <- rr$prediction()
  
  # 查看其他结果：
  c("classif.auc","classif.acc","classif.npv","classif.bbrier") %>%
    msrs() %>%
    rr$aggregate() %>%
    as.data.frame()
  
  autoplot(rr,type = "roc") +
    ggtitle("Train Data of decisionTree") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_dtree_train_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # prc曲线：
  autoplot(rr, type = "prc") +
    ggtitle("Test Data PRC") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  
  # 混淆矩阵可视化：
  autoplot(rr$prediction()) +
    theme(
      text = element_text(size = 20),
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20),
      legend.title = element_text(size = 25),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  
  # 测试集 ------------------
  rf_glr$train(task,row_ids = split_task$train) # 训练集上训练
  
  cv_pred <- rf_glr$predict(task_test)
  cv_pred$confusion
  
  cv_pred$score(msrs(c("classif.auc", "classif.acc", "classif.npv","classif.bbrier")))
  
  autoplot(cv_pred,type = "roc") +
    ggtitle("Test Data of decisionTree") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_dtree_test_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # 混淆矩阵可视化：
  autoplot(cv_pred) +
    ggtitle("Test Data of decisionTree") +
    theme(
      plot.title =  element_text(size = 20,hjust = 0.5),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_dtree_test_hist.pdf',width = 20,height = 15,dpi = 300,units = 'cm')
  
  cv_pred$score(msrs(c("classif.auc","classif.acc","classif.npv","classif.bbrier")))
  
  cv_pred_df <- as.data.table(cv_pred)
  head(cv_pred_df)
}

## 5.4 kknn模型 -------------
if(T){
  # kknn模型
  rf_glr <- as_learner(graph_preb %>>% lrn("classif.kknn", predict_type="prob"))
  rf_glr$id <- "kknn"
  
  # 折交叉验证
  rr <- resample(task = task,
                 learner = rf_glr,
                 resampling = rsmp("cv",folds = 5),
                 store_models = T
  )
  
  
  # 先看看在训练集中的表现。
  # 混淆矩阵：
  rr$prediction()$confusion
  df_pred <- rr$prediction()
  
  # 查看其他结果：
  c("classif.auc","classif.acc","classif.npv","classif.bbrier") %>%
    msrs() %>%
    rr$aggregate() %>%
    as.data.frame()
  
  autoplot(rr,type = "roc") +
    ggtitle("Train Data of kknn") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_kknn_train_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # prc曲线：
  autoplot(rr, type = "prc") +
    ggtitle("Test Data PRC") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  
  # 混淆矩阵可视化：
  autoplot(rr$prediction()) +
    theme(
      text = element_text(size = 20),
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20),
      legend.title = element_text(size = 25),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  
  # 测试集 ------------------
  rf_glr$train(task,row_ids = split_task$train) # 训练集上训练
  
  cv_pred <- rf_glr$predict(task_test)
  cv_pred$confusion
  
  cv_pred$score(msrs(c("classif.auc", "classif.acc", "classif.npv","classif.bbrier")))
  
  autoplot(cv_pred,type = "roc") +
    ggtitle("Test Data of kknn") +
    theme(
      plot.title =  element_text(size = 20),
      text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    ggsci::scale_color_aaas()+
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_kknn_test_roc.pdf',width = 28,height = 16,dpi = 300,units = 'cm')
  
  
  # 混淆矩阵可视化：
  autoplot(cv_pred) +
    ggtitle("Test Data of kknn") +
    theme(
      plot.title =  element_text(size = 20,hjust = 0.5),
      text = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15)
    ) +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas()
  
  ggsave(filename = 'result/ph/step_05_kknn_test_hist.pdf',width = 20,height = 15,dpi = 300,units = 'cm')
  
  cv_pred$score(msrs(c("classif.auc","classif.acc","classif.npv","classif.bbrier")))
  
  cv_pred_df <- as.data.table(cv_pred)
  head(cv_pred_df)
}
