## packages ------------
if (!require("mlr3verse")) install.packages("mlr3verse")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")

showtext::showtext_auto()

## Machine learner  ------------
# 数据预处理
graph_preb <- po("filter",
  filter = mlr3filters::flt("find_correlation"),
  filter.cutoff = 0.3
) %>>% # 去除高度相关的列
  po("scale", center = T, scale = F) %>>% # 中心化
  po("removeconstants") %>>% # 去掉零方差变量
  po("encode") %>>%
  po("imputemedian")

# 随机森林
randomForest <- as_learner(graph_preb %>>% lrn("classif.ranger", predict_type = "prob"))
randomForest$id <- "randomForest"

# 逻辑回归
logistic <- as_learner(graph_preb %>>% lrn("classif.log_reg", predict_type = "prob"))
logistic$id <- "logistic"

# 决策树
decisionTree <- as_learner(graph_preb %>>% lrn("classif.rpart", predict_type = "prob"))
decisionTree$id <- "decisionTree"

# k近邻
kknn <- as_learner(graph_preb %>>% lrn("classif.kknn", predict_type = "prob"))
kknn$id <- "kknn"

ml_learners <- list(
  randomForest=randomForest,
  logistic=logistic,
  decisionTree=decisionTree,
  kknn=kknn
)

## mytheme ------
mytheme <- list(
  theme(
    text = element_text(size = 0),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ),
  labs(fill = "learner", color = "learner"),
  ggsci::scale_color_aaas(),
  ggsci::scale_fill_aaas()
)

# ggplot 画图设置
num_theme <- list(
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
## output_plot ------
output_plot <- c(
  pdf = "downloadHandler(filename='plot.pdf', content=function(file){pdf(file, width=input$w0,height=input$h0);print(lr_plot() );dev.off() })",
  png = "downloadHandler(filename='plot.png', content=function(file){png(file, width=input$w0,height=input$h0, units='in',res=input$ppi0);print(lr_plot() );dev.off() })",
  jpeg = "downloadHandler(filename='plot.jpeg',content=function(file){jpeg(file,width=input$w0,height=input$h0, units='in',res=input$ppi0);print(lr_plot() );dev.off() })",
  tiff = "downloadHandler(filename='plot.tiff',content=function(file){tiff(file,width=input$w0,height=input$h0, units='in',res=input$ppi0);print(lr_plot() );dev.off() })",
  rds = "downloadHandler(filename='plot.RDS', content=function(file){saveRDS(lr_plot(),file)})"
)

