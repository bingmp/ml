if (!require("mlr3verse")) install.packages("mlr3verse")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("rms")) install.packages("rms")


rm(list = ls())
# var <- readRDS("./rds/step_03_mlr3_var.RDS")
# var <- readRDS("./rds/step_03_mlr3_var_ph.RDS")

# 变量名排序
var <- c(
  "group",
  "age_days",
  "birth_weight",
  "admission_weight",
  "gestation_age",
  "pulmory_hemorrhage",
  "area_of_tricuspid_regurgitation",
  "peak_velocity_of_tricuspid_regurgitation",
  "nec",
  "vsd_shunting",
  "pda",
  "pda_maximum_diameter",
  "pda_blood_flow_velocity",
  "pda_blood_flow_direction"
)

df_all <- readRDS("./rds/df_mice_na_ph.RDS") %>%
  subset(select = var)

df_all$group <- as.numeric(df_all$group) - 1

colnames(df_all) <- c(
  "Group",
  "Age_days",
  "Birth_weight",
  "Admission_weight",
  "Gestation_age",
  "Pulmory_hemorrhage",
  "Area_of_tricuspid_regurgitation",
  "Peak_velocity_of_tricuspid_regurgitation",
  "NEC",
  "VSD_shunting",
  "PDA",
  "PDA_maximum_diameter",
  "PDA_blood_flow_velocity",
  "PDA_blood_flow_direction"
)

# 去除上一步回归中无差异的因素再次回归

ddist <- datadist(df_all)
options(datadist = "ddist")

fit1 <- lrm(Group ~ ., data = df_all)
summary(fit1)

nom1 <- nomogram(fit1,
  fun = plogis,
  fun.at = c(0.001, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99),
  lp = T, # 是否显示线性概率
  funlabel = "Risk of PH"
)

# pdf(file = "result/ph/step_06_logis_nom.pdf", width = 14, height = 10)
# plot(nom1)
# dev.off()



# colnames(df_all) <- make.names(names(df_all))

# 全部差异因素回归
fit2 <- glm(Group ~ ., data = df_all, family = binomial)
summary(fit2)$coef
coef(fit2)
anova(object = fit2, test = "Chisq")
coef(fit2)

exp(coef(fit2)) # OR



library("MASS")
step.model <- step(object = fit2, trace = 0)
summary(step.model)

anova(object = step.model, test = "Chisq")

anova(fit2, step.model, test = "Chisq")


# 模型图
# Fill predicted values using regression model
df_all$pred = predict( fit2, df_all , type="response")

ggplot(df_all, aes(x=pred, y=Group)) + 
  geom_point(alpha=0.2,size=0.5) +
  stat_smooth(method="glm", color="grey60", se=FALSE,
              method.args = list(family=binomial))+
  ylab("PH (Mild or Severe)")+
  xlab("Predict")+
  scale_y_continuous( breaks = c(0,1))+
  scale_x_continuous( breaks = seq(0.1,1,0.2),limits = c(0.1,0.9))+
  theme_classic()

ggsave(filename = 'result/ph/step_06_logistic_s.pdf',width = 14,height = 8,dpi = 300,units = 'cm')

