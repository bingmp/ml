if (!require("tidyverse")) install.packages("tidyverse")
if (!require("rmda")) install.packages("rmda")
if (!require("ggDCA")) devtools::install_github("yikeshu0611/ggDCA")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("rms")) install.packages("rms")


rm(list = ls())

var <- readRDS("./rds/step_03_mlr3_var_ph.RDS")
# 变量名排序

df1 <- readRDS("./rds/df_mice_na_ph.RDS") %>%
  subset(select = var) %>%
  as_tibble()

df1$group <- as.numeric(df1$group) - 1

# ggplot 画决策曲线
if (T) {
  ddist <- datadist(df1)
  options(datadist = "ddist")

  pda <- lrm(group ~ pda, df1)
  nec <- lrm(group ~ nec, df1)
  age_days <- lrm(group ~ age_days, df1)
  birth_weight <- lrm(group ~ birth_weight, df1)
  vsd_shunting <- lrm(group ~ vsd_shunting, df1)
  gestation_age <- lrm(group ~ gestation_age, df1)
  admission_weight <- lrm(group ~ admission_weight, df1)
  pulmory_hemorrhage <- lrm(group ~ pulmory_hemorrhage, df1)
  pda_maximum_diameter <- lrm(group ~ pda_maximum_diameter, df1)
  pda_blood_flow_velocity <- lrm(group ~ pda_blood_flow_velocity, df1)
  pda_blood_flow_direction <- lrm(group ~ pda_blood_flow_direction, df1)
  area_of_tricuspid_regurgitation <- lrm(group ~ area_of_tricuspid_regurgitation, df1)
  peak_velocity_of_tricuspid_regurgitation <- lrm(group ~ peak_velocity_of_tricuspid_regurgitation, df1)

  all_factors <- lrm(group ~ .,
    # pda +
    # nec + age_days +
    # birth_weight + vsd_shunting +
    # gestation_age + admission_weight +
    # pulmory_hemorrhage + pda_maximum_diameter +
    # pda_blood_flow_velocity + pda_blood_flow_direction +
    # area_of_tricuspid_regurgitation + peak_velocity_of_tricuspid_regurgitation,
    df1)

  # dca(all_factors) %>% ggplot()

  # 模型合并比较

  # all factor
  Model_data <- dca(all_factors,
    age_days,
    birth_weight, admission_weight,
    gestation_age, 
    pulmory_hemorrhage,
    area_of_tricuspid_regurgitation, peak_velocity_of_tricuspid_regurgitation,
    nec, vsd_shunting,pda, pda_maximum_diameter,
    pda_blood_flow_velocity, pda_blood_flow_direction,
    model.names = c(
      "All factors",
      "Age (day)",
      "Birth weight", "Admission weight",
      "Gestation age", 
      "Pulmory hemorrhage",
      "Area of tricuspid regurgitation", "Peak velocity of tricuspid regurgitation",
      "NEC", "VSD shunting","PDA", "PDA maximum diameter",
      "PDA blood flow velocity", "PDA blood flow direction"
    )
  )

  Model_data$col <- recode(Model_data$model,
    "All factors" = "col1",
    "Age (day)" = "col1",
    "Birth weight" = "col1",
    "Admission weight" = "col1",
    "Gestation age" = "col1",
    "Pulmory hemorrhage" = "col1",
    "Area of tricuspid regurgitation" = "col1",
    "Peak velocity of tricuspid regurgitation" = "col2",
    "NEC" = "col2",
    "VSD shunting" = "col2",
    "PDA" = "col2",
    "PDA maximum diameter" = "col2",
    "PDA blood flow velocity" = "col2",
    "PDA blood flow direction" = "col2",
    "All" = "col1",
    "None" = "col1"
  )
  
  Model_data$row <- recode(Model_data$model,
                             "All factors" = "row1",
                             "Age (day)" = "row1",
                             "Birth weight" = "row1",
                             "Admission weight"= "row1",
                             "Gestation age" = "row2",
                             "Pulmory hemorrhage" = "row2",
                             "Area of tricuspid regurgitation" = "row2",
                             "Peak velocity of tricuspid regurgitation" = "row1",
                             "NEC" = "row1",
                             "VSD shunting" = "row1",
                             "PDA" = "row2",
                             "PDA maximum diameter" = "row2",
                             "PDA blood flow velocity" = "row2",
                             "PDA blood flow direction" = "row2",
                             "All" = "row1",
                             "None" = "row1"
  )
  
  Model_data2 <- subset(Model_data,model=='All factors'|model=='All'|model=='None')
  Model_data3 <- Model_data2;Model_data4 <- Model_data2
 
  Model_data2$col <- 'col1'; Model_data2$row <- "row2"
  Model_data3$col <- 'col2'; Model_data3$row <- "row1"
  Model_data4$col <- 'col2'; Model_data4$row <- "row2"

  rbind(Model_data,Model_data2,Model_data3,Model_data4) %>% 
    ggplot() +
    geom_line(aes(color = model))+
    ggsci::scale_color_d3(palette = "category20") +
    facet_grid(col~row ) +
    theme( strip.text.x = element_blank(),
           strip.text.y = element_blank() )

  ggsave(filename = "result/ph/step_07_dca.pdf", width = 32, height = 20, dpi = 300, units = "cm")

}

# 默认决策曲线
if (T) {
  Models <- decision_curve(
    group ~ pda +
      nec + age_days +
      birth_weight + vsd_shunting +
      gestation_age + admission_weight +
      pulmory_hemorrhage + pda_maximum_diameter +
      pda_blood_flow_velocity + pda_blood_flow_direction +
      area_of_tricuspid_regurgitation + peak_velocity_of_tricuspid_regurgitation,
    data = df1,
    family = binomial(link = "logit"),
    thresholds = seq(0, 1, by = 0.01),
    confidence.intervals = 0.95,
    study.design = "case-control",
    population.prevalence = 0.3
  )

  plot_decision_curve(Models,
    cost.benefit.axis = FALSE,
    col = c("red2", "blue2", "pink2"),
    curve.names = c("Model"),
    confidence.intervals = FALSE,
    standardize = FALSE
  )

  pdf(file = "result/ph/step_07_dca_impact.pdf", width = 12, height = 6)
  plot_clinical_impact(Models,
    population.size = 1000,
    cost.benefit.axis = T,
    n.cost.benefits = 8,
    col = c("red3", "blue3", "pink3"),
    ylim = c(0, 1000),
    xlim = c(0, 1),
    legend.position = "topright",
    # legend.position = "none",
    confidence.intervals = T,
    # cost.benefit.xlab = "Cost:Benefit Ratio",
    cost.benefit.xlab = "Benefit Ratio",
    ylab = "Number high risk",
    xlab = "High Rish Threshold",
    lwd = 3,
    lty = 2
  )
  dev.off()

  # 多个模型展示
  if (F) {
    pda <- decision_curve(
      group ~ pda,
      data = df1,
      family = binomial(link = "logit"),
      thresholds = seq(0, 1, by = 0.01),
      confidence.intervals = 0.95, study.design = "case-control",
      population.prevalence = 0.3
    )

    List <- list(Models, pda)

    plot_decision_curve(List,
      cost.benefit.axis = FALSE,
      col = c("red", "blue"),
      # "topright", "right", "bottomright", "bottom",
      # "bottomleft", "left", "topleft", "top", "none"
      legend.position = "right",
      xlim = c(0, 1),
      curve.names = c("Model", "PDA"),
      confidence.intervals = FALSE,
      standardize = FALSE
    )
  }
}
