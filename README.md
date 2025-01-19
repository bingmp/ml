# Machine learning of BPD-PH

## 一、 Introduction of files

    if (!require("tidyverse")) installed.packages("tidyverse")

### 1.1 data

rawData/ the raw data file.

-   bpd\_final.xlsx

<!-- -->

    list.files("rawData")

    ## [1] "bpd_final.xlsx" "ids.csv"

    readxl::read_excel('rawData/bpd_final.xlsx',1)

    ## # A tibble: 843 × 36
    ##    id     severity_of_ph severity_of_bpd gender ethnicity   province   pda  nrds
    ##    <chr>           <dbl>           <dbl> <chr>  <chr>       <chr>    <dbl> <dbl>
    ##  1 ID1001              1               1 F      han chinese 重庆市       1     1
    ##  2 ID1002              0              NA M      han chinese 重庆市      NA    NA
    ##  3 ID1003              2              NA M      han chinese 重庆市      NA    NA
    ##  4 ID1004              0               1 F      han chinese 重庆市       0     1
    ##  5 ID1005              0              NA M      han chinese 重庆市      NA    NA
    ##  6 ID1006              2               1 M      han chinese 重庆市       1     0
    ##  7 ID1007              2               1 M      han chinese 四川省       0     0
    ##  8 ID1008              2               1 F      han chinese 重庆市       0     1
    ##  9 ID1009              0              NA M      han chinese 重庆市      NA    NA
    ## 10 ID1010              2               0 F      Other       重庆市       1     1
    ## # ℹ 833 more rows
    ## # ℹ 28 more variables: nec <dbl>, cesarean_section <dbl>,
    ## #   pregncy_induced_hypertension <dbl>, gestatiol_diabetes <dbl>,
    ## #   fetal_distress <dbl>, preeclampsia <dbl>, multiple_births <dbl>,
    ## #   tural_conception <dbl>, placental_abnormalities <dbl>,
    ## #   placental_abruption <dbl>, prom <dbl>,
    ## #   duration_of_prom_exceeds_18_hours <dbl>, oligohydramnios <dbl>, …

### 1.2 code

code/ Rscript of the analysis.

    list.files("code")

    ##  [1] "China_map_fun.R"         "step_01_mice.R"         
    ##  [3] "step_02_factor.R"        "step_02_number.R"       
    ##  [5] "step_02_province_test.R" "step_03_select_vars.R"  
    ##  [7] "step_04_learner.R"       "step_05_models.R"       
    ##  [9] "step_06_logis.R"         "step_07_DCA.R"

### 1.3 rds

rds/ Some important intermediate results are saved in this folder

    list.files("rds")

    ## [1] "df_mice_na_ph.RDS"       "step_03_mlr3_var_ph.RDS"
    ## [3] "step_03_mlr3_var.RDS"    "step_04_bmr_ph.rds"

    readRDS('rds/df_mice_na_ph.RDS') %>% as_tibble()

    ## # A tibble: 708 × 45
    ##    group  severity_of_bpd gender ethnicity   province pda   nrds  nec  
    ##    <fct>  <fct>           <fct>  <fct>       <fct>    <fct> <fct> <fct>
    ##  1 Mild   1               F      han chinese 重庆市   1     1     0    
    ##  2 Mild   1               F      han chinese 重庆市   0     1     0    
    ##  3 Severe 1               M      han chinese 重庆市   1     0     0    
    ##  4 Severe 1               M      han chinese 四川省   0     0     0    
    ##  5 Severe 1               F      han chinese 重庆市   0     1     0    
    ##  6 Severe 0               F      Other       重庆市   1     1     0    
    ##  7 Mild   0               F      han chinese 重庆市   1     0     0    
    ##  8 Mild   1               M      han chinese 四川省   0     0     0    
    ##  9 Severe 1               M      han chinese 重庆市   1     1     0    
    ## 10 Mild   1               M      han chinese 重庆市   1     1     1    
    ## # ℹ 698 more rows
    ## # ℹ 37 more variables: cesarean_section <fct>,
    ## #   pregncy_induced_hypertension <fct>, gestatiol_diabetes <fct>,
    ## #   fetal_distress <fct>, preeclampsia <fct>, multiple_births <fct>,
    ## #   tural_conception <fct>, placental_abnormalities <fct>,
    ## #   placental_abruption <fct>, prom <fct>,
    ## #   duration_of_prom_exceeds_18_hours <fct>, oligohydramnios <fct>, …

### 1.4 result

result/ All plots and tables have been saved in this folder

    list.files("result/ph")

    ##  [1] "step_02_number_scale.pdf"    "step_02_number.pdf"         
    ##  [3] "step_02_tab_fct.csv"         "step_02_tab_num.csv"        
    ##  [5] "step_02_tab_number_test.csv" "step_03_cor.pdf"            
    ##  [7] "step_03_rf_error.pdf"        "step_03_rf_importance.pdf"  
    ##  [9] "step_04_auc.pdf"             "step_04_bmr_msrs.csv"       
    ## [11] "step_04_learner.pdf"         "step_04_mlr3_hist_test.png" 
    ## [13] "step_04_mlr3_test_auc.pdf"   "step_04_mlr3_train_roc.pdf" 
    ## [15] "step_05_dtree_test_hist.pdf" "step_05_dtree_test_roc.pdf" 
    ## [17] "step_05_dtree_train_roc.pdf" "step_05_kknn_test_hist.pdf" 
    ## [19] "step_05_kknn_test_roc.pdf"   "step_05_kknn_train_roc.pdf" 
    ## [21] "step_05_logis_test_hist.pdf" "step_05_logis_test_roc.pdf" 
    ## [23] "step_05_logis_train_roc.pdf" "step_05_rf_test_hist.pdf"   
    ## [25] "step_05_rf_test_roc.pdf"     "step_05_rf_train_roc.pdf"   
    ## [27] "step_06_logis_nom.pdf"       "step_07_dca_impact.pdf"     
    ## [29] "step_07_dca_test.pdf"        "step_07_dca.pdf"

### 1.5 shinyApp

    list.files("shinyApp")

    ## [1] "app.R"  "lang"   "module" "www"

Code for machine learning shiny web tool.
