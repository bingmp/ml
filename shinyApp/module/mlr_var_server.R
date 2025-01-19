mlr_var_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot({
        return(NULL)
      })

      sample <- reactive({
        readRDS(("./www/df_sample.RDS")) %>% return()
      })

      observeEvent(input$submit_show, {
        # Load the data # 读取数据
        df_raw <- reactive({
          file_RDS <- input$file_RDS

          if (is.null(file_RDS)) {
            data <- sample()
          } else {
            file_suf <- tail(unlist(strsplit(file_RDS$datapath, "[.]")), 1)
            if (tolower(file_suf) == "rds") {
              data <- readRDS(file_RDS$datapath) %>% na.omit()
            } else {
              data <- sample()
            }
          } # else
          return(data)
        })

        output$table <- DT::renderDataTable(
          if (is.null(df_raw())) {
            return(NULL)
          } else {
            df_raw() %>% return()
          }
        )

        observeEvent(input$submit_analysis, {
          fct_result <- reactive({
            # 分类变量统计函数
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

            df_raw() %>%
              select_if(is.factor) %>%
              tab_fct_fun(group = "group") %>%
              return()
          })

          num_result <- reactive({
            df_num <- tibble(
              group = df_raw()$group,
              df_raw() %>% select_if(is.numeric)
            )

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

            if (input$scale == "raw") {
              if (input$test == "t_test") {
                # 分组 t检验
                tab_test <- reshape2::melt(df_num_long) %>%
                  group_by(vars) %>%
                  rstatix::t_test(value ~ group, paired = F) %>% # t 检验
                  rstatix::add_significance("p") %>%
                  # rstatix::adjust_pvalue(method = "fdr") %>% # 矫正 p 值
                  # rstatix::add_significance("p.adj") %>%
                  rstatix::add_xy_position(x = "group")
              } else if (input$test == "wilcox_test") {
                # 分组 t检验
                tab_test <- reshape2::melt(df_num_long) %>%
                  group_by(vars) %>%
                  rstatix::wilcox_test(value ~ group) %>% # 秩和检验
                  rstatix::add_significance("p") %>%
                  # rstatix::adjust_pvalue(method = "fdr") %>% # 矫正 p 值
                  # rstatix::add_significance("p.adj") %>%
                  rstatix::add_xy_position(x = "group")
              }

              # 箱图
              plot <- ggpubr::ggboxplot(df_num_long,
                x = "group", y = "value", color = "group",
                palette = "jco", facet.by = "vars"
              ) +
                ggpubr::stat_pvalue_manual(tab_test, label = "{p.signif}") +
                # ggpubr::stat_pvalue_manual(tab_test, label = "{p.adj.signif}") +
                labs(fill = "BPD", color = "BPD") + ggtitle(input$plot_title) +
                xlab("") + ylab("") + num_theme
            } else if (input$scale == "scale") {
              # 中心化
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

              if (input$test == "t_test") {
                # 检验
                tab_test <- reshape2::melt(df_num_scale) %>%
                  group_by(vars) %>%
                  rstatix::t_test(value ~ group, paired = F) %>% # t 检验
                  rstatix::add_significance("p") %>%
                  # rstatix::adjust_pvalue(method = "fdr") %>% # 矫正 p 值
                  # rstatix::add_significance("p.adj") %>%
                  rstatix::add_xy_position(x = "group")
                
                  
              } else if (input$test == "wilcox_test") {
                # 检验
                tab_test <- reshape2::melt(df_num_scale) %>%
                  group_by(vars) %>%
                  rstatix::wilcox_test(value ~ group) %>% # 秩和检验
                  rstatix::add_significance("p") %>%
                  # rstatix::adjust_pvalue(method = "fdr") %>% # 矫正 p 值
                  # rstatix::add_significance("p.adj") %>%
                  rstatix::add_xy_position(x = "group")
                
              }
              # 箱图
              plot <- ggpubr::ggboxplot(df_num_scale,
                x = "group", y = "value", color = "group",
                palette = "jco", facet.by = "vars"
              ) +
                ggpubr::stat_pvalue_manual(tab_test, label = "{p.signif}") +
                # ggpubr::stat_pvalue_manual(tab_test, label = "{p.adj.signif}") +
                labs(fill = "BPD", color = "BPD") + ggtitle(input$plot_title) +
                xlab("") + ylab("") + num_theme
            }
            
            tab_test$groups <- as.character(tab_test$groups)
            tab_test$test <- input$test
            
            list(
              tab_num = tab_num,
              tab_test = tab_test,
              plot = plot
            ) %>% return()
          })

          # 变量统计数据展示（连续变量 与 分类变量）
          lr_table <- reactive({
            if (input$var == "number") {
              return(num_result()[["tab_num"]])
            } else if (input$var == "factor") {
              return(fct_result())
            }
          })

          output$lr_table <- DT::renderDataTable({
            return(lr_table())
          })

          # 连续变量差异检验
          output$lr_tab_test <- renderDataTable({
            num_result()[["tab_test"]] %>% return()
          })

          # 连续变量箱图展示
          lr_plot <- reactive({
            num_result()[["plot"]] %>% return()
          })

          output$lr_plot <- renderPlot({
            lr_plot() %>% return()
          })

          # download plot: pdf, png, jpeg, tiff, rds
          output$pdf0 <- eval(parse(text = output_plot[["pdf"]]))
          output$png0 <- eval(parse(text = output_plot[["png"]]))
          output$jpeg0 <- eval(parse(text = output_plot[["jpeg"]]))
          output$tiff0 <- eval(parse(text = output_plot[["tiff"]]))
          output$rds0 <- eval(parse(text = output_plot[["rds"]]))

          output$lr_tab_result <- downloadHandler(
            filename = paste0("tab.csv"),
            content = function(file) {
              write.csv(lr_table(), file, row.names = F, fileEncoding = "GB18030")
            }
          )

          output$lr_tab_test_result <- downloadHandler(
            filename = paste0("test.csv"),
            content = function(file) {
              write.csv(num_result()[["tab_test"]], file, row.names = F, fileEncoding = "GB18030")
            }
          )
        })
      })
    }
  )
}
