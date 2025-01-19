mlr_rsmp_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      sample <- reactive({
        readRDS(("./www/df_sample.RDS")) %>% return()
      })

      observeEvent(input$submit_show, {
        # 选择学习器训练
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
          lr_result <- reactive({
            
            df_all <- cbind(
              df_raw() %>% select_if(is.factor),
              df_raw() %>% select_if(is.numeric)
            )
            
            # 建立任务
            task <- as_task_classif(df_all, target = colnames(df_all)[1] )

            ## 2.2 学习算法
            if (is.null(input$lr_select)) {
              return(NULL)
            } else {
              learner <- ml_learners[[input$lr_select]]
            }

            # 折交叉验证
            lr_result <- resample(
              task = task,
              learner = learner,
              resampling = rsmp("cv", folds = input$cv_n),
              store_models = T
            )
            return(lr_result)
          })

          # 编写函数
          lr_plot <- reactive({
            if (is.null(lr_result())) {
              return(NULL)
            }
            lr_result <- lr_result()

            if (input$msr_type == "predict") {
              p <- autoplot(lr_result$prediction())
            } else {
              p <- autoplot(lr_result, type = input$msr_type)
            }
            p <- p +
              ggtitle(input$msr_title) +
              mytheme

            return(p)
          })

          lr_table <- reactive({
            lr_result <- lr_result()

            if (input$tab_type == "Measure") {
              input$msr_select %>%
                msrs() %>%
                lr_result$aggregate() %>%
                as.data.frame() %>%
                return()
            } else if (input$tab_type == "Predict") {
              df_pred <- lr_result$prediction()

              predict_df <- data.frame(
                truth = df_pred$truth,
                response = df_pred$response,
                df_pred$prob
              )
              rownames(predict_df) <- df_pred$row_ids

              predict_df %>% return()
            } else if (input$tab_type == "confusion") {
              perf <- lr_result$prediction()$confusion
              perf <- as.data.frame.matrix(perf, row.names = paste("Actual", row.names(perf)))
              colnames(perf) <- paste("Predict", colnames(perf))
              perf %>% return()
            }
          })
          output$lr_plot <- renderPlot({
            if (is.null(lr_result())) {
              return(NULL)
            }
            return(lr_plot())
          })

          output$lr_table <- DT::renderDataTable({
            if (is.null(lr_result())) {
              return(NULL)
            }
            return(lr_table())
          })

          if (T) {
            # download plot: pdf, png, jpeg, tiff, rds
            output$pdf0 <- eval(parse(text = output_plot[["pdf"]]))
            output$png0 <- eval(parse(text = output_plot[["png"]]))
            output$jpeg0 <- eval(parse(text = output_plot[["jpeg"]]))
            output$tiff0 <- eval(parse(text = output_plot[["tiff"]]))
            output$rds0 <- eval(parse(text = output_plot[["rds"]]))

            output$lr_tab_result <- downloadHandler(
              filename = paste0("result_rsmp.csv"),
              content = function(file) {
                write.csv(lr_table(), file, row.names = T, fileEncoding = "GB18030")
              }
            )
          }
        })
      })
    }
  )
}
