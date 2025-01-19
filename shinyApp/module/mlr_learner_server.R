mlr_learner_Server <- function(id) {
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

        tasks <- reactive({
          df_all <- cbind(
            df_raw() %>% select_if(is.factor),
            df_raw() %>% select_if(is.numeric)
          )
          # 建立任务
          task <- as_task_classif(df_all, target = colnames(df_all)[1] )
          split_task <- partition(task, ratio = input$partition / 100)

          task_train <- task$clone()$filter(split_task$train)
          task_test <- task$clone()$filter(split_task$test)

          tasks <- list(
            task = task,
            task_train = task_train,
            task_test = task_test
          ) %>% return()
        })

        observeEvent(input$submit_analysis, {
          # 选择学习器训练
          lr_result <- reactive({
            if (is.null(df_raw())) {
              return(NULL)
            }

            ## 2.2 学习算法
            if (is.null(input$lr_select)) {
              return(NULL)
            } else {
              learner <- ml_learners[[input$lr_select]]
            }

            tasks <- tasks()

            learner$train(tasks$task_train) # 训练

            if (input$predict_type == "train") {
              lr_result <- learner$predict(tasks$task_train) # 训练集
            } else if (input$predict_type == "test") {
              lr_result <- learner$predict(tasks$task_test) # 测试集
            }
            return(lr_result)
          })

          # 编写函数
          lr_plot <- reactive({
            if (is.null(lr_result())) {
              return(NULL)
            }
            lr_result <- lr_result()

            p <- autoplot(lr_result, type = input$msr_type) +
              ggtitle(input$msr_title) +
              mytheme

            return(p)
          })

          lr_table <- reactive({
            lr_result <- lr_result()

            if (input$tab_type == "Measure") {
              input$msr_select %>%
                msrs() %>%
                lr_result$score() %>%
                as.data.frame() %>%
                return()
            } else if (input$tab_type == "Predict") {
              df_pred <- lr_result

              predict_df <- data.frame(
                truth = df_pred$truth,
                response = df_pred$response,
                df_pred$prob
              )
              rownames(predict_df) <- df_pred$row_ids

              predict_df %>% return()
            } else if (input$tab_type == "confusion") {
              perf <- lr_result$confusion
              perf <- as.data.frame.matrix(perf, row.names = paste("Actual", row.names(perf)))
              colnames(perf) <- paste("Predict", colnames(perf))
              perf %>% return()
            }
          })
          output$lr_plot <- renderPlot({
            if (is.null(lr_plot())) {
              return(NULL)
            }
            return(lr_plot())
          })

          output$lr_table <- DT::renderDataTable({
            if (is.null(lr_table())) {
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
              filename = paste0("result.csv"),
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
