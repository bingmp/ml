
mlr_bmr_Server <- function(id) {
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
          split_task <- partition(task, ratio = 0.75)

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

            # 建立任务
            task <- tasks()$task_train
            
            ## 2.2 学习算法
            if (is.null(input$lr_select)) {
              return(NULL)
            } else {
               learners <-  ml_learners[input$lr_select]
            }

            design <- benchmark_grid(
              tasks = tasks()$task, learners = learners,
              resampling = rsmp("cv", folds = input$cv_n) # 折交叉验证
            )
            lr_result <- benchmark(design) # 执行基准测试

            return(lr_result)
          })

          # 编写函数
          lr_plot <- reactive({
            if (is.null(lr_result())) {
              return(NULL)
            }

            lr_result <- lr_result()

            if (input$bmr_type != "roc" & input$bmr_type != "prc") {
              p <- autoplot(lr_result, measure = msr(input$bmr_type))
            } else {
              p <- autoplot(lr_result, type = input$bmr_type)
            }

            p <- p + ggtitle(input$msr_title) + mytheme
            return(p)
          })

          lr_table <- reactive({
            lr_result <- lr_result()
            input$msr_type %>%
              msrs() %>%
              lr_result$aggregate() %>%
              subset(select = c(
                "learner_id", # 'resampling_id', 'iters',
                input$msr_type
              )) %>%
              return()
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
            output$pdf0  <- eval(parse(text = output_plot[["pdf"]] ))
            output$png0  <- eval(parse(text = output_plot[["png"]] ))
            output$jpeg0 <- eval(parse(text = output_plot[["jpeg"]] ))
            output$tiff0 <- eval(parse(text = output_plot[["tiff"]] ))
            output$rds0  <- eval(parse(text = output_plot[["rds"]] ))
          }
        })
      }) #  observeEvent: input$show

    } # function(input, output, session)
  ) # moduleServer
} # function(id)
