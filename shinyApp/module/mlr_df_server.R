mlr_df_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      sample <- reactive({
        readRDS(("./www/df_sample.RDS")) %>% return()
      })


      observeEvent(input$submit_show, {
        # Load the data # 读取数据
        df_raw <- reactive({
          file_input <- input$file_input

          if (is.null(file_input)) {
            data <- sample()
          } else {
            file_suf <- tail(unlist(strsplit(file_input$datapath, "[.]")), 1)
            if (tolower(file_suf) == "rds") {
              data <- readRDS(file_input$datapath)
            } else if (tolower(file_suf) == "xlsx" | tolower(file_suf) == "xls") {
              data <- readxl::read_excel(file_input$datapath)
            } else if (tolower(file_suf) == "csv") {
              data <- read.csv(file_input$datapath, fileEncoding = input$fileEncoding)
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
        
        observe({
          if (!is.null(df_raw())) {
            df_all <- df_raw() %>% as.data.frame()
            
            col_names <- colnames(df_all)
            updateSelectInput(session, "var_fct",
              choices = col_names,
              selected = col_names[1]
            )
            updateSelectInput(session, "var_ord",
              choices = col_names,
              selected = ""
            )
            updateSelectInput(session, "var_num",
              choices = col_names,
              selected = ""
            )
          
          observe({ # observe inner
            updateSelectInput(session, "var_select",
              choices = c(input$var_fct,input$var_ord,input$var_num),
              selected = c(input$var_fct,input$var_ord,input$var_num)
            )
            
            updateSelectInput(session, "var_group",
                              choices = c(input$var_fct,input$var_ord),
                              selected = c(input$var_fct,input$var_ord)[1]
            )
            
            # 因子型数据
            if (length(input$var_fct) >= 1) {
              if(input$var_fct %in% col_names %>% all()){
                for ( i in input$var_fct) {
                  df_all[,i] <- factor(df_all[,i], ordered = F)
                }
              }

            }
            # 有序数据
            if (length(input$var_ord) >= 1) {
              if(input$var_ord %in% col_names %>% all()){
                for ( i in input$var_ord) {
                  df_all[,i] <- factor(df_all[,i], ordered = T)
                }
              }
              
            }
            # 数值型数据
            if (length(input$var_num) >= 1) {
              if(input$var_num %in% col_names %>% all()){
                for ( i in input$var_num ) {
                  df_all[,i] <- as.numeric(df_all[,i])
                }
              }
            }
            
            observeEvent(input$submit_analysis, { 

              result_tab <- reactive({
                # 选择变量
                cols <- colnames(df_all) %>% intersect(input$var_select) %>% 
                  setdiff(input$var_group)
               
                # 提取数据
                data.frame(
                  group = df_all[,input$var_group],
                  df_all %>% as_tibble() %>% .[,cols] 
                ) %>% 
                  return()
              })
              output$result_tab <- renderDataTable({
                result_tab() %>%
                  return()
              })
              
              output$rds0 <- downloadHandler( 
                filename="data_select.RDS",
                content = function(file){
                  saveRDS( result_tab() ,file)
                }  )

              })
            
          })
          
          } # if (!is.null(df_raw()))
        })
      })

      # 下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste("sample_data.csv")
        },
        content = function(file) {
          write.csv(sample(), file, row.names = F, fileEncoding = "GB18030")
        }
      )
    }
  )
}
