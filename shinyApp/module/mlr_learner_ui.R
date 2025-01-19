
mlr_learner_UI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(
        title = "Data",
        fluidRow(
          box(
            title = lang$t("Raw data"), width = 8, solidHeader = TRUE, status = "primary",
            splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("table")))
          ),
          box(
            width = 4, status = "success",
            fileInput(ns("file_RDS"), label = lang$t("Input file"), multiple = FALSE,placeholder = '.RDS'),
            h6("Format: .RDS"),
            actionBttn(ns("submit_show"), label = lang$t("Show Data"), style = "fill", color = "primary", size = "sm")
          )
        )
      ),
      tabPanel(
        title = "Plot",
        fluidRow(
          box(
            title = lang$t("Learner"), width = 8, solidHeader = TRUE, status = "primary",
            splitLayout(cellWidths = c("100%"), plotOutput(ns("lr_plot")))
          ),
          box(
            width = 4, status = "success",
            actionBttn(ns("submit_analysis"), label = lang$t("Start Analysis"), style = "fill", color = "primary", size = "sm"),
            hr(),
            fluidRow(
              column(width = 12, textInput(inputId = ns("msr_title"), label = "Title", value = "Machine learning")),
              column(width = 12, selectInput(
                inputId = ns("lr_select"), label = "learners", multiple = F,
                choices = c("randomForest", "logistic", "decisionTree", "kknn"),
                selected = "randomForest"
              )),
              column(width = 12, sliderInput(inputId = ns("partition"), "Partition (train)", value = 75, min = 0, max = 100)),
              column(width = 6, selectInput(inputId = ns("predict_type"), label = "Plot", choices = c("train", "test"), selected = "train")),
              column(width = 6, selectInput(inputId = ns("msr_type"), label = "Measure", choices = c("roc", "prc"), selected = "roc"))
            ),
            download_plot_UI("mlr_learner")
          )
        )
      ),
      tabPanel(
        title = "Table",
        fluidRow(
          box(
            title = lang$t("Measure Table"), width = 8, solidHeader = TRUE, status = "primary",
            splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("lr_table")))
          ),
          box(
            width = 4, status = "success",
            column(width = 12, selectInput(inputId = ns("tab_type"), label = "Table", choices = c("Measure", "Predict", "confusion"), selected = "Measure")),
            column(width = 12, selectInput(
              inputId = ns("msr_select"),
              label = "Measure", multiple = T,
              choices = c("classif.acc", "classif.auc", "classif.npv","classif.ppv","classif.sensitivity", "classif.specificity", "classif.bbrier"),
              selected = c("classif.acc", "classif.auc", "classif.npv","classif.ppv","classif.sensitivity", "classif.specificity", "classif.bbrier")
            )),
            downloadBttn(outputId = ns("lr_tab_result") ,  label = "Downlaod Table", size='sm', block=TRUE ) 
          )
        )
      )
    ) # tabsetPanel
  ) # tagList
} # function(id)
