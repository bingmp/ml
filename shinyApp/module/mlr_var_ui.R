mlr_var_UI <- function(id) {
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
            fileInput(ns("file_RDS"), label = lang$t("Input file"), multiple = FALSE, placeholder = ".RDS"),
            h6("Format: .RDS"),
            actionBttn(ns("submit_show"), label = lang$t("Show Data"), style = "fill", color = "primary", size = "sm")
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
            actionBttn(ns("submit_analysis"), label = lang$t("Start Analysis"), style = "fill", color = "primary", size = "sm"),
            hr(),
            column(width = 12, selectInput(
              inputId = ns("var"), label = "var",
              choices = c("number", "factor"), selected = "number"
            )),
            downloadBttn(outputId = ns("lr_tab_result"), label = "Downlaod Table", size = "sm", block = TRUE)
          )
        )
      ),
      tabPanel(
        title = "Plot",
        fluidRow(
          box(
            title = lang$t("Learner"), width = 8, solidHeader = TRUE, status = "primary",
            splitLayout(cellWidths = c("100%"), plotOutput(ns("lr_plot"))),
            splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("lr_tab_test")))
          ),
          box(
            width = 4, status = "success",
            fluidRow(
              column(width = 12, textInput(inputId = ns("plot_title"), label = "Title", value = "Boxplot")),
              column(width = 12, selectInput(
                inputId = ns("scale"), label = "Data",
                choices = c("raw", "scale"), selected = "raw"
              )),
              column(width = 12, selectInput(
                inputId = ns("test"), label = "Test",
                choices = c("t_test", "wilcox_test"), selected = "wilcox_test"
              ))
            ),
            downloadBttn(outputId = ns("lr_tab_test_result"), label = "Downlaod Table", size = "sm", block = TRUE),
            download_plot_UI("mlr_var")
          )
        )
      )
    ) # tabsetPanel
  ) # tagList
} # function(id)
