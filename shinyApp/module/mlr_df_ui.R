mlr_df_UI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(
        title = "Data",
        fluidRow(
          box(
            title = lang$t("Excel data"), width = 8, solidHeader = TRUE, status = "primary",
            splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("table")))
          ),
          box(
            width = 4, status = "success",
            fileInput(ns("file_input"), label = lang$t("Input file"), multiple = FALSE, placeholder = ".xlsx .xls .csv .RDS"),
            h6("Format: .xlsx .xls .csv .RDS"),
            column(width = 12, selectInput(inputId = ns("fileEncoding"), label = "fileEncoding of csv", choices = c("UTF-8", "GB18030"), selected = "GB18030")),
            actionBttn(ns("submit_show"), label = lang$t("Show Data"), style = "fill", color = "primary", size = "sm"),
            hr(),
            column(width = 12, selectInput(
              inputId = ns("var_num"), label = "Number", multiple = T,
              choices = "", selected = ""
            )),
            column(width = 12, selectInput(
              inputId = ns("var_fct"), label = "Factor", multiple = T,
              choices = "", selected = ""
            )),
            column(width = 12, selectInput(
              inputId = ns("var_ord"), label = "Order", multiple = T,
              choices = "", selected = ""
            )),
            downloadButton(ns("downloadSampleData"), lang$t("Sample data"))
          )
        )
      ),
      tabPanel(
        title = "Result",
        fluidRow(
          box(
            title = "Data type", width = 8, solidHeader = TRUE, status = "primary",
            splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("result_tab")))
          ),
          box(
            width = 4, status = "success",
            actionBttn(ns("submit_analysis"), label = lang$t("Start Analysis"), style = "fill", color = "primary", size = "sm"),
            hr(),
            fluidRow(
              column(width = 12, selectInput(
                inputId = ns("var_group"), label = "Group", multiple = F,
                choices = "",
                selected = ""
              )),
              column(width = 12, selectInput(
                inputId = ns("var_select"), label = "Select Variables", multiple = T,
                choices = "",
                selected = ""
              )),
              downloadBttn(outputId = ns("rds0"), label = "RDS", size = "sm", block = TRUE)
            )
          )
        )
      )
    )
  )
}
