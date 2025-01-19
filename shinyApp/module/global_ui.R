if (!require("shiny.i18n")) install.packages("shiny.i18n") # 语言切换
if (!require("DT")) install.packages("DT")

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")
lang$set_translation_language("en") # here you select the default translation to display

download_plot_UI <- function(id){# download plot ui setting
  ns <- NS(id)
  tagList(
    hr(),
    dropdownButton(circle=FALSE, label="Download Plot", status="success",icon = icon("download"),
                   br(),br() ,
                   numericInput(inputId  = ns('w0'),    label = "plot.weight",value = 8),
                   numericInput(inputId  = ns('h0'),    label = "plot.high",value = 8),
                   numericInput(inputId  = ns('ppi0'),  label = "plot.dpi",value = 150),
                   downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("rds0"),  label = "RDS",  size='sm', block=TRUE )
    )
  )
}