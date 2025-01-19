if (!require("shiny")) install.packages("shiny")
if (!require("bs4Dash")) install.packages("bs4Dash")
if (!require("shinyWidgets")) install.packages("shinyWidgets")


source("module/global_ui.R")
source("module/mlr_df_ui.R")
source("module/mlr_var_ui.R")
source("module/mlr_bmr_ui.R")
source("module/mlr_rsmp_ui.R")
source("module/mlr_learner_ui.R")

# File with translations
# lang <- Translator$new(translation_csvs_path = "./lang/info/")
# lang$set_translation_language("en") # here you select the default translation to display

ui <- bs4DashPage(
  # preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  title = "Mlr3verse",
  fullscreen = T, help = NULL,
  header = bs4DashNavbar(
    shiny.i18n::usei18n(lang),
    div(
      shinyWidgets::materialSwitch(
        inputId = "lang", label = lang$t("English"),
        value = F, status = "primary", right = T
      ),
      style = "height:20px"
    ),
    title = dashboardBrand(
      title = lang$t("CHCMU"),
      color = "primary",
      href = "https://stu.chcmu.asia",
      image = "./logo_chcmu.png",
      opacity = 1
    ),
    disable = FALSE,
    # skin = "light",
    # status = "white",
    border = TRUE,
    sidebarIcon = shiny::icon("bars"),
    # controlbarIcon = shiny::icon("table-cells"),
    fixed = FALSE,

    # Dropdown menu for notifications
    rightUi = dropdownMenu(
      type = "notifications", badgeStatus = "warning",
      notificationItem(
        icon = icon("user", lib = "glyphicon"),
        status = "danger", "Bingmp",
        href = "https://gitee.com/bingmp"
      ),
      notificationItem(
        icon = icon("github", lib = "font-awesome"),
        status = "danger", "Github",
        href = "https://github.com/bingmp"
      ),
      notificationItem(
        icon = icon("docker", lib = "font-awesome"),
        status = "danger", "Docker",
        href = "https://hub.docker.com/pengbm"
      ),
      notificationItem(
        icon = icon("lungs", lib = "font-awesome"), status = "info",
        "Lab. of Pediatric Respir. Medicine",
        href = "https://stu.chcmu.asia"
      ),
      notificationItem(
        icon = icon("envelope", lib = "glyphicon"), status = "danger",
        "bingmp@stu.cqmu.edu.cn"
      )
    )
  ),
  ## Sidebar content
  sidebar = bs4DashSidebar(
    width = 1,
    skin = "light",
    status = "primary",
    elevation = 2,
    bs4SidebarUserPanel(
      image = "./lung.gif",
      name = lang$t("Machine learning")
    ),
    sidebarMenu(
      id = "sidebar",
      sidebarHeader(title = "mlr3verse"),
      bs4Dash::menuItem(lang$t("Data"), tabName = "data", icon = icon("database")),
      bs4Dash::menuItem(lang$t("Summary"), tabName = "summary", icon = icon("table-list", lib = "font-awesome")),
      bs4Dash::menuItem(lang$t("Benchmark"), tabName = "benchmark", icon = icon("table-list", lib = "font-awesome")),
      bs4Dash::menuItem(lang$t("Resample"), tabName = "resample", icon = icon("table-list", lib = "font-awesome")),
      bs4Dash::menuItem(lang$t("Train and Test"), tabName = "learner", icon = icon("chart-simple"))
    )
  ),
  footer = dashboardFooter(
    left = a(
      href = "https://idbview.com",
      target = "_blank", "idbview"
      # ,a(
      #   href = "https://beian.miit.gov.cn/",
      #   target = "_blank", "渝ICP备2023006607号"
      # ),
    ),
    right = "@2024"
  ),
  # controlbar = dashboardControlbar(disable = T),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "data", mlr_df_UI("mlr_df")),
      tabItem(tabName = "summary", mlr_var_UI("mlr_var")),
      tabItem(tabName = "benchmark", mlr_bmr_UI("mlr_bmr")),
      tabItem(tabName = "resample", mlr_rsmp_UI("mlr_rsmp")),
      tabItem(tabName = "learner", mlr_learner_UI("mlr_learner"))
    )
  )
)

server <- function(input, output, session) {
  waiter::waiter_hide()

  observeEvent(input$lang, {
    if (input$lang == T) {
      shiny.i18n::update_lang("cn")
    } else {
      shiny.i18n::update_lang("en")
    }
  })

  source("module/mlr_df_server.R")
  source("module/mlr_var_server.R")
  source("module/global_server.R")
  source("module/mlr_bmr_server.R")
  source("module/mlr_rsmp_server.R")
  source("module/mlr_learner_server.R")

  mlr_df_Server("mlr_df")
  mlr_var_Server("mlr_var")
  mlr_bmr_Server("mlr_bmr")
  mlr_rsmp_Server("mlr_rsmp")
  mlr_learner_Server("mlr_learner")
}

shinyApp(ui, server)
