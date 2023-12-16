sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Landing", tabName = "landing", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    ## menuItem("Settings", tabName = "settings", icon = icon("sliders")),
    menuItem("Data", tabName = "data", icon = icon("file-excel")),
    menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-column")),
    menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
    menuItem("Manual", tabName = "manual", icon=icon("mortar-board")),
 hr(),
    actionButton("runLoadCode", "Run Stage 2", icon = icon("play")),
    ## actionBttn("runLoadCode", "Run Stage 2", style = "jelly", color =  "primary", icon = icon("play")),
    actionButton("runProcessCode", "Run Stage 3", icon = icon("play"), class = "btn-disabled"),
    ## actionBttn("runProcessCode", "Run Stage 3", style = "unite", color = "primary", icon = icon("play")),
    actionButton("runEDACode", "Run Stage 4", icon = icon("play"), class = "btn-disabled")
    ## actionBttn("runEDACode", "Run Stage 4", style = "unite", color = "primary", icon = icon("play"))
  )
)
