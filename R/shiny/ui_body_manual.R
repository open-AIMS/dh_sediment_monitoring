tag_styles <- tags$style(HTML(
        "
    #shiny-tab-manual .container-fluid {
      padding: 0px;
      margin: -15px;
    }
"
))

manual_tab <- tabItem(tabName = "manual",
                fluidPage(
                  tag_styles,
                  tags$iframe(src = 'https://open-aims.github.io/dh_sediment_monitoring/manual.html', 
style='width:100vw;height:100vh;',
                              ## width = '100%', height = '100%', 
                              frameborder = 0, scrolling = 'auto'
                  )
                )
        )
