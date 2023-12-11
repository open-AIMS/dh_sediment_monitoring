readme_tab <- tabItem(tabName = "readme",
                fluidPage(
                  tags$iframe(src = './readme.html', 
                              width = '100%', height = '800px', 
                              frameborder = 0, scrolling = 'auto'
                  )
                )
        )
