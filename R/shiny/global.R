toggle_buttons <- function(lst = status_$status, stage = 1, bttn1, bttn2 = NULL) {
  st <- get_stage_status(lst, stage = 1)
  if (st == "success") {
    if (!is.null(bttn2)) {
      shinyjs::enable(bttn2)
      shinyjs::removeClass(id = bttn2, class = "btn-disabled")
      shinyjs::addClass(id = bttn2, class = "btn-default")
    }
    shinyjs::removeClass(id = bttn1, class = "bttn-default")
    shinyjs::addClass(id = bttn1, class = "btn-success")
    updateActionButton(inputId = bttn1, icon = icon("circle-check"))
  } else if (st == "failure") {
    shinyjs::removeClass(id = bttn1, class = "bttn-primary")
    shinyjs::addClass(id = bttn1, class = "bttn-danger")
    updateActionButton(inputId = bttn1, icon = icon("circle-xmark"))
  } else if (st == "warning") {
    shinyjs::removeClass(id = bttn1, class = "bttn-primary")
    shinyjs::addClass(id = bttn1, class = "bttn-warning")
    updateActionButton(inputId = bttn1, icon = icon("circle-exclamation"))
  }
}

get_stage_status <- function(lst, stage = 1) {
  ll <- lapply(seq_along(lst), function(l) {
          ll <- lapply(seq_along(lst[[l]]$names), function(x) {
                  ic <- switch(lst[[l]]$status[[x]],
                          "success" = "circle-check",
                          "failure" = "circle-xmark",
                          "warning" = "circle-exclamation",
                          "pending" = "clock"
                  )
                  structure(list(), sttype = "element", sticon = ic)
          })
          ll <- setNames(ll, lst[[l]]$names)
          attr(ll, "errors") <- any(lst[[l]]$status == "failure")
          attr(ll, "complete") <- all(lst[[l]]$status == "success")
          attr(ll, "warnings") <- any(lst[[l]]$status == "warning")
          attr(ll, "pendings") <- any(lst[[l]]$status == "pending")
          attr(ll, "status") <- ifelse(all(lst[[l]]$status == "success"), "success",
            ifelse(any(lst[[l]]$status == "failure"), "failute",
              ifelse(any(lst[[l]]$status == "warning"), "warning", "pending"))
          )   
          ll
  })
 attr(ll[[stage]], "status")
}

