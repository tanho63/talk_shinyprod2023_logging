library(shiny)
library(logger)
library(here)
library(sessioninfo)

log_crash <- function(){
  #' Using `get()` here feels a little off-label. It's the most convenient way to
  #' get to the actual error object that is fired within `shinyCallingHandlers()`:
  #' <https://github.com/rstudio/shiny/blob/b8923e9/R/utils.R#L483-L494>
  #'
  #' My hypothesis is that the option/handler is designed to work with the usual
  #' base R handlers like `recover()` and `traceback()`, which don't have arguments.
  e <- get("e", envir = parent.frame())

  stack_trace <- shiny::printStackTrace(e) |>
    capture.output(type = "message") |>
    list()

  logger::log_fatal(
    msg = e$message,
    stack_trace = stack_trace,
    #' In the interest of keeping these examples self-contained, I've swapped out
    #' sitrep for sessioninfo::session_info() here but sitrep is designed a little
    #' more thoughtfully in this case

    # sitrep = sitrep(),
    session_info = sessioninfo::session_info(),
    timestamp = Sys.time()
  )

  stop(e)
}

options(shiny.error = log_crash)
logger::log_threshold("INFO")
logger::log_appender(logger::appender_file(here::here("error.log")))
logger::log_formatter(logger::formatter_json)
logger::log_layout(logger::layout_json_parser())

ui <- shiny::fluidPage(
  shiny::textInput("err","Error message to log"),
  shiny::actionButton("run","stop()")
)

server <- function(input, output, session) {
  shiny::observeEvent(
    input$run, {
      stop(input$err)
    }
  )
}

shiny::shinyApp(ui, server)
