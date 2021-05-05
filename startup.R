#' Start plumbing.
p <- plumber::plumb("R/plumber.R")

p$registerHook("exit", function() {
  message("Exit plumber!")
})

p$run(
  port = as.integer(Sys.getenv("TDI_port", unset = "8585")),
  host = Sys.getenv("TDI_host", unset = "0.0.0.0"),
  docs = FALSE
)
