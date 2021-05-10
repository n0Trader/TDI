#' Start plumbing.
p <- plumber::plumb("R/plumber.R") %>%
  plumber::pr_cookie(
    plumber::random_cookie_key(),
    name = "counter"
  ) %>% plumber::pr_hook("exit", function(req) {
    message("Exit plumber!")
  }) %>% plumber::pr_get("/session", function(req) {
    if (!is.null(req$session$counter)){
      count <- as.numeric(req$session$counter)
    } else  count <- 0
    req$session$counter <- count + 1
    return(paste0("This is visit #", count))
  })

# Start the plumber API.
p$run(
  port = as.integer(Sys.getenv("TDI_port", unset = "8585")),
  host = Sys.getenv("TDI_host", unset = "0.0.0.0"),
  docs = FALSE
)
