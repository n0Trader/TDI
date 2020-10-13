# Setup Mock test classes.
MockDriver <<- R6::R6Class("MockDriver", inherit = TDIDriver,
  public = list(
    initialize = function() {
      invisible(self)
    },

    connect = function() {
      # Create connection to the MockAPI.
      con <- MockAPI$new(
        driver = self, 
        conn_args = list(baseURL = "https://mock.com"),
        endpoints = list(a = "/api"),
        values = list(v = c(1,2))
      )
      invisible(con)
    }
))

MockAPI <<- R6::R6Class("MockAPI", inherit = TDIConnection)