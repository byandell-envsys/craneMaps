#' Read GBIF Credentials if needed
#'
#' @param reset reset credentials if `TRUE`
#'
#' @return invisible
#' @export
gbif_credentials <- function(reset = FALSE) {
  if(reset) {
    # GBIF needs a username, password, and email
    credentials <- list(
      GBIF_USER = readline(prompt = "username: "),
      GBIF_PWD = readline(prompt = "password: "),
      GBIF_EMAIL = readline(prompt = "email: ")
    )
    for (env_variable in names(credentials)) {
      .Internal(Sys.setenv(env_variable, credentials[[env_variable]]))
    }
  }
  invisible()
}
