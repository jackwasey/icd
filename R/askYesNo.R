# R core team, from utils package, but not available in R 3.4
askYesNo <- function(msg,
                     default = TRUE,
                     prompts = getOption(
                       "askYesNo",
                       gettext(c("Yes", "No", "Cancel"))
                     ),
                     ...) {
  if (is.character(prompts) && length(prompts) == 1) {
    prompts <- strsplit(prompts, "/")[[1]]
  }
  if (!is.character(prompts) || length(prompts) != 3) {
    fn <- match.fun(prompts)
    return(fn(
      msg = msg, default = default, prompts = prompts,
      ...
    ))
  }
  choices <- tolower(prompts)
  if (is.na(default)) {
    choices[3L] <- prompts[3L]
  } else if (default) {
    choices[1L] <- prompts[1L]
  } else {
    choices[2L] <- prompts[2L]
  }
  msg1 <- paste0("(", paste(choices, collapse = "/"), ") ")
  if (nchar(paste0(msg, msg1)) > 250) {
    cat(msg, "\n")
    msg <- msg1
  }
  else {
    msg <- paste0(msg, " ", msg1)
  }
  ans <- readline(msg)
  match <- pmatch(tolower(ans), tolower(choices))
  if (!nchar(ans)) {
    default
  } else if (is.na(match)) {
    stop("Unrecognized response ", dQuote(ans))
  } else {
    c(TRUE, FALSE, NA)[match]
  }
}
