
`$.jack` <-
  `[[.jack` <-
  `[.jack` <-
  function(x, ...) {
    y <- NextMethod(.Generic)
    class(y) <- .Class
    y
  }

print.jack <- function(x, ...) {
  x <- as.data.frame(x)
  NextMethod("print", x)
}

jack <- function(...)
  structure(data.frame(...), class = c("jack", "data.frame"))

j <- jack(cars)
class(j$speed) <- c("numeric", "kcaj")
class(j$dist) <- "kcaj"
head(j)
class(j)
class(j$speed)
class(j$dist)

class(j[1])
class(j[1,])
class(j[1, 1])
class(j[[1]])
class(j[[2]])
class(j$speed)
class(j$dist)
