predizer <- function(arq) {
  all_letters <- structure(
    1:35,
    .Label = c("1", "2", "3", "4", "5", "6", "7",
               "8", "9", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
               "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
               "y", "z"),
    class = "factor"
  )
  x <- png::readPNG(arq)
  dim(x) <- c(1, dim(x))
  a <- predict(model, x)
  z <- as.numeric(apply(a, c(1, 2), which.max))
  paste(all_letters[z], collapse = '')
}
