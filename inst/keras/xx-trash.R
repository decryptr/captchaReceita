cortes <- purrr::rerun(5, sample(-10:10, 5)) %>%
  purrr::map(~.x + c(40, 65, 100, 130, 160))

cut_imgs <- function(imgs, cuts){
  cuts <- c(1, cuts, 180)
  im <- map(1:6, ~cuts[.x:(.x+1)]) %>%
    map(~imgs[,,seq(.x[1], .x[2], length.out = 30) %>% trunc(),])
  abind::abind(im, along = 1)
}

transformar_imgs <- function(imgs, cuts){
  all <- lapply(cuts, function(x){
    cut_imgs(imgs, x)
  })
  all <- abind::abind(all, along = 1)
  all
}

transformar_labels <- function(y, num_cuts){
  a <- lapply(1:6, function(x) y[,x,]) %>% abind::abind(along = 1)
  lapply(1:num_cuts, function(x) a) %>% abind::abind(along = 1)
}

x_train <- transformar_imgs(bd$train$x, cortes)
x_test <- transformar_imgs(bd$test$x, cortes)
dim(x_train) <- c(dim(x_train), 1)
dim(x_test) <- c(dim(x_test), 1)
y_train <- transformar_labels(bd$train$y, 5)
y_test <-  transformar_labels(bd$test$y, 5)



# Padronizar x e y --------------------------------------------------------

media <- mean(x_train)
desvio <- sd(x_train)

x_train <- (x_train[] - media)/desvio
x_test <- (x_test[] - media)/desvio

rm(bd)
