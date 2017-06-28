library(keras)
# carregar----------------------------------------------------------------------
arqs <- dir('~/decryptr/captchaReceitaData/inst/img', full.names = TRUE)
set.seed(2)
my_sample <- sample(1:length(arqs), length(arqs) * 4 / 5, replace = FALSE)

all_letters <- arqs %>%
  basename() %>%
  tools::file_path_sans_ext() %>%
  tolower() %>%
  stringr::str_split('') %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  factor()

create_response <- function(x) {
  a <- strsplit(x, '')[[1]]
  if (length(unique(a)) == 1) {
    mm <- matrix(rep(1, 6), ncol = 1)
  } else {
    mm <- model.matrix(rep(1, 6) ~ a - 1)
  }
  m <- matrix(0L, nrow = 6, ncol = 35)
  colnames(m) <- all_letters
  sua <- sort(unique(a))
  colnames(mm) <- sua
  m[, sua] <- mm
  attributes(m) <- list(dim = c(6, 35))
  m
}

y_train <- arqs[my_sample] %>%
  basename() %>%
  tools::file_path_sans_ext() %>%
  tolower() %>%
  plyr::laply(create_response, .progress = 'text')
y_test <- arqs[-my_sample] %>%
  basename() %>%
  tools::file_path_sans_ext() %>%
  tolower() %>%
  plyr::laply(create_response, .progress = 'text')
x_train <- arqs[my_sample] %>%
  plyr::laply(function(x) png::readPNG(x), .progress = 'text')
x_test <- arqs[-my_sample] %>%
  plyr::laply(function(x) png::readPNG(x), .progress = 'text')

d <- tibble::tibble(
  nome = c('y_train', 'y_test', 'x_train', 'x_test'),
  bd = list(y_train = y_train, y_test = y_test,
            x_train = x_train, x_test = x_test)
)
saveRDS(d, 'data-raw/bd.rds')

# data--------------------------------------------------------------------------
bd <- readRDS('data-raw/bd.rds')
set.seed(1991040)
ind <- sample(1:dim(bd$bd$y_test)[1], 100, replace = FALSE)
y_valid <- bd$bd$y_test[ind,,]
x_valid <- bd$bd$x_test[ind,,,]
bd$bd$y_test <- bd$bd$y_test[-ind,,]
bd$bd$x_test <- bd$bd$x_test[-ind,,,]

# MODELO DO DANIEL WTF ---------------------------------------------------------

model <- keras_model_sequential()
model %>%
  layer_conv_2d(
    input_shape = c(50,180,4),
    filters = 2 * 2, kernel_size = c(2,2),
    padding = "same",
    activation = "relu",
    strides = list(1, 1)
  ) %>%
  layer_max_pooling_2d() %>%
  layer_conv_2d(
    filters = 3 * 3, kernel_size = c(3,3),
    padding = "same",
    activation = "relu"
  ) %>%
  # layer_max_pooling_2d() %>%
  layer_conv_2d(
    filters = 3 * 3, kernel_size = c(3,3),
    padding = "same",
    activation = "relu"
  ) %>%
  layer_max_pooling_2d() %>%
  layer_conv_2d(
    filters = 5 * 5, kernel_size = c(5,5),
    padding = "same",
    activation = "relu"
  ) %>%
  layer_max_pooling_2d() %>%
  layer_reshape(c(6, 22 * 25)) %>%
  bidirectional(layer_lstm(units = 512, return_sequences = TRUE),
                merge_mode = "concat") %>%
  layer_dropout(0.1) %>%
  bidirectional(layer_lstm(units = 256, return_sequences = TRUE),
                merge_mode = "concat") %>%
  bidirectional(layer_lstm(units = 128, return_sequences = TRUE),
                merge_mode = "concat") %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.1) %>%
  layer_dense(64, activation = "relu") %>%
  layer_dropout(0.1) %>%
  layer_dense(35, activation = "relu") %>%
  layer_activation("softmax")

# compile-----------------------------------------------------------------------
model %>%
  compile(
    optimizer = "adagrad",
    loss = "categorical_crossentropy",
    metrics = "accuracy"
  )

# fit---------------------------------------------------------------------------
model %>%
  fit(
    x = bd$bd$x_train,
    y = bd$bd$y_train,
    batch_size = 16,
    epochs = 100,
    validation_data = list(bd$bd$x_test, bd$bd$y_test)
  )

# resultados--------------------------------------------------------------------
probabilidades <- predict(model, x_valid)
predicoes <- apply(probabilidades, c(1, 2), which.max)
y_valid_obs <- apply(y_valid, c(1, 2), which.max)
mean(abs(probabilidades - y_valid))
mean(predicoes == y_valid_obs)
sapply(1:6, function(l) {mean(predicoes[,l] == y_valid_obs[,l])})

# predizer----------------------------------------------------------------------
## captchaTJMGAudio::baixar_imgs_audios(10)
arqs_i <- dir('~/curso-r/slides/data/captchas', full.names = TRUE, pattern = '\\.png')
i <- 1
arq <- arqs_i[i]
plot(magick::image_read(arq))
i <- i + 1
predizer(arq)
