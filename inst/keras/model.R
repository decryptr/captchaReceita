library(keras)
K <- backend()
library(magrittr)
# carregar----------------------------------------------------------------------

arqs <- dir(paste0(system.file(package = "captchaReceitaData"), "/img"), full.names = TRUE)
set.seed(2)
my_sample <- sample(1:length(arqs), length(arqs) * 4 / 5, replace = FALSE)

get_label_from_file_name <- function(filename){
  filename %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    tolower() %>%
    stringr::str_extract('[^_]*$')
}

all_letters <- arqs %>%
  get_label_from_file_name() %>%
  stringr::str_split("") %>%
  unlist() %>%
  unique() %>%
  sort()

create_response <- function(filename, all_letters){
  response_vector <- filename %>%
    get_label_from_file_name() %>%
    stringr::str_split("") %>%
    unlist()

  response <- sapply(response_vector, function(x) as.integer(x == all_letters)) %>%
    t()
  colnames(response) <- all_letters
  response
}

y_train <- arqs[my_sample] %>%
  get_label_from_file_name() %>%
  plyr::laply(create_response, all_letters = all_letters, .progress = "text")

y_test <- arqs[-my_sample] %>%
  get_label_from_file_name() %>%
  plyr::laply(create_response, all_letters = all_letters, .progress = "text")

x_train <- arqs[my_sample] %>%
  plyr::laply(png::readPNG, .progress = 'text')
x_test <- arqs[-my_sample] %>%
  plyr::laply(png::readPNG, .progress = 'text')

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
    filters =  32, kernel_size = c(3,3),
    padding = "same",
    activation = "relu"
  ) %>%
  # layer_max_pooling_2d() %>%
  layer_conv_2d(
    filters = 64, kernel_size = c(3,3),
    padding = "same",
    activation = "relu"
  ) %>%
  layer_max_pooling_2d() %>%
  layer_conv_2d(
    filters = 128, kernel_size = c(5,5),
    padding = "same",
    activation = "relu"
  ) %>%
  layer_max_pooling_2d() %>%
  layer_reshape(c(6, 5760)) %>%
  bidirectional(layer_lstm(units = 512, return_sequences = TRUE),
                merge_mode = "concat") %>%
  layer_dropout(0.1) %>%
  bidirectional(layer_lstm(units = 256, return_sequences = TRUE),
                merge_mode = "concat") %>%
  bidirectional(layer_lstm(units = 128, return_sequences = TRUE),
                merge_mode = "concat") %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(64, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(36, activation = "relu") %>%
  layer_activation("softmax")



# CTC loss ----------------------------------------------------------------
batch_size <- 32L
ctc_loss <- function(y_true, y_pred){

  a <- K$constant(6L, shape = c(batch_size, 1L))

  K$ctc_batch_cost(y_true, y_pred, a, a)
}


# compile-----------------------------------------------------------------------
model %>%
  compile(
    optimizer = "adam",
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
