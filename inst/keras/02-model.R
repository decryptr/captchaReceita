library(keras)
library(tensorflow)
library(purrr)
library(abind)
K <- backend()

# data--------------------------------------------------------------------------

bd <- readRDS('data-raw/bd_pb.rds')
set.seed(1991040)
ind <- sample(1:dim(bd$test$y)[1], 100, replace = FALSE)
y_valid <- bd$test$y[ind,,]
x_valid <- bd$test$x[ind,,,,drop = FALSE]
bd$test$y <- bd$test$y[-ind,,]
bd$test$x <- bd$test$x[-ind,,,,drop = FALSE]

# Padronizar --------------------------------------------------------------

media <- mean(bd$train$x)
desvio <- sd(bd$train$x)
bd$train$x <- (bd$train$x[] - media )/desvio
bd$test$x <- (bd$test$x[] - media )/desvio

# MODELO DO DANIEL WTF ---------------------------------------------------------

model <- keras_model_sequential()
model %>%
  layer_conv_2d(
    input_shape = c(50, 180, 1),
    filters = 16,
    kernel_size = c(5,5),
    padding = "same",
    activation = "relu",
    kernel_regularizer = regularizer_l2(l = 0.1)
  ) %>%
  layer_max_pooling_2d() %>%
  layer_conv_2d(
    filters =  32,
    kernel_size = c(5,5),
    padding = "same",
    activation = "relu",
    kernel_regularizer = regularizer_l2()
  ) %>%
  layer_max_pooling_2d() %>%
  layer_conv_2d(
    filters =  64,
    kernel_size = c(5,5),
    padding = "same",
    activation = "relu"
  ) %>%
  layer_max_pooling_2d() %>%
  layer_flatten() %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 210) %>%
  layer_reshape(target_shape = c(6, 35)) %>%
  layer_activation("softmax")


# compile-----------------------------------------------------------------------
model %>%
  compile(
    optimizer = "adam",
    loss = "categorical_crossentropy",
    metrics = "accuracy"
  )

# fit-------------------------4--------------------------------------------------
model %>%
  fit(
    x = bd$train$x,
    y = bd$train$y,
    batch_size = 32,
    epochs = 100,
    shuffle = TRUE,
    validation_data = list(bd$test$x, bd$test$y)
  )


save_model_hdf5(model, "inst/keras/model_97.hdf5")
