library(magrittr)
# carregar----------------------------------------------------------------------

arqs <- dir(paste0(system.file(package = "captchaReceitaData"), "/img"), full.names = TRUE) %>%
  stringr::str_subset('_[0-9a-z]+\\.png$')
set.seed(2)
my_sample <- sample(1:length(arqs), 500, replace = FALSE)

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

x_train <- array(NA, dim = c(length(arqs[-my_sample]), 50, 180, 1))
for(i in 1:length(arqs[-my_sample])){
  im <- png::readPNG(arqs[-my_sample][i])
  im <- (im[,,1] + im[,,2] + im[,,3])/3
  dim(im) <- c(50, 180, 1)
  x_train[i,,,] <- im
}

x_test <- array(NA, dim = c(length(arqs[my_sample]), 50, 180, 1))
for(i in 1:length(arqs[my_sample])){
  im <- png::readPNG(arqs[my_sample][i])
  im <- (im[,,1] + im[,,2] + im[,,3])/3
  dim(im) <- c(50, 180, 1)
  x_test[i,,,] <- im
}


y_train <- array(NA, dim = c(length(arqs[-my_sample]), 6, length(all_letters)),
                 dimnames = list(
                  NULL, NULL , all_letters
                 ))
for(i in 1:length(arqs[-my_sample])){
  y_train[i,,] <- create_response(arqs[-my_sample][i], all_letters)
}


y_test <- array(NA, dim = c(length(arqs[my_sample]), 6, length(all_letters)),
                 dimnames = list(
                   NULL, NULL , all_letters
                 ))
for(i in 1:length(arqs[my_sample])){
  y_test[i,,] <- create_response(arqs[my_sample][i], all_letters)
}

bd <- list(
  train = list(
    x = x_train,
    y = y_train
  ),
  test = list(
    x = x_test,
    y = y_test
  )
)


saveRDS(bd, "data-raw/bd_pb.rds")



# # resultados--------------------------------------------------------------------
# probabilidades <- predict(model, x_valid)
# predicoes <- apply(probabilidades, c(1, 2), which.max)
# y_valid_obs <- apply(y_valid, c(1, 2), which.max)
# mean(abs(probabilidades - y_valid))
# mean(predicoes == y_valid_obs)
# sapply(1:6, function(l) {mean(predicoes[,l] == y_valid_obs[,l])})
#
# # predizer----------------------------------------------------------------------
# ## captchaTJMGAudio::baixar_imgs_audios(10)
# arqs_i <- dir('~/curso-r/slides/data/captchas', full.names = TRUE, pattern = '\\.png')
# i <- 1
# arq <- arqs_i[i]
# plot(magick::image_read(arq))
# i <- i + 1
# predizer(arq)
