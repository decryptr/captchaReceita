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


saveRDS(bd, "data-raw/bd.rds")

# Deixar preto e branco ---------------------------------------------------

bd$train$x <- (bd$train$x[,,,1] + bd$train$x[,,,2] + bd$train$x[,,,3])/3
bd$test$x <- (bd$test$x[,,,1] + bd$test$x[,,,2] + bd$test$x[,,,3])/3
dim(bd$train$x) <- c(dim(bd$train$x), 1)
dim(bd$test$x) <- c(dim(bd$test$x), 1)

saveRDS(bd, 'data-raw/bd_pb.rds')


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
