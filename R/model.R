#' DogNet
#'
#' nn_module that is just a resnet18 with a custom endpoint with 'classes' output.
#'
#' @param classes (int) number of output probabilities.
#' @param freeze (bool) should resnet18 weights be frozen?
#' @param device (chr or torch_device)
#'
#' @export
nn_dog <- torch::nn_module(
  "DogNet",
  initialize = function(classes = 100, freeze = TRUE, device = "cpu") {
    resnet18 <- torchvision::model_resnet18(pretrained = TRUE)$to(device = device)

    if(freeze)
      resnet18$parameters %>% purrr::walk(function(param) param$requires_grad_(FALSE))

    resnet18$fc <- torch::nn_linear(
      in_features = resnet18$fc$in_features, # 1000
      out_features = classes # 100
    )$to(device = device)
    self$resnet18 <- resnet18
  },

  forward = function(x) {
    self$resnet18(x)
  }
)
