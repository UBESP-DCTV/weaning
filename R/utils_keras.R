# layer_rescale_1d <- keras::new_layer_class(
#   classname = "rescale1d",
#
#   initialize = function(scalars) {
#     super$initialize()
#     self$scalars <- as.double(scalars)
#   },
#
#   build = function(input_shape) {
#     input_dim <- input_shape[length(input_shape)]
#     self$W <- tf$constant(
#       rep(self$scalars, input_shape[[1]]),
#       shape = input_shape
#     )
#   },
#
#   call = function(inputs) {
#     tf$multiply(inputs, self$W)
#   }
# )
