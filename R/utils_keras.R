# layer_rescale_1d <- keras::new_layer_class(
#   classname = "rescale1d",
#
#   initialize = function(scalars) {
#     super$initialize()
#     self$scalars <- scalars
#   },
#
#   build = function(input_shape) {
#     input_dim <- input_shape[length(input_shape)]
#     self$W <- tf$constant(self$scalars)
#   },
#
#   call = function(inputs) {
#     self$W <- tf$cast(self$W, inputs$dtype)
#     tf$multiply(inputs, self$W)
#   }
# )
