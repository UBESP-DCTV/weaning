library(tidyverse)
library(targets)


a <- tr_generator()
b <- tr_generator()
c <- tr_generator()
d <- tr_generator()
e <- tr_generator()

tr_list <- list(a, b, c, d, e)

walk(tr_list, str, 1)
walk(tr_list, ~str(.x[["x"]], 1))

f <- val_generator()
g <- val_generator()
h <- val_generator()
i <- val_generator()
l <- val_generator()

val_list <- list(f, g, h, i, l)
walk(val_list, str, 1)
walk(val_list, ~str(.x[["x"]], 1))
