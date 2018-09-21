test = tibble(x = 1:3, f = c("sin", "cos", "sqrt"))

test_ = mutate(test, ans = pmap_dbl(list(x = x, y = f), ~ do.call(.y, list(.x))))


test2 = tibble(mu = 1:3, sd = c(0.001, 0.001, 0.001), f = c("rnorm", "rnorm", "rnorm"))
test2_ = mutate(test2, ans = pmap_dbl(list(mu, sd, f), ~ do.call(..3, list(mu = ..1, sd = ..2))))

test2_ = mutate(test2, ans = pmap_chr(list(mu, sd, f), ~ paste0(..1, ..2, ..3)))
test2_

test2_ = mutate(test2, ans = pmap_dbl(list(mu, sd, f), ~ do.call(..3, list(n = 1, mean = ..1, sd = ..2))))
test2_
