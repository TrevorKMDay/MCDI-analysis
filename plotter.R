library(ggplot2)

W_0 <- .Machine$double.eps
T_i <- solve.gomp2(y = 1 / exp(1), k_g = .168, A = 1)

ggplot(data.frame(x=c(0, 2)), aes(x)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 50)) +
  geom_hline(yintercept = 1 / exp(1), color = "red") +
  geom_vline(xintercept = T_i, color = "green") +
  stat_function(fun = function(.x) { W_0 * (1 / W_0) ^ (1 - exp(-.168 * .x)) })

T_j <- solve.gomp2(y = 0.5 / exp(1), k_g = .168, A = 1)
T_k <- solve.gomp2(y = 0.25 / exp(1), k_g = .168, A = 1)

# .184 = a * exp(c * 18.2 + d)
# .368 = a * exp(c * 21.3 + d)



x <- seq(0, 22, by = .0001)
y <- sapply(x, function(x) solve.gomp2(x = x, k_g = .168, A = 1))

solve <- nls(y ~ a * exp(b * x),
          data = data.frame(x = x, y = y),
          start = list(a = .001, b = .2),
          control = list(maxiter = 100))

ggplot(data.frame(x=c(0, 2)), aes(x)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 50)) +
  geom_hline(yintercept = 1 / exp(1), color = "red") +
  geom_vline(xintercept = T_i, color = "green") +
  stat_function(fun = function(.x) { W_0 * (1 / W_0) ^ (1 - exp(-.168 * .x)) }) +
  stat_function(fun = function(.x) coef(solve)[1] * exp(coef(solve)[2] * .x),
                color = "red", size = 1, linetype = "dashed") +
  stat_function(fun = function(.x) .0000015 * exp(.608 * .x),
                color = "green", size = 1, linetype = 2)
