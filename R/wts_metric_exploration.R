
library(tidyverse)


# wts <- abs(rnorm(100000, mean=1, sd=1))
# profit <- abs(rnorm(100000, mean=1, sd=1))
# profit_wts <- profit/ wts
# 
# df <- tibble(wts, profit, profit_wts)
# 
# df %>% ggplot(aes(x=profit, y=wts)) +
#   geom_point(alpha=0.1, color=profit_wts)
# 
# 
# 
# 
# wts <- seq(0,150, 0.2)
# profit_wts <- c(rep(1, length(wts)),rep(10, length(wts)), rep(100, length(wts)) )
# profit_wts <- rep(seq(1,100), length(wts))

df <- expand.grid(wts=seq(0.2,150, 0.2), 
            profit_wts=seq(0.1,100, 0.5))

#df <- tibble(wts = rep(wts,3 ), profit_wts)
df <- df %>% 
  mutate(profit = profit_wts * wts)

df %>% 
  ggplot(aes(x=profit, y=wts, color=profit_wts)) +
  geom_point(alpha=0.1) + 
  scale_colour_gradient(low="blue", high="red") + 
  NULL

df %>% 
  ggplot(aes(x=profit, y=wts, color=profit_wts)) +
  geom_line(alpha=0.1) + 
  scale_colour_gradient(low="blue", high="red") + 
  NULL
