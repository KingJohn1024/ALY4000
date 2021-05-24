library(scales)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)
theme_set(theme_minimal())
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

tickers <- c('FB', 'AMZN', 'NFLX', 'GOOG')
weights <- c(.25, .25, .25, .25)

sd <- '2019-01-01'
ed <- '2021-05-21'

# Gets quantitative data in "tibble" format
prices <- tq_get(tickers, 
                 from = sd, 
                 to   = ed, 
                 get  = 'stock.prices')
head(prices)

# Convert daily prices to returns
returns <- prices %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = 'daily',
               col_rename = 'rtn')
returns

# Generate portfolio returns based on simple stock returns and weights
port_returns <- returns %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = rtn, 
               weights     = weights, 
               col_rename  = 'PortRet')

# Generate benchmark returns
bmark_returns <- 'SPY' %>%
  tq_get(get  = 'stock.prices', 
         from = sd, 
         to   = ed) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = 'daily', 
               col_rename = 'BenchRet')

# left join port returns with benchmark return
ff <- left_join(port_returns, 
                bmark_returns,
                by = "date")
ff

# Tidy up the data by pivoting the port & bench return columns to labeled rows
xf <- ff %>% 
  pivot_longer(PortRet:BenchRet, 
               names_to  = 'Type', 
               values_to = 'DailyRets')

# Compute the cumulative returns
xf <- xf %>%
  group_by(Type) %>%
  mutate(Rets = (cumprod(1+(DailyRets))-1))

ggplot(xf, aes(x = date, y = Rets, color = Type)) + 
  geom_point(size = .75) +
  geom_line() +
  scale_y_continuous(labels=percent) +
  geom_text(data = filter(xf, date==max(date)), 
            aes(label=percent(Rets)), 
            color = 'black', 
            hjust = -.1) +
  scale_color_brewer(palette = 'Dark2') +
  ggtitle('Portfolio vs Benchmark Returns') + 
  theme(plot.title = element_text(hjust = 0.5))

tail(xf)

# Reference article
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ05-performance-analysis-with-tidyquant.html