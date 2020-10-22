---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Analysis on expected return vs risk - focus on DJIA. 
draft: false
image: stock.jpg
keywords: ""
slug: DJIA
title: "Risk/Return Analysis on DJIA"
---


We will use the `tidyquant` package to download historical data of stock prices, calculate returns, and examine the distribution of returns. 

We must first identify which stocks we want to download data for, and for this we must know their ticker symbol; Apple is known as AAPL, Microsoft as MSFT, McDonald's as MCD, etc. The file `nyse.csv` contains 508 stocks listed on the NYSE, their ticker `symbol`, `name`, the IPO  (Initial Public Offering) year, and the sector and industry the company is in.


```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- read_csv(here::here("data","nyse.csv"))
```

# Plot number of companies per sector

```{r companies_per_sector}

# YOUR CODE GOES HERE
sector_chart <- nyse %>%
  count(sector) %>%
  arrange(desc(n))

ggplot(sector_chart,aes(x=reorder(sector,-n),y=n))+geom_col() + theme(axis.title = element_text(face="bold"),  title = element_text(face="bold"), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, vjust= 1, hjust= 1)) + labs( title  = "Companies per Sector in the S&P500", x = "Sector" , y = "Count", caption = "Source: NYSE") + NULL

```

Next, let's choose the [Dow Jones Industrial Aveareg (DJIA)](https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average) stocks and their ticker symbols and download some data. Besides the thirty stocks that make up the DJIA, we will also add `SPY` which is an SP500 ETF (Exchange Traded Fund).


```{r, tickers_from_wikipedia}

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers + SPY
tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF

```


```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}

myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2000-01-01",
         to   = "2020-08-31") %>%
  group_by(symbol) 

```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```

Now I will create a dataframe and assign it to a new object, where I'll summarise monthly returns since 2017-01-01 for each of the stocks and `SPY`; min, max, median, mean, SD.

```{r summarise_monthly_returns}
library(kableExtra)

# filter to keep data only after 2017-01-01
monthly_summ <- myStocks_returns_monthly %>%
  filter(date >= "2017-01-01") %>%
  group_by(symbol)

#add new variables for description
monthly_summ_table <- mutate(monthly_summ, min_return = min(monthly_returns), max_return = max(monthly_returns),mean_return = mean(monthly_returns), median_return = median(monthly_returns),  sd_return = sd(monthly_returns)) %>%
  group_by(symbol) 

#summarise and save down as data frame
monthly_summ_1 <- monthly_summ_table %>% 
summarise(min_return = min(monthly_returns), max_return = max(monthly_returns),mean_return = mean(monthly_returns), median_return = median(monthly_returns), sd_return = sd(monthly_returns))

data.frame(monthly_summ_1)

#present as table  
monthly_summ_1 %>%
 #kable (digits=4, align = rep('c',5)) %>%
  #kable_styling() %>% 
  print()

```


Plot a density plot, using `geom_density()`, for each of the stocks

```{r density_monthly_returns}
#ggplot faceted for each stock to show density plot of returns

ggplot(monthly_summ_table, aes(x=monthly_returns, colour = symbol)) +geom_density(alpha=0.3) +facet_wrap("symbol") + theme (axis.title = element_text(face="bold"),  title = element_text(face="bold"), plot.title = element_text(hjust = 0.5) , axis.text.x = element_text(angle = 90, vjust= 1, hjust= 1)) + labs( title  = "Monthly Returns of Companies in DJIA", x = "Monthly Returns" , y = "Density", caption = "Source: DJIA") + NULL
```

What can you infer from this plot?

Majority of stocks average roughly 0% return on a monthly basis. The most risky would be DOW as this has the greatest standard deviation - therefore the most uncertainty around returns. Whereas the SPY ETF has the lowest standard deviation giving more certainty around the return which is slightly above 0% monthly. 

Finally, produce a plot that shows the expected monthly return (mean) of a stock on the Y axis and the risk (standard deviation) in the X-axis. Please use `ggrepel::geom_text_repel()` to label each stock with its ticker symbol

```{r risk_return_plot}
library(ggrepel)

#used monthly summ data frame and specifed mean_return and sd_retur n as x and y axis
ggplot(monthly_summ_1, aes(x= sd_return , y= mean_return, label = symbol)) + geom_point() + geom_text_repel() + theme (axis.title = element_text(face="bold"),  title = element_text(face="bold"), plot.title = element_text(hjust = 0.5)) + labs( title  = "Expected Monthly Returns", x = "Mean Return" , y = "Standard Deviation", caption = "Source: DJIA") + NULL
```

# What does the chart tell us?

The best stocks ar those that offer the highest mean return with the lowest standard deviation - so MSFT would be one of the safer options where as WBA or DOW would be a risker option. For example DOW and BA are significantly more risky than the other stocks that offer similar mean reutrns. 