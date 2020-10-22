---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
description: Nullam et orci eu lorem consequat tincidunt vivamus et sagittis magna sed nunc rhoncus condimentum sem. In efficitur ligula tate urna. Maecenas massa sed magna lacinia magna pellentesque lorem ipsum dolor. Nullam et orci eu lorem consequat tincidunt. Vivamus et sagittis tempus.
draft: false
image: trump.jpg
keywords: ""
slug: trump
title: "Donald Trump Ratings Approval Over Time"
---

I got the data from fivethirtyeight.com as it has detailed data on [all polls that track the president's approval ](https://projects.fivethirtyeight.com/trump-approval-ratings)

```{r, cache=TRUE}
# Import approval polls data
approval_polllist <- read_csv(here::here('data', 'approval_polllist.csv'))


# Use `lubridate` to fix dates, as they are given as characters.

approval_polllist$modeldate <- mdy(approval_polllist$modeldate)
approval_polllist$startdate <- mdy(approval_polllist$startdate)
approval_polllist$enddate <- mdy(approval_polllist$enddate)
approval_polllist$createddate <- mdy(approval_polllist$createddate)
approval_polllist$timestamp <- parse_date_time(approval_polllist$timestamp, "hmsdmy")
glimpse(approval_polllist)

```

## Create a plot

I want to calculate the average net approval rate (approve- disapprove) for each week since he got into office. Then I shall plot the net approval, along with its 95% confidence interval. There are various dates given for each poll, I will use `enddate`, i.e., the date the poll ended.


```{r trump_margins_completed, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "trump_approval_margin.png"), error = FALSE)


net_approval <- approval_polllist %>%
  filter(subgroup == "Voters") %>%
mutate(year=year(enddate), week=isoweek(enddate), approval_margin=approve - disapprove)

nap_data <- net_approval %>%
group_by(year, week) %>%
summarise(mean_approval_margin = mean(approval_margin),
            sd_approval_margin = sd(approval_margin),
            count = n(),
            se_approval_margin = sd(approval_margin)/sqrt(count),
            margin_of_error = qt(0.975,count-1) * se_approval_margin,
            approval_margin_low = mean_approval_margin - margin_of_error,
            approval_margin_high = mean_approval_margin + margin_of_error) %>%
  filter(count>1)


ggplot(nap_data, aes(x=week, fill=as.factor(year), y=mean_approval_margin, colour=as.factor(year))) + theme(legend.position ="none") + scale_fill_manual(values=c("red","darkolivegreen2","#66FFFF", "#E5CCFF")) +  geom_ribbon(aes(ymin= approval_margin_low, ymax= approval_margin_high), alpha=0.2) + facet_wrap(~ year) + geom_line(size=0.3) + geom_point(size=0.4) + geom_hline(yintercept=0, colour="orange")  + theme_bw(base_size = 7) + labs(title="Estimating Net Approval (approve-disapprove) for Donald Trump", subtitle = "Weekly average of all polls", y= "Average net Approval (%)", x="Week of the year") + theme(legend.position ="none", plot.title = element_text(size=8, face="bold")) + scale_y_continuous(breaks=c(-20, -17.5, -15, -12.5, -10, -7.5, -5, -2.5, 0, 2.5, 5, 7.5)) + scale_x_continuous(breaks=c(0, 13, 26, 39, 52))


```


## Compare Confidence Intervals

Compare the confidence intervals for `week 15` (6-12 April 2020) and `week 34` (17-23 August 2020). Can you explain what's going on? One paragraph would be enough.
> In week 15, as also visible on the chart, the standard error is quite small (at around 1), while in week 34, the standard error is significantly bigger (at around 2.6). This results in a bigger range of the 95% confidence interval from the mean approval rate. In terms of what happened, this is largely to be brought back that people have less of a same opinion on Trump and thus a larger range of answers was collected form the polls, which led to a greater range in the 95% confidence interval. This could be due to political actions from Trump that have strongly divided opinion in the population. Here specifically it could be let to the Black Lives Matter movement and police brutality in the States, which started in July and reached peak in August. Thus, as people have very divided opinions, ultimately this reflects in the confidence interval for that period of time in week 34. 