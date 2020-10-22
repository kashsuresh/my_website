---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
image: omega.jpg
keywords: ""
slug: Omega
title: "Omega Group Pay Discrimination Analysis"
---

At the last board meeting of Omega Group Plc., the headquarters of a large multinational company, the issue was raised that women were being discriminated in the company, in the sense that the salaries were not the same for male and female executives. A quick analysis of a sample of 50 employees (of which 24 men and 26 women) revealed that the average salary for men was about 8,700 higher than for women. This seemed like a considerable difference, so it was decided that a further analysis of the company salaries was warranted. 

You are asked to carry out the analysis. The objective is to find out whether there is indeed a significant difference between the salaries of men and women, and whether the difference is due to discrimination or whether it is based on another, possibly valid, determining factor. 

## Loading the data


```{r load_omega_data}
omega <- read_csv(here::here("data", "omega.csv"))
glimpse(omega) # examine the data frame
```

## Relationship Salary - Gender ?

Calculate summary statistics on salary by gender. Also, create and print a dataframe where, for each gender, you show the mean, SD, sample size, the t-critical, the SE, the margin of error, and the low/high endpoints of a 95% condifence interval

```{r, confint_single_valiables}
# Summary Statistics of salary by gender
mosaic::favstats (salary ~ gender, data=omega)

# Dataframe with two rows (male-female) and having as columns gender, mean, SD, sample size, 
# the t-critical value, the standard error, the margin of error, 
# and the low/high endpoints of a 95% condifence interval
stats_omega <- omega %>%
group_by(gender) %>%
summarise(mean=mean(salary), sd=sd(salary), ssize=n(),t_crit=abs(qt(0.05/2, ssize-1)), se_salary= sd/sqrt(ssize), margin_of_error= qt(0.975,ssize-1) * se_salary, conf_low=mean-margin_of_error, conf_high=mean+margin_of_error)
  
print(stats_omega)


```

> As the low and high points of the confidence intervals don't overlap, there appears to be a significant difference. Only if there was an overlap between confidence intervals could one argue that they are indifferent.

Using t.test and infer package to hypthesis test

```{r, hypothesis_testing}
# hypothesis testing using t.test() 

t.test(salary ~ gender, 
       data = omega)

#as p-value is smaller than 0.05, we can reject the null hypothesis and conclude that the mean salary between men and women is significantly different. 

# hypothesis testing using infer package

gender_stat <- omega %>% 
specify(salary ~ gender) %>% 
calculate("diff in means", 
            order = c("male", "female"))

gender_null <- omega %>% 
specify(salary ~ gender) %>% 
hypothesize(null = "independence") %>% 
generate(reps = 10000, type="permute") %>%  
calculate("diff in means", 
            order = c("male", "female"))

#obtaining p value
gender_null %>% 
  get_pvalue(obs_stat = gender_stat, direction = "both")

#visualising in chart
gender_null %>% 
  visualize(obs_stat = gender_stat) + 
  labs(x = "Difference in salary by gender (male-female)",
       y = "Count",
       subtitle = "Red line shows observed difference in mean proportions") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
```

> Again, as p-value is smaller than 0.05, we can reject the null hypothesis and conclude that the mean salary between men and women is significantly different.


## Relationship Experience - Gender?

At the board meeting, someone raised the issue that there was indeed a substantial difference between male and female salaries, but that this was attributable to other reasons such as differences in experience. A questionnaire send out to the 50 executives in the sample reveals that the average experience of the men is approximately 21 years, whereas the women only have about 7 years experience on average (see table below).

```{r, experience_stats}
# Summary Statistics of salary by gender
favstats (experience ~ gender, data=omega)

```

Analysing the data set using infer and t.test to see if there is also a significant difference. 
```{r}
t.test(experience ~ gender, 
       data = omega)

#as p-value is smaller than 0.05, we can reject the null hypothesis and conclude that the mean experience between men and women is significantly different.

# hypothesis testing using infer package

experience_stat <- omega %>% 
specify(experience ~ gender) %>% 
calculate("diff in means", 
            order = c("male", "female"))

exp_null <- omega %>% 
specify(experience ~ gender) %>% 
hypothesize(null = "independence") %>% 
generate(reps = 1000, type="permute") %>%  
calculate(stat= "diff in means", 
            order = c("male", "female"))

#obtaining p value with get_pvalue

exp_null %>% 
  get_pvalue(obs_stat = experience_stat, direction = "both")

#graphing data

exp_null %>% 
  visualize(obs_stat = experience_stat) + 
  labs(x = "Difference in experience by gender (male-female)",
       y = "Count",
       subtitle = "Red line shows observed difference in mean proportions") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
```



## Relationship Salary - Experience ?

Someone at the meeting argues that clearly, a more thorough analysis of the relationship between salary and experience is required before any conclusion can be drawn about whether there is any gender-based salary discrimination in the company.

scatterplot to analyse the relationship between salary and experience


```{r, salary_exp_scatter}

ggplot(omega, aes(x=experience, y=salary, colour = gender)) + geom_point() + labs(title="Relationship between salary and experience", y= "Salary", x="Experience in years") 

#most females have less experience and may thus earn less, as can be seen towards the left side of the chart. There are limited data points of females with more experience, however those within the set scatter within the male population and not visually significantly below, however neither do they appear to be top earners for their experience group.
```


## Check correlations between the data

```{r, ggpairs}
omega %>% 
  select(gender, experience, salary) %>% #order variables they will appear in ggpairs()
  ggpairs(aes(colour=gender, alpha = 0.3))+
  theme_bw()
```

> most females have less experience and may thus earn less, as can be seen towards the left side of the chart. There are limited data points of females with more experience, however those within the set scatter within the male population and not visually significantly below, however neither do they appear to be top earners for their experience group.