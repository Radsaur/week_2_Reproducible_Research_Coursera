# week_2_Reproducible_Research_Coursera


## Introduction

As per the assignment we have to create an RMarkdown document to analyze a number of steps performed by individuals. In order to work as effective as R permits we have to upload certain pacakges. The R code shows whcihc packages were uploaded:


```{r, results='hide'}

library(dplyr)
library(tidyverse)
library(ggplot2)
library(xtable)
library(lubridate)
```


## 1. Code for reading in the dataset and/or processing the data
In order to read the dataset, we first set the working directroey and then command R to read the file.
```{r, echo=TRUE}
setwd('D:/DOCS/Sauran.S/R/Coursera/Literate_programming')
df<- read.csv('activity.csv')
summary(df) 

```

## 2. Histogram of the total number of steps taken each day
Below we present the histogram of the total numbe of steps. In order to plot the histogram, some data preprocessing has to be done first. Since the tasks requests that we plot the total number of steps, we have to sum up the number of steps per each day. In the code below, the first line calculates the number of steps per day and the second one plots the histogram.
```{r pressure, cache=TRUE}

tot_steps<-df %>% group_by(date) %>% summarize(steps=sum(steps,na.rm = T))

tot_steps$steps %>% hist(main='Total number of steps',xlab='steps per day' ,ylim=c(0,28))

```
![https://github.com/Radsaur/week_2_Reproducible_Research_Coursera/blob/main/ss_files/figure-html/unnamed-chunk-10-1.png](week_2_Reproducible_Research_Coursera/ss_files/figure-html/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

## 3.Mean and median number of steps taken each day
The code below shows how we calculated the mean and median number of steps per day
```{r, cache=TRUE}
mean_number<-tot_steps$steps %>% mean(na.rm=T) %>% round(2)
median_number<-tot_steps$steps %>% median(na.rm=T)%>% round(2)

```

After evaluation, the code above states that the mean of the total number of steps is `r mean_number` and the meadian is `r median_number`

## 4. Time series plot of the average number of steps taken
Following the instructions we do a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r, cache=TRUE}
df %>% group_by(interval) %>% 
    summarize(steps=mean(steps,na.rm = T))->intervals 

intervals %>% ggplot(aes(x=interval, y=steps)) + 
    geom_line()+labs(title = 'average number of per inerval')

```

## 5. The 5-minute interval that, on average, contains the maximum number of steps
The interval with the maximum number of steps can be found as follows:
```{r}
max_interv<- intervals[intervals$steps==intervals$steps %>% max,1]
```
So, the max number of steps inteval is `r max_interv`


## 6. Code to describe and show a strategy for imputing missing data

##### We have to calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
First, we look at the composition of dataset using the function, 'str'. It shows that the only row which contains the NAs is "steps". So we calculate the number of missing values in "steps" as follows
```{r}
df %>% str

NA_numb<- is.na(df$steps) %>% sum()
```
So the number of missing values is `r NA_numb`

##### The second taks is to devise a strategy for filling in all of the missing values in the dataset.
In order to imput missThe package creates multiple imputations (replacement values) for multivariate missing data. The method is based on Fully Conditional Specification, where each incomplete variable is imputed by a separate model. 
So we activate the package.
```{r,results='hide', warning=FALSE}
library(mice)
```
Then we use the package to impute the missing values
```{r, cache= TRUE, results='hide'}
imputed_Data <- mice(df, maxit = 10, method = 'pmm', seed = 500,printFlag = F)
imputed_Data$imp$steps
completeData <- complete(imputed_Data,2)
```
Let's now check whether the function 'str' will show any NA's
```{r}
completeData %>% str
```
So, this time no NAs are not displayed. 

## 7. Histogram of the total number of steps taken each day after missing values are imputed

Now, it is time to re-do the histogram and check whether it significantly differs from the histogram above which does contain missing values. 
```{r, cache=TRUE}
group_steps<-completeData %>%group_by(date) %>% summarise(steps=sum(steps))  
group_steps$steps %>% hist(main='Total number of steps after imputting NAs',xlab='steps per day')


```
The histogram looks quite similar to the histogram above. 

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
The next task is to check whether the walking patter differs betweek working days and week ends.
First, we segregate the data frame into working days and weekend days 
```{r, cache=TRUE}
completeData$date<-completeData$date %>% ymd()
completeData$wday<-ifelse(completeData$date %>% wday()>5,'week_end','week_day')

completeData$wday %>% unique()
```

Second, we calculate the mean of the steps calculated per interval across all days and plot the panel data.
```{r, cache=TRUE}
av_steps_weekdays<-completeData %>% group_by(interval,wday) %>% summarise(steps=mean(steps))

ggplot(av_steps_weekdays, aes(interval, steps,color=wday))+geom_line()+
    labs(title = 'Panel plot weekend vs working days')
```



