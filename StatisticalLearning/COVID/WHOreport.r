library(tidyverse)


who_report <- read_csv('WHOreport.csv')

view(who_report)

who_report[is.na(who_report)] = 0

head(who_report)

who_report %>%
  filter(location %in% c('United States', 'South Korea')) %>%
  group_by(date) %>%
  ggplot(aes(x = date,
             y = new_cases,
             color = factor(location))) +
  geom_point()


#filter report for a number of notable countries
who_report %>%
  filter(location %in% c('United States', 'South Korea', 'China', 'Italy')) %>%
  group_by(date) %>%
  ggplot(aes(x = date,
             y = total_cases,
             color = factor(location))) +
  geom_point()


who_report_us <- who_report %>%
  filter(location %in% c('United States')) %>%
  mutate(date_num = row_number())

us_total_cases_plot <- who_report %>%
  filter(location %in% c('United States')) %>%
  mutate(date_num = row_number()) %>%
  ggplot(aes(x = date_num,
             y = total_cases)) +
  geom_point()

# use kernel smoothing to approximate total cases curve in the US
k1 <- ksmooth(x = who_report_us$date_num,
              y = who_report_us$total_cases,
              kernel = 'normal',
              bandwidth = 5,
              x.points = who_report_us$date_num)

k1

# create plot
us_total_cases_plot +
  geom_line(data = data.frame(x = k1$x,
                              y = k1$y),
            mapping = aes(x = x,
                          y = y),
            color = 'blue') +
  ggtitle('US Total Cases Plot, Pre-March17, 2020')


# quantify the error of the smoothing function

#kernel smoothing predictions
k1$y

#reported total cases
us_total_cases <- who_report_us[,c('total_cases')]

#calculating rmse
us_total_cases_rmse <- sqrt(mean(sum((k1$y - us_total_cases)^2)))

#Can we reduce rmse by changing bandwidth?

tot_rmse <- NULL

for(i in 1:20) {
  ks_total_us_cases <- ksmooth(x = who_report_us$date_num,
                               y = who_report_us$total_cases,
                               kernel = 'normal',
                               bandwidth = i,
                               x.points = who_report_us$date_num)
  
  tot_rmse[i] <- sqrt(mean(sum((ks_total_us_cases$y - us_total_cases)^2)))
}

#we see a trade-off between reducing bandwidth, which raises our bias, and increasing bandwidth, which increases our total rmse
plot(tot_rmse) +
  title('RMSE for Different Bandwidths')


#Can we perform LOOCV? We could try for bandwidths 1,2,3,4,5
who_report_us$date_num


loocv_rmse <- NULL

#different bandwidths
for(j in 1:10) {
  
  error <- NULL
  #leaving one row out
  for(i in 1:max(who_report_us$date_num)) {
    
    current_test <- who_report_us %>%
      filter(date_num == i) %>%
      select('total_cases')
    
    train_set <- who_report_us %>%
      filter(date_num != i)
    
    current_model <- ksmooth(x = train_set$date_num,
                             y = train_set$total_cases,
                             kernel = 'normal',
                             bandwidth = j,
                             x.points = i)
    
    error[i] <- current_model$y - current_test$total_cases
  }
  
  loocv_rmse[j] = sqrt(mean(sum((error)^2)))

}

#visualizing the different LOOCV errors
plot(loocv_rmse) +
  title("LOOCV RMSE by Different Kernel Smoothing Bandwidths")


ggplot(data = data.frame(x = factor(c(1:10)),
                         y = loocv_rmse),
       mapping = aes(x = x,
                     y = y)) +
  geom_point() +
  xlab('Bandwidth') +
  ylab('RMSE') +
  ggtitle("LOOCV RMSE by Different Kernel Smoothing Bandwidths") +
  theme_gray()




## Let's see how a smoothing spline performs by comparison

library(splines)

who_report_us_indexes <- sample(nrow(who_report_us),
                                .7 * nrow(who_report_us),
                                replace = FALSE)

who_report_us_train <- who_report_us[who_report_us_indexes,]
who_report_us_test <- who_report_us[-who_report_us_indexes,]


cs_total_cases_us <- lm(total_cases ~ bs(who_report_us$date_num, knots = c(5, 25, 50)),
                        data = who_report_us)

summary(cs_total_cases_us)

?bs

dates.grid <- 1:(max(who_report_us$date_num))


preds <- predict(cs_total_cases_us, data.frame(dates = dates.grid))

us_total_cases_plot +
  geom_line(data = data.frame(dates = dates.grid,
                           preds = preds),
         mapping = aes(x = dates,
                       y = preds),
         color = 'blue')


cs_rmse <- sqrt(mean(sum((preds - us_total_cases)^2)))

#much less than the kernel smoothing, but probably in large part due to leaving out the last point!
cs_rmse



#let's try a smoothing spline

ss_total_cases_us <- smooth.spline(who_report_us$date_num,
                                   who_report_us$total_cases,
                                   lambda = .000001)

#seems to drastically overfit our data
us_total_cases_plot +
  geom_line(data = data.frame(date_num = ss_total_cases_us$x ,
                              total_cases = ss_total_cases_us$y),
            color = 'blue')


 




