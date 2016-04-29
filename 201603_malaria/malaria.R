library(foreign)
library(dplyr)
library(pipeR)
library(foreach)
library(ggplot2)

origin_data <- read.dta('~/malaria.dta')

##########################################################

regression_result <- foreach(i = 1825:1955,.combine = "rbind") %do% {
  test <- filter(origin_data, yob == i)
  fit_occ <- lm(occscore ~ malmort1890 + lebergott99 + south + hookworm + year, data=test, weights=test$cellsize)
  coef_occ <- summary(fit_occ)$coefficients[2,1]
  se_occ <- summary(fit_occ)$coefficients[2,2]
  
  fit_sei <- lm(sei ~ malmort1890 + lebergott99 + south + hookworm + year, data=test, weights=test$cellsize)
  coef_sei <- summary(fit_sei)$coefficients[2,1]
  se_sei <- summary(fit_sei)$coefficients[2,2]
  cbind(coef_occ,se_occ,coef_sei,se_sei,i)
}

regression_result <- as.data.frame(regression_result)
names(regression_result)[5] <- "yob"

regression_result %>>%
  mutate(pre = as.numeric(yob >= 1825 & yob <= 1900)) %>>%
  mutate(mid = as.numeric(yob >= 1900 & yob <= 1920)) %>>%
  mutate(post = as.numeric(yob >= 1920 & yob <= 1960)) %>>%
  mutate(upper_occ = coef_occ + 1.96 * se_occ) %>>%
  mutate(upper_sei = coef_sei + 1.96 * se_sei) %>>%
  mutate(lower_occ = coef_occ - 1.96 * se_occ) %>>%
  mutate(lower_sei = coef_sei - 1.96 * se_sei) %>>%
  as.data.frame() %>>%
  ~ first_step_data

first_step_data %>>%
  filter(pre == 1) %>>%
  mutate(avg_occ = mean(coef_occ)) %>>%
  mutate(avg_sei = mean(coef_sei)) %>>%
  ~ pre_data

first_step_data %>>%
  filter(post == 1) %>>%
  mutate(avg_occ = mean(coef_occ)) %>>%
  mutate(avg_sei = mean(coef_sei)) %>>%
  ~ post_data

first_step_data %>>%
  filter(mid == 1) %>>%
  ~ mid_data

ggplot(data = first_step_data,aes(yob,coef_occ)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_line(data = pre_data, aes(yob, avg_occ), colour = "red", size = 1) +
  geom_line(data = post_data, aes(yob, avg_occ), colour = "red", size = 1) + 
  geom_line(data = first_step_data, aes(yob, lower_occ), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_occ), colour = "orange") +
  geom_segment(data = first_step_data, aes(x = 1899, xend = 1919+1, y = min(pre_data$avg_occ), yend = max(post_data$avg_occ)),colour = "red",size = 1) +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Occupational Income Score")

ggplot(data = first_step_data,aes(yob,coef_sei)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_line(data = pre_data, aes(yob, avg_sei), colour = "red", size = 1) +
  geom_line(data = post_data, aes(yob, avg_sei), colour = "red", size = 1) + 
  geom_line(data = first_step_data, aes(yob, lower_sei), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_sei), colour = "orange") +
  geom_segment(data = first_step_data, aes(x = 1899, xend = 1919+1, y = min(pre_data$avg_sei), yend = max(post_data$avg_sei)),colour = "red",size = 1) +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Duncan Socio-Economic Indicator")

intro_data <- data.frame(x_a = c(-38,-18,0,20), y_a = c(0,0,1,1))

ggplot(data = intro_data, aes(x_a,y_a)) +
  scale_y_continuous(breaks = c(0 , 1)) +
  scale_x_continuous(breaks = c(-18 , 0)) +
  annotate("rect", xmin=-18,xmax=0,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  xlab("Year of birth relative to start of campaign") +
  ylab("Childhood exposure to eradiction campaign") +
  ggtitle("Childhood Exposure to Eradication Campaign") +
  geom_line(linetype = 2)

ggplot(data = first_step_data,aes(yob,coef_occ)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_smooth(data = pre_data, aes(yob,coef_occ), method = "lm", colour = "red", fill = "red", size = 1) +
  geom_smooth(data = post_data, aes(yob,coef_occ), method = "lm", colour = "red", fill = "red", size = 1) +
  geom_smooth(data = mid_data, aes(yob,coef_occ), method = "lm", colour = "red", fill = "red", size = 1) +
  geom_line(data = first_step_data, aes(yob, lower_occ), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_occ), colour = "orange") +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Occupational Income Score (Group Linear Fit)")

ggplot(data = first_step_data,aes(yob,coef_occ)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_smooth(data = pre_data, aes(yob,coef_occ), colour = "red", fill = "red", size = 1) +
  geom_smooth(data = post_data, aes(yob,coef_occ), colour = "red", fill = "red", size = 1) +
  geom_smooth(data = mid_data, aes(yob,coef_occ), colour = "red", fill = "red", size = 1) +
  geom_line(data = first_step_data, aes(yob, lower_occ), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_occ), colour = "orange") +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Occupational Income Score (Group Non-linear Fit)")

ggplot(data = first_step_data,aes(yob,coef_occ)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_smooth(data = first_step_data, aes(yob,coef_occ), colour = "red", fill = "red", size = 1) +
  geom_line(data = first_step_data, aes(yob, lower_occ), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_occ), colour = "orange") +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Occupational Income Score (Non-linear Fit)")

ggplot(data = first_step_data,aes(yob,coef_sei)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_smooth(data = pre_data, aes(yob,coef_sei), method = "lm", colour = "red", fill = "red", size = 1) +
  geom_smooth(data = post_data, aes(yob,coef_sei), method = "lm", colour = "red", fill = "red", size = 1) +
  geom_smooth(data = mid_data, aes(yob,coef_sei), method = "lm", colour = "red", fill = "red", size = 1) +
  geom_line(data = first_step_data, aes(yob, lower_sei), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_sei), colour = "orange") +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Duncan Socio-Economic Indicator (Group Linear Fit)")

ggplot(data = first_step_data,aes(yob,coef_sei)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_smooth(data = pre_data, aes(yob,coef_sei), colour = "red", fill = "red", size = 1) +
  geom_smooth(data = post_data, aes(yob,coef_sei), colour = "red", fill = "red", size = 1) +
  geom_smooth(data = mid_data, aes(yob,coef_sei), colour = "red", fill = "red", size = 1) +
  geom_line(data = first_step_data, aes(yob, lower_sei), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_sei), colour = "orange") +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Duncan Socio-Economic Indicator (Group Non-linear Fit)")

ggplot(data = first_step_data,aes(yob,coef_sei)) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point() + 
  geom_smooth(data = first_step_data, aes(yob,coef_sei), colour = "red", fill = "red", size = 1) +
  geom_line(data = first_step_data, aes(yob, lower_sei), colour = "orange") + 
  geom_line(data = first_step_data, aes(yob, upper_sei), colour = "orange") +
  xlab("Year of Birth") +
  ylab("Coefficient") +
  ggtitle("Duncan Socio-Economic Indicator (Non-linear Fit)")

####################################################
origin_data %>>%
  filter((!is.na(malmort1890))) %>>%
  mutate(high =  as.numeric(malmort1890 >= quantile(malmort1890, probs = 0.7, na.rm = TRUE))) %>>%
  mutate(low =  as.numeric(malmort1890 <= quantile(malmort1890, probs = 0.3, na.rm = TRUE))) %>>%
  mutate(time = ifelse(yob < 1900,"pre",ifelse(yob > 1920, "post", "mid"))) %>>%
  ~ final_data

ggplot(data = final_data, aes(yob,occscore, colour = factor(high))) +
  scale_colour_manual(values = c("#66CC99", "#CC6666", "#336633", "red"), labels = c("Low Historical Mortality", "High Historical Mortality", "Mean OIC of Group Low", "Mean OIC of Group High")) +
  scale_x_continuous(breaks = seq(1820, 1960, 20)) +
  annotate("rect", xmin=1899+1,xmax=1919+1,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point(alpha = 0.6, size = 1) +
  stat_summary(fun.y = mean, size = 1, geom = "line", aes(color = paste("mean", factor(high)), group=factor(high))) +
  scale_size(guide="none") +
  annotate("text", x = 1850, y = 3.4, label = "Mean OIC of Cohorts borned at\n Low Historical Mortality Area", colour = "#336633", size = 4) +
  annotate("text", x = 1930, y = 2.9, label = "Mean OIC of Cohorts borned at\n High Historical Mortality Area", colour = "red", size = 4) +
  ylim(2.5,3.75) +
  xlab("Year of Birth") +
  ylab("Occupational Income Score (OIC)") +
  ggtitle("Group Productivity Time Trend") +
  theme(legend.position = "none")

ggplot(data = final_data, aes(malmort1890,occscore, colour = time)) +
  annotate("rect", xmin=0, xmax=0.5,ymin=-Inf,ymax=Inf, fill = "grey",colour = "grey", alpha = 0.4) +
  geom_point(alpha = 0.3, size = 1) +
  ylim(2.8,3.5) +
  geom_smooth(group = time, method = "loess", size = 1.5) + 
  xlab("Malaria Mortality Year 1890") +
  ylab("Occupational Income Score (OIC)") +
  annotate("text", x = 0.75, y = 3.34, label = "Year of Birth 1920-1960", size = 4, colour = cond) +
  annotate("text", x = 0.75, y = 3.24, label = "Year of Birth 1900-1920", size = 4) +
  annotate("text", x = 0.75, y = 3.07, label = "Year of Birth 1825-1900", size = 4) +
  theme(legend.position = "none")
