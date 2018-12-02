load("polls_2008.rda")
#data("polls_2008")
qplot(day,margin, data = polls_2008)

#use reggression to calculate the condition expectation
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+","-")
polls_2008 %>% 
  mutate(resid = resid) %>%
  ggplot(aes(day, margin)) +
  geom_smooth( method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)

#bin smoothing, find the middle of the week average to smooth the line out for 2 days
span <- 3.5
tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(dist <= span)

tmp %>%filter(center %in% c(-125, -55)) %>%
    ggplot(aes(day, margin)) +
    geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "gray") +
    geom_point(size = 2) +
    geom_smooth(aes(group = center), 
                method = "lm", formula=y~1, se = FALSE) +
    facet_wrap(~center)

#icompute the means for every point to form the underlying curve
# add animater
library(devtools)
library(RCurl)
library(httr)
library(udunits2)
set_config( config( ssl_verifypeer = 0L ) )
devtools::install_github("dgrtwo/gganimate")

                