# built using the following tutorials as a starting point:

# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html (Dumbell Plot)
# https://rud.is/b/2017/02/18/putting-it-all-together/ (More on Dumbell Plots)
# http://r-statistics.co/Complete-Ggplot2-Tutorial-Part2-Customizing-Theme-With-R-Code.html (Customizing ggplot2 themes)

# load the libraries
library(tidyverse)
library(readxl)
library(forcats)
library(ggalt)
library(gridExtra)
theme_set(theme_classic())

# load the data
tuition <- read_excel("us_avg_tuition.xlsx")

# add rate columns and type to split
tuition <- tuition %>%
  mutate(`2004` = round((tuition$`2004-05`-mean(tuition$`2004-05`))/mean(tuition$`2004-05`),2)) %>%
  mutate(`2016` = round((tuition$`2015-16`-mean(tuition$`2015-16`))/mean(tuition$`2015-16`),2)) %>%
  mutate(type = ifelse(`2016`-`2004` > 0, "increased", "decreased")) %>%
  select(State, `2004`, `2016`, type)

# split for relative cost increase
tuition_inc <- tuition %>%
  filter(type == "increased") %>%
  select(-type) %>%
  gather(year, diff_rate, -State)

# split for relative cost decrease
tuition_dec <- tuition %>%
  filter(type == "decreased") %>%
  select(-type) %>%
  gather(year, diff_rate, -State)

# make a df for labels for relative tuition increasing
labs_df_inc <- tuition_inc %>% 
  filter(State == tuition_inc %>% 
           arrange(-as.integer(year),-diff_rate) %>% 
           filter(row_number() ==1) %>% 
           select(State) %>% 
           as.character()
         )

# make a df for labels for realtive tuition decreasing
labs_df_dec <- tuition_dec %>% 
  filter(State == tuition_dec %>% 
           arrange(-as.integer(year),diff_rate) %>% 
           filter(row_number() ==1) %>% 
           select(State) %>% 
           as.character()
  )
  

## make the dataset for tuition going up
tup <- tuition_inc %>% 
  group_by(State) %>% 
  arrange(as.integer(year)) %>% 
  filter(between(row_number(), 1, n())) %>%   # switch from slice () to filter()
  spread(year, diff_rate) %>% 
  ungroup() %>% 
  mutate(State = as_factor(State)) %>%
  mutate(State = fct_reorder(State, `2016`))

## make dataset for tuition going down
tdown <- tuition_dec %>% 
  group_by(State) %>% 
  arrange(as.integer(year)) %>% 
  filter(between(row_number(), 1, n())) %>%   # switch from slice () to filter()
  spread(year, diff_rate) %>% 
  ungroup() %>% 
  mutate(State = as_factor(State)) %>%
  mutate(State = fct_reorder(State, -`2016`))

## plot for relative cost increases
ti <- ggplot(tup) +
  geom_dumbbell(aes(x=`2004`, xend=`2016`, y=State, group=State),
                color="#c0c0c0", 
                size=1,
                size_xend =2,
                colour_xend="#0072B2") +
  geom_text(data=labs_df_inc, aes(x=diff_rate, y=25.2, label=paste0("'",substring(year,3,4))), vjust=0, size=2) +
  scale_x_reverse(labels = scales::percent) + 
  scale_y_discrete(position = "left") +
  labs(x="Tuition Cost as a Percentage Above/Below", 
       y=NULL, 
       title="Change in Tuition Costs Relative", 
       subtitle="Relative Price has Increased", 
       caption="Source: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/") +
  theme(plot.title = element_text(hjust=1, face="bold", family = "Helvetica"),
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(color = "white", hjust=0),
        axis.title.x = element_text(hjust=1),
        plot.background=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(color = "light grey"),
        text=element_text(size=9,  family="Arial"),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(ti)

## plot for relative cost decreases
td <- ggplot(tdown) +
  geom_dumbbell(aes(x=`2004`, xend=`2016`, y=State, group=State),
                color="#c0c0c0", 
                size=1,
                size_xend =2,
                colour_xend="#D55E00") +
  geom_text(data=labs_df_dec, aes(x=diff_rate, y=25.2, label=paste0("'",substring(year,3,4))), vjust=0, size=2) +
  scale_x_reverse(labels = scales::percent) + 
  scale_y_discrete(position = "right") +
  labs(x="Average Cost for Each Year", 
       y=NULL, 
       title="to Average Costs (between 2004 and 2016)", 
       subtitle="Relative Price has Decreased", 
       caption="Source: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/") +
  theme(plot.title = element_text(hjust=0, face="bold", family = "Helvetica"),
        plot.subtitle = element_text(hjust=1),
        axis.title.x = element_text(hjust=0),
        plot.background=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(color = "light grey"),
        text=element_text(size=9,  family="Arial"),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(td)

# use grid.arrange to place the plots
tt1 <- grid.arrange(ti, td, padding = 0, ncol=2)
tt1

## save the image
ggsave("tt1.png", tt1, width = 8, height = 6, units = "in", device = "png")
