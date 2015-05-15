library(dplyr)
library(tidyr)
library(rvest)
library(ggplot2)
library(readr)
library(magrittr)

nfl_data <- 
	read_csv('data/nfl/all_nfl_drafted_player_data.csv')

nfl_data[nfl_data$player == 'Shelley Smith','weight.lbs'] <- 
	303

nfl_data %<>%
	mutate(
		bmi = (weight.lbs / height.inches ^ 2) * 703
	)

nfl_data$decade <- 
	nfl_data$year.draft %>% 
	substr(1,3) %>% 
	paste0("0s")


data.means <- 
	nfl_data %>% 
	filter(type.pos == 'Offensive Line', !is.na(weight.lbs), !id.pos == 'LS') %>% 
	group_by(year.draft) %>% 
	summarise(mean = mean(weight.lbs),
						max = max(weight.lbs),
						min = min(weight.lbs),
						se = sd(weight.lbs)/sqrt(length(weight.lbs))) %>% 
	mutate(upper.mean = mean+(2.101*se),  # calculate 95% CI for mean
				 lower.mean = mean-(2.101*se)) %>%  # calculate 95% CI for mean
	ungroup

minsMaxs <- 
	nfl_data %>% 
	filter(type.pos == 'Offensive Line', !is.na(weight.lbs), !id.pos == 'LS') %>% 
	group_by(decade) %>% 
	summarise(max = max(weight.lbs),
						min = min(weight.lbs)) %>% 
	ungroup

biggest <- 
	nfl_data %>%
	filter(type.pos == 'Offensive Line', !is.na(weight.lbs), !id.pos == 'LS') %>% 
	left_join(minsMaxs) %>%  # merge historical lows to current year low data
	mutate(record.max = ifelse(weight.lbs== max, "Y", "N")) %>% # identifies if current year was record low
	filter(record.max == "Y")  # filter for days that represent current year record lows

smallest <- 
	nfl_data %>%
	filter(type.pos == 'Offensive Line', !is.na(weight.lbs), !id.pos == 'LS') %>% 
	left_join(minsMaxs) %>%  # merge historical lows to current year low data
	mutate(record.min = ifelse(weight.lbs== min, "Y", "N")) %>% # identifies if current year was record low
	filter(record.min == "Y")  # filter for days that represent current year record lows


all_players <- 
	nfl_data %>% 
	filter(type.pos == 'Offensive Line', !is.na(weight.lbs), !id.pos == 'LS') %>% 
	select(year.draft, player, weight.lbs)
	
p2 <- 
	data.means %>% 
	ggplot(aes(year.draft, mean)) +
	theme(plot.background = element_blank(),
				panel.grid.minor = element_blank(),
				panel.grid.major = element_blank(),
				panel.border = element_blank(),
				panel.background = element_blank(),
				axis.ticks = element_blank(),
				#axis.text = element_blank(),
				axis.title = element_blank()) +
	geom_linerange(data.means, mapping=aes(x=year.draft, ymin=min, ymax=max), colour = "wheat2", alpha=1)

p2 <- 
	p2 +
	geom_linerange(data.means, mapping=aes(x=year.draft, ymin=lower.mean, ymax=upper.mean), colour = "firebrick",alpha = 1)

p2 <- 
	p2 +
	geom_line(data.means, mapping=aes(x=year.draft, y=mean, group=1)) +
	geom_vline(xintercept = 1935, colour = "black", linetype=1, size=1, alpha = .5)

p2 <- 
	p2 +
	coord_cartesian(ylim = c(170,390)) +
	scale_y_continuous(breaks = seq(170, 390, by=10)) +
	scale_x_continuous(breaks = seq(1935, 2015, 5))

p2 <- 
	p2 + 
	geom_hline(yintercept = 180, colour = "white", linetype=1) +
	geom_hline(yintercept = 200, colour = "white", linetype=1) +
	geom_hline(yintercept = 220, colour = "white", linetype=1) +
	geom_hline(yintercept = 240, colour = "white", linetype=1) +
	geom_hline(yintercept = 260, colour = "white", linetype=1) +
	geom_hline(yintercept = 280, colour = "white", linetype=1) +
	geom_hline(yintercept = 300, colour = "white", linetype=1) +
	geom_hline(yintercept = 320, colour = "white", linetype=1) +
	geom_hline(yintercept = 340, colour = "white", linetype=1) +
	geom_hline(yintercept = 360, colour = "white", linetype=1) +
	geom_hline(yintercept = 380, colour = "white", linetype=1) 

p2 <- 
	p2 +
	geom_point(data=all_players, aes(x=year.draft, y=weight.lbs), colour="blue3",
						 alpha = .1, position = position_jitter(w = 0.35, h = 0.5))

p2 <- 
	p2 +
	geom_point(data = smallest, aes(x=year.draft, y=weight.lbs), colour="blue3") +
	geom_point(data = biggest, aes(x=year.draft, y=weight.lbs), colour="firebrick3")

#Next, we can add the data that represents the 95% confidence interval around the daily mean temperatures for 1975-2013.

p2 <- 
	p2 +
	annotate("segment", x = 1954, xend = 1950, y = 310, yend = 320, colour = "firebrick3") +
	annotate("text", x = 1950, y = 324, label = "Earl Putman", size=3, colour="firebrick3") +
	annotate("segment", x = 1999, xend = 1995, y = 375, yend = 380, colour = "firebrick3") +
	annotate("text", x = 1994, y = 383, label = "Aaron Gibson", size=3, colour="firebrick3") +
	annotate("segment", x = 1982, xend = 1985, y = 190, yend = 183, colour = "blue3") +
	annotate("text", x = 1985, y =180, label = "Brian Clark", size=3, colour="blue3") +
	annotate("segment", x = 1992, xend = 1995, y = 255, yend = 245, colour = "blue3") +
	annotate("text", x = 1995, y =240, label = "Kendall Gammon", size=3, colour="blue3")


p2 <- 
	p2 +
	ggtitle("McGyvering NFL Draft Data") +
	theme(plot.title=element_text(face="bold",hjust=.15, vjust=-2.0, colour="red",size=24)) +
	annotate("text", x = 1960, y = 368, label = "Evolution of Offensive Lineman Weight - 1936-2015", size=4, fontface="bold")

p2 <- 
	p2 +
	annotate("text", x = 1960, y = 360,
					 label = "Data represents reported weight in pounds of drafted NFL Offensive Lineman", size=3, colour="gray30") +
	annotate("text", x = 1960, y = 352,
					 label = "as reported by Football-Reference.com", size=3, colour="gray30") 