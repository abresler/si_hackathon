
packages <-
	c('readr', 'dplyr', 'magrittr', 'ggplot2', 'wesanderson', 'ggthemes')
lapply(packages, library, character.only)
covers <- 
	'data/si/swimsuit_covers_raw.csv' %>% 
	read_csv

years <- 
	covers$Year

colors <- 
	covers$Color
all_colors <-
	data_frame()
for(c in 1:length(colors)){
	color <- 
		colors[c]
	color %<>% 
		str_split('\\,') %>% 
		unlist
	
	data <- 
		data_frame(year = years[c], color)
	
	data %<>% 
		mutate(decade = data$year %>% substr(1,3) %>% paste0('0s'))
	all_colors %<>%
		bind_rows(data)
}

covers_tally <- 
	all_colors %>% 
	tally(sort = T)
library(ggplot2)
pal <-
	c(wes_palette("Darjeeling"),wes_palette("Darjeeling2"))
all_colors %>% 
	ggplot(aes(x=color,y=..count.., fill = decade)) +
	geom_histogram(alpha = .75 ) +
	scale_fill_manual(values = pal) +
	facet_grid(.~ decade, scales = "fixed") +
	ylab(" ") +
	xlab(" ") +
	theme_few(base_family = "Helvetica",base_size = 8) +
	#scale_y_continuous(breaks=seq(0,.12,.01), labels=percent) +
	theme(legend.position="none") +
	coord_flip() +
	scale_y_reverse() +
	labs(title="Sports Illustrated Swimsuit Edition Covers\nBathing Suit Colors by Decade")


all_data %>%
	ggplot(aes(x=opp_pts.g, y=..density..)) +
	geom_density(fill="decade", colour=NA, alpha=.2) +
	geom_line(stat="density")