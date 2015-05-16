get_bref_player_season_stats <- function(season.end, stat_type = c("Advanced","Totals","Per Minute","Per Game"), team.totals = F , league = 'NBA'){
	packages <-
		c('rvest','magrittr','dplyr','stringr','tidyr')
	lapply(packages, library, character.only = T)
	
	bref_team_base <-
		'http://www.basketball-reference.com/leagues/'
	bref_base <-
		'http://www.basketball-reference.com'
	
	stat_type <- 
		match.arg(stat_type,choices = c("Advanced",'Totals','Per Minute',"Per Game")) 
	
	stat_type %<>% 
		tolower %>% gsub("\\ ",'_',.)
	url <- 
		paste0(bref_team_base,league,'_',season.end,'_',stat_type,'.html')
	css_page <- 
		paste0('#',stat_type)
	
	css_player <- 
		'td:nth-child(2) a'
	
	page <- 
		url %>% 
		html
	
	tables <- 
		page %>% 
		html_table(fill = T)
	
	data <- 
		tables[1] %>% 
		data.frame %>% 
		tbl_df
		
	url.player <- 
		page %>% 
		html_nodes(css_player) %>% 
		html_attr('href') %>% 
		paste0('http://www.basketball-reference.com/',.)
	
	names(data) %<>% 
		tolower
	
	data %<>% 
		dplyr::filter(!rk == 'Rk')
	
	data %<>% 
		select(-rk) %>% 
		rename(id.team = tm, id.pos = pos)
	
	if(team.totals == T){
		multi.teams <- 
			data %>% 
			dplyr::filter(id.team == "TOT")
		
		one.team <- 
			data %>% 
			dplyr::filter(!player %in% multi.teams$player)
		
		data <- 
			bind_rows(multi.teams,one.team) %>% 
			arrange(player)
	} else{
		data %<>% 
			dplyr::filter(!id.team == 'TOT')
	}
	
	numerics <- 
		data %>% 
		select(-player, -id.pos, -id.team) %>% 
		names
	
	data[,numerics] %<>%
		apply(2, as.numeric)
	
	season.start <- 
		season.end - 1
	
	id.season <- 
		season.start %>% 
		paste0('-',season.end %>% substr(3,4))
	
	data %<>% 
		mutate(season.end, 
					 id.season,
					 id.pos = data$id.pos %>% substr(1,2),
					 id.name.table = stat_type,
					 url.season = url)
	return(data)
	
}