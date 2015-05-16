'http://stats.nba.com/stats/playergamelog?LeagueID=00&PlayerID=2216&Season=2001-02&SeasonType=Regular+Season'
player = "Zach Randolph"
get_nba_player_game_log_stats <-
	function(player, season , season.type = 'Regular Season') {
		packages <-
			c('dplyr', 'magrittr', 'jsonlite', 'stringr', 'tidyr', 'lubridate', 'readr')
		lapply(packages, library, character.only = T)
		players <- 
			read_csv('data/nba/all_player_data.csv')
		
		chosen.player <- 
			player
		
		playerID <-
			players %>% 
			filter(player == chosen.player) %>% 
			select(player_id) %>% 
			.$player_id %>% 
			as.numeric()
		
		base <-
			'http://stats.nba.com/stats/playergamelog?LeagueID=00&PlayerID='
		
		id.season.type <-
			season.type %>%
			gsub('\\ ','\\+', .)
		
		season.end <-
			season + 1
		
		id.season <-
			paste0(season,"-",
						 season.end %>%
						 	substr(3,4))
		
		url.data <-
			base %>%
			paste0(playerID,'&Season=', id.season, '&SeasonType=',id.season.type)
		
		if(season %>% length > 1){
			results.data <-
				data_frame()
			for(u in 1:length(url.data)){
				url <- 
					url.data[u]
				s <-
					id.season[u]
				data <-
					url %>%
					fromJSON(simplifyDataFrame = T,flatten = T)
				rd <-
					data$resultSets$rowSet %>%
					data.frame(stringsAsFactors = F) %>%
					tbl_df
				
				names(rd) <-
					data$resultSets$headers %>%
					unlist %>%
					tolower() %>%
					gsub('\\_','.',.)
				
				rd[,rd %>%
										 	select(min:plus.minus) %>%
										 	names] %<>% apply(2, function(x)
										 		as.numeric(x))
				
				rd %<>%
					mutate(
						game.date = mdy(game.date),
						win = rd$wl %>% grepl('W',.),
						location.home = rd$matchup %>%
							grepl("vs.",.),
						id.team.player = rd$matchup %>%
							substr(1, 3),
						id.team.opponent =
							rd$matchup %>%
							substr(
								start = rd$matchup %>% nchar - 2, stop = rd$matchup %>% nchar
							),
						id.season	 = s)
				results.data %<>%
				bind_rows(rd)
			}
		} else {
			data <-
			url.data %>%
			fromJSON(simplifyDataFrame = T,flatten = T)
		
		results.data <-
			data$resultSets$rowSet %>%
			data.frame(stringsAsFactors = F) %>%
			tbl_df
		
		names(results.data) <-
			data$resultSets$headers %>%
			unlist %>%
			tolower() %>%
			gsub('\\_','.',.)
		
		results.data[,results.data %>%
								 	select(min:plus.minus) %>%
								 	names] %<>% apply(2, function(x)
								 		as.numeric(x))
		
		results.data %<>%
			mutate(
				game.date = mdy(game.date),
				win = results.data$wl %>% grepl('W',.),
				location.home = results.data$matchup %>%
					grepl("vs.",.),
				id.team.player = results.data$matchup %>%
					substr(1, 3),
				id.team.opponent =
					results.data$matchup %>%
					substr(
						start = results.data$matchup %>% nchar - 2, stop = results.data$matchup %>% nchar
					),
				pts.per.min = pts/min,
				id.season
			)
		
		}
		results.data %<>% 
			arrange(game.date) %<>% 
			mutate(season = (id.season %>% substr(1,4) %>% as.numeric) + 1)
		return(results.data)
	}
