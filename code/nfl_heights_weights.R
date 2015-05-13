get_nfl_draft <-
	function(year) {
		packages <-
			c('rvest', 'dplyr', 'stringr', 'magrittr')

		lapply(packages, library, character.only = T)

		url <-
			paste0('http://www.pro-football-reference.com/years/',year,'/draft.htm')

		table <-
			url %>%
			html %>%
			html_table

		table <-
			table[1] %>%
			data.frame %>%
			tbl_df


		table.names <-
			table %>%
			slice(1) %>%
			tolower

		table %<>%
			slice(-1)

		names(table) <-
			table.names

		names(table)[4] <-
			'player'

		columns <-
			table %>% ncol

		table <-
			table[,1:(columns - 1)]

		table$player.hof <-
			table$player %>%
			grepl('\\HOF',.)

		table$player %<>%
			gsub('\\HOF','',.) %>%
			str_trim


		names(table)[15:18] %<>%
			paste0('.pass')

		names(table)[19:21] %<>%
			paste0('.rush')

		names(table)[22:24]  %<>%
			paste0('.receiving')


		table %<>%
			dplyr::filter(!is.na(player),!rnd == 'Rnd')


		url.player <-
			url %>%
			html %>%
			html_nodes('td:nth-child(4) a') %>%
			html_attr('href') %>%
			paste0('http://www.pro-football-reference.com',.)


		player <-
			url %>%
			html %>%
			html_nodes('td:nth-child(4) a') %>%
			html_text()

		player_urls <-
			data_frame(player, url.player)

		table %<>%
			left_join(player_urls)

		table %<>%
			mutate(year, url.year.draft = url)
		return(table)
	}
