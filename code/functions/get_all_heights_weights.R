all_data <-
	read_csv(
		'~/Desktop/webpage/abresler.github.io/presentations/si_hackathon/all_nfl_drafts.csv'
	)

url.players <-
	all_data %>%
	dplyr::filter(!is.na(url.player)) %>%
	.$url.player

all_player_metadata <-
	data_frame()

url.players <-
	url.players[!url.players %in% all_player_metadata$url.player]
for (url in url.players) {
	player <-
		all_data %>%
		dplyr::filter(url == url.player) %>%
		select(player) %>%
		extract2(1) %>%
		unique

	if(url %>%
		 html %>%
		 html_nodes('.float_left p+ p') %>%
		 html_text %>% length > 0){
		meta_data <-
		url %>%
		html %>%
		html_nodes('.float_left p+ p') %>%
		html_text

	meta_data %<>%
		str_split_fixed("\n",3)

	height.weight <-
		meta_data[2] %>%
		str_split("\\s+") %>%
		unlist

	if (height.weight == '') {
		player.metadata <-
			data_frame(
				player,
				height.inches = NA,
				weight.lbs = NA,
				url.player = url
			)
	} else{
		player.metadata <-
			data_frame(
				player,
				height = height.weight[2],
				weight.lbs = height.weight[4] %>% extract_numeric(),
				url.player = url
			)

		player.metadata %<>%
			separate(height, into = c('feet', 'inches'), sep = '\\-')

		player.metadata$feet %<>% as.numeric
		player.metadata$inches %<>% as.numeric

		player.metadata %<>%
			mutate(
				height.inches = (feet * 12) + inches,
				bmi = (weight.lbs / height.inches ^ 2) * 703
			)

		player.metadata %<>%
			select(-feet,-inches)
	}
	} else{
		player.metadata <-
			data_frame(
				player,
				height.inches = NA,
				weight.lbs = NA,
				url.player = url
			)
	}
	all_player_metadata %<>%
		bind_rows(player.metadata)
}
