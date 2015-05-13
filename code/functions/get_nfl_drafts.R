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



years <-
	1936:2015

all_data <-
	data_frame()

for(year in years){
	data <-
		get_nfl_draft(year)

	all_data %<>%
		bind_rows(data)
}

all_player_metadata %>%
	distinct %>%
	write_csv('~/Desktop/webpage/abresler.github.io/presentations/si_hackathon/data/all_draft_player_metadata.csv')

all_data %>%
	left_join(all_player_metadata)

positions <-
	all_data %>%
	arrange(pos) %>%
	select(pos) %>%
	.$pos %>%
	unique

positions.types <-
	data_frame(pos = positions, pos.new = pos)

all_data %>%
	dplyr::filter(pos == 'MG', !is.na(url.player)) %>%
	select(url.player) %>%
	extract2(1) %>%
	.[2] %>% browseURL()

rbs <-
	c('B', 'HB' , 'RB', 'TB', 'WB')

positions.types[positions.types$pos.new %in% rbs,'pos.new'] <-
	'RB'

fbs <-
	c('BB', 'FB')

positions.types[positions.types$pos.new %in% fbs,'pos.new'] <-
	'FB'

wrs <-
	c('WR', 'FL', 'SE')

positions.types[positions.types$pos.new %in% wrs,'pos.new'] <-
	'WR'


tes <-
	c('E','TE')

positions.types[positions.types$pos.new %in% tes,'pos.new'] <-
	'TE'

ngs <-
	c('DG','NG', 'NT', 'MG')

positions.types[positions.types$pos.new %in% ngs,'pos.new'] <-
	'NT'

ts <-
	c('T','OT')

positions.types[positions.types$pos.new %in% ts,'pos.new'] <-
	'OT'

gs <-
	c('G','OG')

positions.types[positions.types$pos.new %in% gs,'pos.new'] <-
	'OG'

pos_types <-
	data_frame(pos.new =
	positions.types %>%
	arrange(pos.new) %>%
	select(pos.new) %>%
	unique %>%
	extract2(1), type.pos = '')

o.lineman <-
	c('C','LS', 'OG', 'OL','OT')

pos_types[pos_types$pos.new %in% o.lineman,'type.pos'] <-
	'Offensive Line'

d.lineman <-
	c('DE','DL', 'DT','NT')

pos_types[pos_types$pos.new %in% d.lineman,'type.pos'] <-
	'Defensive Line'

back <-
	c('RB')

pos_types[pos_types$pos.new %in% back,'type.pos'] <-
	'Running Back'

f.back <-
	c('FB')

pos_types[pos_types$pos.new %in% f.back,'type.pos'] <-
	'Fullback'

backer <-
	c('ILB', 'LB', 'OLB')

pos_types[pos_types$pos.new %in% backer,'type.pos'] <-
	'Linebacker'

wide <-
	c('WR', 'KR')

pos_types[pos_types$pos.new %in% wide,'type.pos'] <-
	'Recieiver'

second <-
	c('CB', 'DB', 'S', 'SS', 'FS')

pos_types[pos_types$pos.new %in% second,'type.pos'] <-
	'Secondary'

qb <-
	c('QB')

pos_types[pos_types$pos.new %in% qb,'type.pos'] <-
	'Quarterback'

punter <-
	c('P')

pos_types[pos_types$pos.new %in% punter,'type.pos'] <-
	'Punter'

kicker <-
	c('K')

pos_types[pos_types$pos.new %in% kicker,'type.pos'] <-
	'Kicker'

end <-
	c('TE')

pos_types[pos_types$pos.new %in% end,'type.pos'] <-
	'Tight End'

pos_types %<>%
	left_join(positions.types)

all_data %<>%
	left_join(pos_types)

all_data %<>%
	select(-pos) %>%
	rename(id.pos = pos.new, id.team = tm, season.final = to, selections.all_pro = ap1, selections.pro_bowl = pb, years.starter = st, value.career.av = carav, value.team.av = drav, games.played = g, completions.pass = cmp, id.round = rnd, id.pick = pick, year.draft = year, college = `college/univ`)

all_data %<>%
	select(year.draft, id.round:age,type.pos:weight.lbs, college, player.hof, season.final:sk, tkl,url.player, url.year.draft) %>%
	arrange(desc(year.draft))

all_data %>%
	write_csv('Desktop/webpage/abresler.github.io/presentations/si_hackathon/data/all_nfl_drafted_player_data.csv')
