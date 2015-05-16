data.2015 <- 
	get_bref_player_season_stats(season.end = 2015,team.totals = T,stat_type = 'Per Game')
library(kohonen)
min_games = 5

kohonen.data <- 
	data.2015 %>% 
	dplyr::filter(g >= min_games) %>% 
	tbl_df

set.seed(7)
xdim <- 
	12
ydim <-
	12

som_grid <- 
	somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal")
	
kohonen.matrix <- 
	kohonen.data %>% 
	dplyr::select(fg, fga, x3p, x3pa, x2p, x2pa, ft, fta, orb, trb, ast:pts) %>% 
	as.matrix  %>% 
	scale

names(kohonen.matrix) <- 
	kohonen.data %>% 
	dplyr::select(fg, fga, x3p, x3pa, x2p, x2pa, ft, fta, orb, trb, ast:pts) %>% names

som_model_nba <- 
	kohonen.matrix %>% 
	som(grid = som_grid,
			rlen = 100, # could be 100
			alpha = c(.05,.01),
			keep.data = T,
			n.hood = 'circular')
source('~/Desktop/webpage/abresler.github.io/presentations/si_hackathon/code/coolBlueHotRed.R')
som_model_nba %>% plot(type = "changes")
som_model_nba %>% plot(type = "counts",palette.name = coolBlueHotRed)
som_model_nba %>% plot(type = "codes",palette.name = coolBlueHotRed)
som_model_nba %>% plot(type = "dist.neighbours",palette.name = coolBlueHotRed)

som_model_nba %>% plot(type = "property",property = som_model_nba$codes[,3],main = 
													names(som_model_nba$data)[3], palette.name = coolBlueHotRed)


clusters = 12
som_cluster <- 
	som_model_nba$codes %>% 
	dist %>% 
	hclust %>% 
	cutree(clusters)

# Show the map with different colours for every cluster						  


cluster.player <- 
	som_cluster[som_model_nba$unit.classif]

group.player <- 
	som_model_nba$unit.classif

kohonen.data$cluster.player <- 
	cluster.player

kohonen.data$group.player <- 
	group.player



data <- 
	kohonen.data %>% 
	dplyr::select(cluster.player, group.player, player:url.season, id.pos) %>% 
	dplyr::select(player, cluster.player, group.player, id.pos, id.team)

data$player %<>% 
	as.factor

data$group.player %<>%
	as.factor()

edges <- 
	data %>% 
	dplyr::select(player, cluster.player )

nodes.clusters <- 
	data %>% 
	dplyr::select(player, cluster.player) %>% 
	gather(role, name) %>% 
	unique

nodes.teams <- 
	data %>% 
	dplyr::select(player, id.team) %>% 
	gather(role, name) %>% 
	unique

nodes <- 
	nodes.clusters %>% 
	bind_rows(nodes.teams)

edges.clusters <- 
	data %>% 
	dplyr::select(player, cluster.player )

edges.teams <- 
	data %>% 
	dplyr::select(player, cluster.player )

edges <- 
	edges.clusters %>% 
	bind_rows(edges.teams)

names(edges) <- 
	c('source','target')

JSON <-
	nodes %>%
	jsonNodesLinks(edges)

