packages <-
	c('rpivotTable', 'materializeR', 'htmltools')

lapply(packages, library, character.only = T)

table <-
	all_data	%>%
	select(-url.player, -url.year.draft) %>%
	rpivotTable(rows = c('year.draft'),
							cols = c('type.pos'),
							vals = "selections.all_pro",
							aggregatorName = "Sum",
							rendererName = "Table Barchart")

title <-
	c('Exploring the NFL Draft, 1936 - 2015')
table.page <-
	table %>%
	tagList(
		materialize(),
		tags$style(".rpivotTable select {display:inherit; !important}"),
		tags$h5(title)
		,.)

table.page %>%
	html_print
