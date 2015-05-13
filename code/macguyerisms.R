macgyver_me <- function(){
	packages <- 
	c('magrittr', 'dplyr', 'rvest', 'qdap', 'tidyr', 'stringr', 'stringi', 'tm', 'readr',
		'wordcloud')

lapply(packages, library, character.only = T)

url <- 
	'http://www.macgyveronline.com/pages/macgyverisms.html'

all <- 
	url %>% 
	html %>% 
	html_nodes('td+ td tr+ tr font') %>% 
	html_text %>% 
	.[1]

titles <- 
	url %>% 
	html %>% 
	html_nodes('td+ td td strong') %>% 
	html_text

exclude <- 
	titles %>% 
	paste0(collapse = "|")

mac.stuff <-
	all %>% 
	gsub(exclude, '', .) %>% 
	str_split(pattern = '\\- ') %>%
	unlist %>% 
	gsub("[^A-Z a-z 0-9 .  ]",'', .) %>% 
	Trim() %>% 
	.[!. %in% ''] %>% 
	gsub("\\s+"," ",.) %>% 
	gsub('SAK', 'Swiss Army Knife', .) %>% 
	.[2:length(.)] %>% 
	tolower

mac.stuff <- 
	data_frame(action = mac.stuff)

mystopwords <- c(stopwords('english'),"used","with", 'made')

tdm <- 
	mac.stuff %>% 
	DataframeSource %>% 
	Corpus %>% 
	tm_map(function(x)removeWords(x,mystopwords)) %>% 
	TermDocumentMatrix


m <- 
	tdm %>% 
	as.matrix()
v <- 
	m %>% 
	rowSums %>% 
	sort(decreasing=TRUE)

d <- 
	data.frame(word = names(v),freq=v)

wordcloud(d$word,d$freq, scale=c(4,.2), max.words=300, random.order=FALSE, rot.per=0.15, use.r.layout=TRUE, colors=wesanderson::wes_palette('Zissou'))
}