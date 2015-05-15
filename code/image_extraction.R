library(rPlotter)
library(dplyr)
extract_colours <- function(
	url_img = "http://developer.r-project.org/Logo/Rlogo-1.png", num_col = 5, rsize = 100) {
	
	## Read Image
	if (class(url_img) != "Image") {
		img <- readImage(url_img) # local file or url
	} else {
		img <- url_img # is already a loaded "Image"
	}
	
	## Resize Image (make it smaller so the remaining tasks run faster)  
	if (max(dim(img)[1:2]) > rsize) {
		if (dim(img)[1] > dim(img)[2]) {
			img <- resize(img, w = rsize)
		} else {
			img <- resize(img, h = rsize)
		}
	}
	
	## Melt
	img_melt <- melt(img)
	
	## Reshape
	img_rgb <- reshape(img_melt, timevar = "Var3", idvar = c("Var1", "Var2"), direction = "wide")
	img_rgb$Var1 <- -img_rgb$Var1
	
	## Detect dominant colours with kmeans (multiple starts)
	col_dom <- kmeans(img_rgb[, 3:5], centers = num_col, nstart = 3, iter.max = 100)
	
	## Return k-means centers as RGB colours
	cus_pal <- sort(rgb(col_dom$centers))
	return(as.character(cus_pal))
	
}

url.image <- 
	"http://www.wired.com/wp-content/uploads/2015/02/Untitled-12.jpg"

bigpal <-
	url.image %>% 
	extract_colours( 256, 200)
## get 5 best 6 colour palettes for MPD
newpal <- 
	bigpal %>% 
	mpd_select_colours(sat.thresh = 0.25, dark.thresh = 0.1 , ncolours = 6, nreturn = 5)
## plot palettes
image <- 
	readImage(url.image)
h <- 
	split.screen(c(1,2))
par(mar = c(0,0,0,0)) # set zero margins on all 4 sides
plot(x = NULL, y = NULL, xlim = c(0,400), ylim = c(0,400), pch = '',
		 xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xaxs = 'i', yaxs = 'i',
		 bty = 'n', asp=1) # plot empty figure
rasterImage(image, xleft = 0, ybottom = 0, xright = 400, ytop = 400) # plot jpeg
screen(2)
par(mar=c(0,0,0,0))
h <- split.screen(c(5,1))
for (i in 1:length(newpal)) {
	screen(2+i)
	pie(rep(1, 4), col = newpal[[i]])
}
dev.off()
