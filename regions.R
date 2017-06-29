library( 'ggplot2' )
library( 'reshape' )
library( 'plyr' )
library( 'grid' )
library( 'gridExtra' )
library( 'scales' )

data = read.table( file = 'pivot.csv' , header = TRUE , sep = "," )

regions = unique( data$label2[ data$label2 != "Missouri" ] )

data = rename( data , c( "Sub.NSDUH.2012.14.Marijuana.Use.in.Past.Month" = "Marijuana" ,
	"Sub.NSDUH.2012.14.Illicit.Drug.Use.other.than.Marijuana.in.Past.Mont" = "Illicit Drug\n(not Marijuana)" ,
	"Sub.NSDUH.2012.14.Alcohol.Use.in.Past.Month" = "Alcohol" ,
	"Sub.NSDUH.2012.14.Binge.Alcohol.Use.in.Past.Month" = "Binge**" ,
	"Sub.NSDUH.2012.14.Cigarette.Use.in.Past.Month" = "Cigarette" ,
	"sub.NSDUH.2012.14.Nonmedical.Use.of.Pain.Relievers.Past.Year.Age.18." = "Pain Reliever\n(past year)" ,
	"label2" = "Region"
	)
)

data$NSDUH.2012.14.reg = NULL
data$Geography = NULL
data$label = NULL
data$X30DCig16 = NULL
data$X30DAlc16 = NULL
data$X30DBinge16 = NULL
data$X30DMJ16 = NULL
data$X30DInh16 = NULL
data$X30DScript16 = NULL
data$X30DOTC16 = NULL
data$X30DSynthetic16 = NULL
data$X2016.Past.month.electronic.cigarette.use = NULL
data$X2016.Past.month.hookah.use = NULL

#data = data[-c(6:length(data[,1])),]
#state = "Missouri"
state = " Missouri   "
data$Region = gsub( "Missouri" , state , data$Region )

data = unique( data )
goop = melt( data , id = c( "Region" ) )

goop$labels = goop$value

goop$labels = gsub( '%' , '' , goop$labels )
goop$labels = as.numeric( goop$labels )
goop$labels = format( round( goop$labels , 1 ) )
goop$labels = as.factor( goop$labels )
goop$labels = paste( goop$labels , "%" , sep = '' )

goop$value = gsub( '%' , '' , goop$value )

goop$value = as.numeric( goop$value ) / 100

plots = list()
cat( "make plots\n" )

#	Bigger font
labelSize = 3
axisSize = 8
tickSize = 8
titleSize = 9
legendSize = 8
legendX = 0.6
legendY = 0.85
keySize = 0.25
purple = "#5e498B"
yellow = "#F8CB44"
barWidth = 0.6
ymin = 0
ymax = 1.0
barSeparation = 0.075
labelSeparation = -barSeparation
barLabelAngle = 90
chartTitle = " " #"Current Substance Use for Ages 18+, 2012-14"
xOrder = c( "Alcohol" , "Cigarette" , "Binge**" , "Marijuana" , "Pain Reliever\n(past year)" , "Illicit Drug\n(not Marijuana)" )
w = 5 #5    4
h = 3 #3    2.5

for ( i in 1:length( regions ) ) {

	region = regions[i]
	cat( paste( region , '\n' ) )
	dat = data.frame( goop[ goop$Region == state , ] )
	dat = rbind( dat , goop[ goop$Region == region , ] )
	
	g = ggplot( dat , aes( x = reorder( variable , -value ) , y = value , fill = Region ) )

	g = g + geom_bar( stat = "identity" , position = position_dodge( width = barWidth + barSeparation ) , width = barWidth )
	
	g = g + geom_text( data = dat , aes( variable , value , label = labels ) , size = labelSize , hjust = labelSeparation , guide = FALSE , color = "black" , position = position_dodge( width = barWidth ) , angle = barLabelAngle )

	g = g + theme_bw( )
	g = g + theme( plot.margin = unit( c( 0.1 , 0.1 , -0.11 , -0.15 ) , "in" ) )
	
	g = g + ggtitle( chartTitle )
	g = g + theme( title = element_text( size = titleSize , color = purple , face = "bold" ) )
	
	g = g + xlab( '' )
	g = g + theme( axis.text.x = element_text( size = axisSize , angle = 45 , vjust = 1 , hjust = 1 ) )
	g = g + scale_x_discrete( labels = xOrder )

	g = g + ylab( "" )
	g = g + scale_y_continuous( limits = c( ymin , ymax ) , labels = percent )
	g = g + theme( axis.text.y = element_text( size = axisSize ) )

	g = g + theme( legend.key.size = unit( keySize , "cm" ) )
	g = g + theme( legend.position = c( legendX , legendY ) )
	g = g + theme( legend.title = element_blank() )
	g = g + theme( legend.background = element_blank( ) )
	g = g + theme( legend.text = element_text( size = legendSize ) )
	g = g + guides( fill = guide_legend( override.aes = list( size = 3 ) ) )

	g = g + scale_fill_manual( values = c( yellow , purple ) )
	g = g + theme( legend.direction = "horizontal" )
	#g = g + theme( legend.justification = 1 )

	g = g + theme( panel.grid.minor = element_blank() ,
	           panel.grid.major = element_blank() )

	g = g + theme( panel.border = element_blank() )
	g = g + theme( axis.line = element_line( color = "black" , size = 0.5 ) )

	cat( paste( "store plot for" , region , sep = ' ' ) )


    filename = paste( toString( region ) , "png" , sep = "." )
	ggsave( filename , width = w , height = h , dpi = 300 , units = "in" )

	#plots[[i]] = g
}

cat( "save plots\n" )
#w = 1500
#h = 900
#w2 = 1200
#h2 = 750
#for ( i in 1:length( plots ) ) {
#	filename = paste( toString( regions[i] ) , "png" , sep = "." )
#
#	png( filename , width = w2 , height = h2 , res = 300 )
#
#	print( plots[[i]] )
#
#	dev.off( ) 
#}

