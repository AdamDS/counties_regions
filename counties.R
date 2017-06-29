#load packages
library( 'ggplot2' )
library( 'reshape' )
library( 'plyr' )
library( 'grid' )
library( 'gridExtra' )
library( 'scales' )

#get pivot table
data = read.table( file = 'pivot.csv' , header = TRUE , sep = "," , stringsAsFactors = FALSE )

#extract the list of counties
counties = data$Geography[ data$Geography != "Missouri" ]

#rename raw data columns to what they will be in chart
data = rename( data , c( "X30DCig16" = "Cigarettes" ,
						 "X30DAlc16" = "Alcohol" ,
						 "X30DBinge16" = "Binge*" ,
						 "X30DMJ16" = "Marijuana" ,
						 "X30DInh16" = "Inhalants" ,
						 "X30DScript16" = "Rx misuse" ,
						 "X30DOTC16" = "OTC misuse" ,
						 "X30DSynthetic16" = "Synthetic" ,
						 "X2016.Past.month.electronic.cigarette.use" = "E-cigs" ,
						 "X2016.Past.month.hookah.use" = "Hookah"
						)
)

#remove the regions data columns
data$label = NULL
data$NSDUH.2012.14.reg = NULL
data$Sub.NSDUH.2012.14.Marijuana.Use.in.Past.Month = NULL
data$Sub.NSDUH.2012.14.Illicit.Drug.Use.other.than.Marijuana.in.Past.Mont = NULL
data$Sub.NSDUH.2012.14.Alcohol.Use.in.Past.Month = NULL
data$Sub.NSDUH.2012.14.Binge.Alcohol.Use.in.Past.Month = NULL
data$Sub.NSDUH.2012.14.Cigarette.Use.in.Past.Month = NULL
data$sub.NSDUH.2012.14.Nonmedical.Use.of.Pain.Relievers.Past.Year.Age.18. = NULL
data$label2 = NULL

#order Missouri to beginning
#relevel( data$Geography , "Missouri" )
state = " Missouri   "
data$Geography[ data$Geography == "Missouri" ] = state

#reform all data into factor data (id) and numerical, usage data type (variable) and percentage (value)
goop = melt( data , id = c( "Geography" ) )

#need a labels column above bars based on numerical values
goop$labels = goop$value

#clean up label precision to tenths
goop$labels = gsub( '%' , '' , goop$labels )
goop$labels = as.numeric( goop$labels )
goop$labels = format( round( goop$labels , 1 ) )
goop$labels = as.factor( goop$labels )
goop$labels = paste( goop$labels , "%" , sep = '' )

#remove percent sign & make values as numbers
goop$value = gsub( '%' , '' , goop$value )
goop$value = as.numeric( goop$value ) / 100

#custom chart parameters
chartTitle = ' ' #'Current Substance Use for Grades 6-12, 2016'
labelSize = 3
axisSize = 9
tickSize = 9
titleSize = 11
legendSize = 9
keySize = 0.25
purple = "#5e498B"
yellow = "#F8CB44"
palette = c( yellow , purple ) 
barWidth = 0.6
ymin = 0
ymax = .40
barSeparation = 0.075
labelSeparation = -barSeparation
barLabelAngle = 90
xOrder = c( "Alcohol" , "E-cigs" , "Rx misuse" , "Marijuana" , "Cigarettes" , "Binge*" , "Hookah" , "OTC misuse" , "Inhalants" , "Synthetic" )

#loop over all counties
for ( i in 1:length( counties ) ) {

	county = counties[i]

	#data to plot, always MO with county
	dat = data.frame( goop[ goop$Geography == state , ] )
	dat = rbind( dat , goop[ goop$Geography == county , ] )
	#dat$ordering = c(1:length(dat$Geography))
	#dat$Geography = factor( dat$Geography , levels = letters[1:length(dat$Geography)] )
	
	#begin plot, use category (variable) as x, corresponding values as y, color bars by state or county
	 g = ggplot( dat , aes( x = variable , y = value , fill = Geography ) )

	#bar plot, use values instead of count (identity), offset state & county bars with dodge
	g = g + geom_bar( stat = "identity" , position = position_dodge( width = barWidth + barSeparation ) , width = barWidth )
	
	#labels above bars
	g = g + geom_text( data = dat , aes( variable , value , label = labels ) , size = labelSize , hjust = labelSeparation , guide = FALSE , color = "black" , position = position_dodge( width = barWidth ) , angle = barLabelAngle )

	#use white background in chart area, remove background from around chart
	g = g + theme_bw( )
	g = g + theme( plot.background = element_rect( fill = "transparent" , colour = NA ) )
	
	#chart title text and format
	g = g + ggtitle( chartTitle  )
	g = g + theme( title = element_text( size = titleSize , color = purple , face = "bold" ) )
	
	#x-label removed, x tick labels formatted and ordered
	g = g + xlab( '' )
	g = g + theme( axis.text.x = element_text( size = axisSize , angle = 45 , vjust = 1 , hjust = 1 ) )
	g = g + scale_x_discrete( limits = xOrder ) 

	#y-label removed, y tick labels formatted
	g = g + ylab( "" )
	g = g + scale_y_continuous( limits = c( ymin , ymax ) , labels = percent , expand = c( 0 , 0 ) )
	g = g + theme( axis.text.y = element_text( size = axisSize ) )

	#legend shape formatting
	g = g + theme( legend.key.size = unit( keySize , "cm" ) )
	g = g + theme( legend.position = c( .5 , 0.85 ) )
	g = g + theme( legend.background = element_blank( ) )
	g = g + theme( legend.direction = "horizontal" )
	g = g + scale_fill_manual( values = palette )
	#g = g + scale_color_manual( values = palette )
	#g = g + guides( fill = guide_legend( override.aes = list( size = 3 ) ) )
	g = g + theme( legend.text.align = 1 )
	g = g + theme( legend.justification = 0 )

	#legend text and formatting
	g = g + theme( legend.title = element_blank() )
	g = g + theme( legend.text = element_text( size = legendSize ) )

	#format chart grid & boundaries
	g = g + theme( panel.grid.minor = element_blank() ,
	           panel.grid.major = element_blank() )
	g = g + theme( panel.border = element_blank() )
	g = g + theme( axis.line = element_line( color = "black" , size = 0.5 ) )

	#reduce margins around chart
	g = g + theme( plot.margin = unit( c( 0.1 , 0.12 , -0.15 , -0.15 ) , "in" ) )

	cat( paste( "store plot for" , county , sep = ' ' ) )
	#save charts as .png
	filename = paste( toString( county ) , "png" , sep = "." )
	ggsave( filename , width = 5 , height = 3 , dpi = 300 , units = "in" )
}
