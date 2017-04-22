#!/usr/bin/Rscript
##Get vector with variables for R
	args <- commandArgs(trailingOnly=TRUE)
#
		ELEVATION <- args[1]
		SLOPE <- args[2]
        LIMIT <- as.numeric(args[3])
		WATERCOURSE <- args[4]
		FLOWDIR <- args[5]
		AZIMUTH <- as.numeric(args[6])
		REPORT <- args[7]
		TERRACE_MAP <- args[8]
        LEVELS <- args[9]
        grassversion <- as.numeric(args[10])
##Setting working directory for output files
setwd(dirname(REPORT))
errorlog <- file("errorlog.Rout", open="wt")
sink(errorlog, type="message")
#
pdf(basename(REPORT), width=11, height=16, paper="a4r", pointsize=11)
#
##Install (if not present) and load required packages
###Check grass version
#
if (Sys.info()["sysname"] != "Windows") {
    installib <- .libPaths()
    if (grep("home", installib)) {
        installib <- grep("home", installib, value=T)
    } else {
        installib <- paste("/home/", Sys.getenv("LOGNAME"), "/R", sep="")
        .libPaths(c(.libPaths(), installib))
    }
} else {
    installib <- .libPaths()[1]
}
#
	if (grassversion == 7) {
			checkinstall <- suppressWarnings(require(rgrass7))
		if (checkinstall=="FALSE") {
				install.packages("GRANbase", dep=TRUE, lib=installib, repos='http://cran.us.r-project.org')
				library(rgrass7)
		} else {library(rgrass7)}
	} else {
			checkinstall <- suppressWarnings(require(spgrass6))
		if (checkinstall=="FALSE") {
				install.packages("spgrass6", dep=TRUE, lib=installib, repos='http://cran.us.r-project.org')
				library(spgrass6, lib.loc=libLocs)
		} else {library(spgrass6)}
	}
#
			checkinstall <- suppressWarnings(require(ggplot2))
	if (checkinstall=="FALSE") {
		install.packages("ggplot2", dep=TRUE, lib=installib, repos='http://cran.us.r-project.org')
		library(ggplot2)
	} else {
        if (packageVersion("ggplot2") >= "2.2.0") {
        library(ggplot2)
        } else {
        install.packages("ggplot2", dep=TRUE, lib=installib, repos='http://cran.us.r-project.org')
		library(ggplot2)}
    }
#
            checkinstall <- suppressWarnings(require(data.table))
	if (checkinstall=="FALSE") {
		install.packages("data.table", dep=TRUE, lib=installib, repos='http://cran.us.r-project.org')
		library(data.table)
	} else {library(data.table)}

#
#
##Export data from maps and read it as data.table
execGRASS("r.stats", input=c(ELEVATION,SLOPE,WATERCOURSE), output="tempout.txt", separator="space", flags=c("1", "g", "N", "overwrite")) #exporting elevation, slope and watercourse altitude values, omitting cells with missing data
VALUES <- fread("tempout.txt", header=FALSE, sep=" ", col.names=c("X","Y","ELEVATION","SLOPE","WATERCOURSE"), na.strings="*") #read data in data.table and name columns
#
##Prepare data
#Moving watercourse altitudes to new table
WATERCOURSE.data <- data.table(X=VALUES$X, Y=VALUES$Y, WATERCOURSE=VALUES$WATERCOURSE)
WATERCOURSE.data <- WATERCOURSE.data[complete.cases(WATERCOURSE.data)]
VALUES[, WATERCOURSE:=NULL]
VALUES <- VALUES[complete.cases(VALUES), ] #remove lines that only had watercourse elevations + values affected by slope calculation edge effect
#Getting relative elevations according to flow direction
if (FLOWDIR == "NS" | FLOWDIR == "SN") {
    WATERCOURSE.data[, WATERCOURSE_STRAIGHT:=mean(WATERCOURSE), by=Y] #handle meandering watercourse
    WATERCOURSE.data <- WATERCOURSE.data[!duplicated(WATERCOURSE.data$Y), ]
#Calculating distance values along watercourse
    # order watercourse by flow direction; distance is estimated at "curves", but it avoids problems occurring from meandering
    if (FLOWDIR == "NS") {
    WATERCOURSE.data <- WATERCOURSE.data[order(-Y)]
    } else {
    WATERCOURSE.data <- WATERCOURSE.data[order(Y)]}
    DISTANCE.calc <- 0
    forend <- nrow(WATERCOURSE.data)-1
        for (i in 1:forend) {
        DISTANCE.calc[i+1] <- DISTANCE.calc[i] + sqrt((WATERCOURSE.data[i+1,"X"]-WATERCOURSE.data[i,"X"])^2 + (WATERCOURSE.data[i+1,"Y"]-WATERCOURSE.data[i,"Y"])^2)
        }
    if (WATERCOURSE.data[1,"WATERCOURSE_STRAIGHT"] > WATERCOURSE.data[.N,"WATERCOURSE_STRAIGHT"]){
    DISTANCE.calc <- DISTANCE.calc[order(-DISTANCE.calc)]}
    WATERCOURSE.data[, DISTANCE:=DISTANCE.calc]
    VALUES <- merge(VALUES, WATERCOURSE.data[,c("Y","DISTANCE", "WATERCOURSE_STRAIGHT")], by="Y")
    VALUES <- VALUES[, RELEV:=ELEVATION-WATERCOURSE_STRAIGHT] #calculate relative elevations above watercourse
    } else {
    WATERCOURSE.data[, WATERCOURSE_STRAIGHT:=mean(WATERCOURSE), by=X] #handle meandering watercourse
    WATERCOURSE.data <- WATERCOURSE.data[!duplicated(WATERCOURSE.data$X), ]
#Calculating distance values along watercourse
    # order watercourse by flow direction; distance is estimated at "curves", but it avoids problems occurring from meandering
    if (FLOWDIR == "WE") {
    WATERCOURSE.data <- WATERCOURSE.data[order(X)]
    } else {
    WATERCOURSE.data <- WATERCOURSE.data[order(-X)]}
    DISTANCE.calc <- 0
    forend <- nrow(WATERCOURSE.data)-1
        for (i in 1:forend) {
        DISTANCE.calc[i+1] <- DISTANCE.calc[i] + sqrt((WATERCOURSE.data[i+1,"X"]-WATERCOURSE.data[i,"X"])^2 + (WATERCOURSE.data[i+1,"Y"]-WATERCOURSE.data[i,"Y"])^2)
        }
    if (WATERCOURSE.data[1,"WATERCOURSE_STRAIGHT"] > WATERCOURSE.data[.N,"WATERCOURSE_STRAIGHT"]){
    DISTANCE.calc <- DISTANCE.calc[order(-DISTANCE.calc)]}
    WATERCOURSE.data[, DISTANCE:=DISTANCE.calc]
    VALUES <- merge(VALUES, WATERCOURSE.data[,c("X","DISTANCE", "WATERCOURSE_STRAIGHT")], by="X")
    VALUES <- VALUES[, RELEV:=ELEVATION-WATERCOURSE_STRAIGHT][RELEV < 0, RELEV:=0] #calculate relative elevations above watercourse
    }
#
VALUES[, RELEV:=round(RELEV, digits=0)] #slope characteristics shall be analysed per every meter
VALUES[, SLOPE:=round(SLOPE, digits=1)] #filtered slopes are rounded to one 
##Subset data if there was an altitude limit set for the terrace extraction
if (LIMIT != 999){ 
VALUES <- VALUES[RELEV <= LIMIT]
}else{
LIMIT <- max(VALUES$ELEVATION)}
#
RESOLUTION <- as.numeric(round(VALUES[2,X] - VALUES[1,X]))
#
##Recalculate coordinates if azimuth was set *calculation based on Telbisz, T. et al. 2011 SwathCalc tool
FLOWDIR_DICT <- data.table(USER=c("NS","SN","EW","WE"), FROMNORTH=c(0,180,90,270)) # Always rotate the map to north, better handling further on
if (AZIMUTH != 999){
    pi <- 3.14159265358979
    centerx <- mean(VALUES$X)
    centery <- mean(VALUES$Y)
    SINUS <- sin(as.numeric(AZIMUTH)*pi/180)
    COSINUS <- cos(as.numeric(AZIMUTH)*pi/180)
    px.c <- VALUES$X - centerx
    py.c <- VALUES$Y - centery
    px.new <- px.c * COSINUS - py.c * SINUS
    py.new <- px.c * SINUS + py.c * COSINUS
    VALUES$OLD_X <- VALUES$X
    VALUES$OLD_Y <- VALUES$Y
    VALUES[, X:=px.new + centerx]
    seq.x <- round(seq(min(VALUES$X), max(VALUES$X), by=(max(VALUES$X) - min(VALUES$X))/length(unique(VALUES$OLD_X))), digits=1)
    VALUES[, X:=findInterval(X, seq.x)][, X:=seq.x[X+1]]
    VALUES[, Y:=py.new + centery]
    seq.y <- round(seq(min(VALUES$Y), max(VALUES$Y), by=(max(VALUES$Y) - min(VALUES$Y))/length(unique(VALUES$OLD_Y))), digits=1)
    VALUES[, Y:=findInterval(Y, seq.y)][, Y:=seq.y[Y+1]]
}else{
    if (FLOWDIR != "NS"){
        AZIMUTH <- FLOWDIR_DICT[USER %like% FLOWDIR,2]
        pi <- 3.14159265358979
        centerx <- mean(VALUES$X)
        centery <- mean(VALUES$Y)
        SINUS <- sin(as.numeric(AZIMUTH)*pi/180)
        COSINUS <- cos(as.numeric(AZIMUTH)*pi/180)
        px.c <- VALUES$X - centerx
        py.c <- VALUES$Y - centery
        px.new <- px.c * COSINUS - py.c * SINUS
        py.new <- px.c * SINUS + py.c * COSINUS
        VALUES$OLD_X <- VALUES$X
        VALUES$OLD_Y <- VALUES$Y
        VALUES$X <- round(px.new + centerx, digits=1)
        VALUES$Y <- round(py.new + centery, digits=1)
    }
}
#
###Defining thresholds of area and mean slope for altitudes based on topography of total study area
##Selecting cells with slope less than 13% and finding area ratio for potential/flat elevations
RELEV_ALL <- VALUES[, .N, by=RELEV]
RELEV_FLAT <- VALUES[SLOPE <= 13, .(.N, mean(SLOPE), sd(SLOPE)), by=RELEV]
setkey(RELEV_FLAT, RELEV)[RELEV_ALL, RATE:=N/RELEV_ALL$N*100] #based on the RELEV values the proportion of flat cells in a given altitude is calculated
setnames(RELEV_FLAT, old=c("RELEV","COUNT","MEAN","SD","RATE"))
RELEV_FLAT[RELEV_FLAT$MEAN < 1.2, MEAN:=1.2] #slopes under 0.7 degrees/1.2 percent [perfect plain] are too flat, misleading the threshold
#
##Calculating regression line equations to represent change of mean slope and minimum area threshold for every altitude
equation_MEAN <- lm(RELEV_FLAT$MEAN ~ RELEV_FLAT$RELEV)
coeffs_MEAN <- coefficients(equation_MEAN)
RELEV_FLAT$MEAN_LINE <- coeffs_MEAN[2]*RELEV_FLAT$RELEV + coeffs_MEAN[1]

equation_RATE <- lm(RELEV_FLAT$RATE ~ RELEV_FLAT$RELEV)
coeffs_RATE <- coefficients(equation_RATE)
RELEV_FLAT$RATE_LINE <- coeffs_RATE[2]*RELEV_FLAT$RELEV + coeffs_RATE[1]
#
###Plot change of thresholds and swath-profile of the total study site
##Create plot showing the histogram of cells under 13% slope and mean slopes, sd of slopes, linear equation lines for total area; *script base taken from strackoverflow answer
xmaxFLAT <- max(RELEV_FLAT$RELEV) #xmin is set to 0, sometimes negative values occur due to inconsistency of watercourse and elevation data
RELEV_FLAT[, MEAN.sc:=100/13*MEAN]
RELEV_FLAT[, SD.sc:=100/13*SD]
RELEV_FLAT[, MEAN_LINE.sc:=100/13*MEAN_LINE] # just for the time of plotting rescale MEAN SLOPE and its trendline to match 0-100 RATE
cols <- c("proportion"="darkgrey", "SD"="orange", "mean"="yellow")
PLOT1_5 <- ggplot(RELEV_FLAT, aes(x=RELEV))
PLOT1_5 <- PLOT1_5 +
    geom_area(aes(y=RATE, fill="proportion"), alpha=0.5) + scale_fill_manual(name=NULL, values=cols) +
    geom_line(aes(y=RATE_LINE), color="dimgrey", size=1.25)
PLOT1_5 <- PLOT1_5 +
    geom_point(aes(y=SD.sc, colour="SD"), shape=25, fill=NA, size=1.5) +
    geom_point(aes( y=MEAN.sc, colour="mean"), shape=17, size=3) +
    scale_colour_manual(name=NULL, values=cols) +
    geom_line(aes(y=MEAN_LINE.sc), color="yellow", size=1.25)
PLOT1_5 <- PLOT1_5 +
    scale_y_continuous(sec.axis=sec_axis(~.*0.13, name="Slope percent (%)"))
PLOT1_5 <- PLOT1_5 +
    xlim(0, xmaxFLAT) + labs(x="Potential rel. elev. (m)", y="Proportion (%)") +
    ggtitle( paste ("Proportion of surfaces under 13% slope and the mean&SD of slope values - TOTAL AREA")) +
    theme(text = element_text(size=14), plot.title=element_text(hjust=0.5), axis.text = element_text(size=14), axis.title=element_text(size=14),
    panel.background = element_rect(fill="whitesmoke", color="black"), panel.grid.minor = element_blank())
PLOT1_5 <- PLOT1_5 +
    theme(legend.position="bottom", legend.text=element_text(size=11))
print(PLOT1_5)
###########IMPORTANT: the tool uses the newest (version 2.2.0) of ggplot2 package############
#
##Create swath-profile (profile is always created from the floodplain, showing distance from stream)
#Calculate min, mean, max and distance
SWATHDATA <- VALUES[, .(MIN=min(ELEVATION), MEAN=mean(ELEVATION), MAX=max(ELEVATION)), by=.(X)][order(X)]
SWATHDATA$DIST <- trunc(SWATHDATA$X - min(SWATHDATA$X))
if (SWATHDATA[1,MIN] - SWATHDATA[.N,MIN] > 0){
    SWATHDATA$DIST <- max(SWATHDATA$DIST) - SWATHDATA$DIST
}
#
#Plot swath-profile
cols <- c("mean"="lightgreen", "min"="dodgerblue3", "max"="red")
PLOTSWATH <- ggplot(data=SWATHDATA, aes(x=DIST)) +
    geom_ribbon(aes(ymin=min(SWATHDATA$MIN), ymax=SWATHDATA$MAX), fill="white", alpha=0.5) +
    xlab("Distance from watercourse(m)") + ylab("Elevation a.s.l.(m)") +
    theme(text = element_text(size=14), plot.title=element_text(hjust=0.5), axis.text = element_text(size=14), axis.title=element_text(size=14), panel.background = element_rect(fill="whitesmoke", color="black"), panel.grid.minor = element_blank()) +
    ggtitle( paste ("Swath profile of region"))
PLOTSWATH <- PLOTSWATH  +
    geom_line(data=SWATHDATA, aes(x=DIST, y=MEAN, colour="mean"), size=1.25) + scale_colour_manual(name=NULL, values=cols)
PLOTSWATH <- PLOTSWATH +
    geom_line(data=SWATHDATA, aes(x=DIST, y=MIN, colour="min"), size=1.15, linetype="dashed") + scale_colour_manual(name=NULL, values=cols)
PLOTSWATH <- PLOTSWATH +
    geom_line(data=SWATHDATA, aes(x=DIST, y=MAX, colour="max"), size=1.15, linetype="dashed") + scale_colour_manual(name=NULL, values=cols)
PLOTSWATH <- PLOTSWATH +
    theme(legend.position="bottom", legend.text=element_text(size=11))
print(PLOTSWATH)
#
### Analyse the parallel sections of the area; as it is rotated only cutting from North to South, which means cutting from inlet point to outlet
#Section gives the distance for creating parallel sections from the DEM, piece is the number of sections for the given extent
AREAxmax <- max(VALUES$X, na.rm=TRUE)
AREAxmin <- min(VALUES$X, na.rm=TRUE)
AREAymax <- max(VALUES$Y, na.rm=TRUE)
AREAymin <- min(VALUES$Y, na.rm=TRUE)
section <- 330 # hardcoded based on trial-and-error method
piece <- (round((AREAymax-AREAymin)/section)+1)*2 # overlapping cutting
#
#Updating data.table with the results of terrace extraction
TERRACES <- data.table()
TERRACES.plotdata <- data.table()
try ( {
for (i in 1:piece) {
#The data is divided to sections by subsetting based on XY coordinates
    SECTIONymax <- AREAymax+section-i*section/2
    SECTIONymin <- AREAymax-i*section/2
TERRACE_SECT <- VALUES[VALUES$Y > SECTIONymin & VALUES$Y <= SECTIONymax]
    SECTIONxmax <- max(TERRACE_SECT$X, na.rm=TRUE)
    SECTIONxmin <- min(TERRACE_SECT$X, na.rm=TRUE)
MINIMUM <- TERRACE_SECT[, min(SLOPE), by=RELEV]
setnames(MINIMUM, old=c("RELEV.min","SLOPE.min"))
#Create the final plot with the scatter plot - x is relative elevation, y is slope, drawing the lower envelope curve for filtered slope
SECTIONx_old_min <- min(TERRACE_SECT$OLD_X)
SECTIONx_old_max <- max(TERRACE_SECT$OLD_X)
SECTIONy_old_min <- min(TERRACE_SECT$OLD_Y)
SECTIONy_old_max <- max(TERRACE_SECT$OLD_Y)
cols <- c("relative elevation ~ slope percent"="#771100", "slope min. curve"="black")
PLOT6_7 <- ggplot(TERRACE_SECT, aes(x=RELEV, y=SLOPE))
PLOT6_7 <- PLOT6_7 +
    geom_point(aes(fill="relative elevation ~ slope percent"), colour="#771100", alpha=.2) + scale_fill_manual(name=NULL, values=cols) +
    ylim(0,60) + xlab("Relative elevation(m)") + xlim(0, xmaxFLAT) + ylab("Slope(%)") +
    ggtitle( paste ("Terrace_scatterplot#", i, "X=",trunc(SECTIONx_old_min),":",trunc(SECTIONx_old_max),", "," Y=", trunc(SECTIONy_old_min),":",trunc(SECTIONy_old_max), sep="")) +
    theme(text = element_text(size=14), plot.title=element_text(hjust=0.5), axis.text = element_text(size=14), axis.title=element_text(size=14), panel.background = element_rect(fill="whitesmoke", color="black"), panel.grid.minor = element_blank())
PLOT6_7 <- PLOT6_7 +
    geom_line(data=MINIMUM, aes(x=RELEV.min, y=SLOPE.min, colour="slope min. curve"), size=1)  + scale_colour_manual(name=NULL, values=cols) +
    geom_hline(aes(yintercept=13), color="black", linetype="dotted")
PLOT6_7 <- PLOT6_7 +
    theme(legend.position="bottom", legend.text=element_text(size=11))
print(PLOT6_7)
#
##Find cells that could belong to a terrace surface
#Find the values on the lower envelope curve
CURVEPOTENTIALS <- MINIMUM[SLOPE.min <= 13]
#Based on the values from the envelope curve make a subset of those cells based on relative elevations
TERR_RELEV_POT_ALL <- TERRACE_SECT[RELEV %in% CURVEPOTENTIALS$RELEV.min]
#Subset the cells based on the maximum slope value & Calculating the ratio of low slope cells and every cell in a given relative elevation, calculating the mean and standard deviation of the slope value for potential cells by relative elevation
#tryCatch is necessary to continue for loop in case of no potential cells (NA data frames); in case of error a table with zero values will be created and plotted
TERR_RELEV_POT_FLAT <- tryCatch(
{TERR_RELEV_POT_FLAT <- TERR_RELEV_POT_ALL[SLOPE <= 13, .(.N, mean(SLOPE), sd(SLOPE)), by=RELEV]
TERR_RELEV_POT_ALL <- TERR_RELEV_POT_ALL[, .N, by=RELEV]
setkey(TERR_RELEV_POT_FLAT, RELEV)[TERR_RELEV_POT_ALL, RATE:=N/TERR_RELEV_POT_ALL$N*100] #based on the RELEV values the proportion of flat cells in a given altitude is calculated
setnames(TERR_RELEV_POT_FLAT, old=c("RELEV","COUNT","MEAN","SD","RATE"))},
error=function(e)
{RELEVerr <- 0
COUNTerr <- 0
MEANSLOPEerr <- 0
SDSLOPEerr <- 0
RATEerr <- 0
TERR_RELEV_POT_FLAT <- data.table(RELEV=RELEVerr, COUNT=COUNTerr, MEAN=MEANSLOPEerr, SD=SDSLOPEerr, RATE=RATEerr)}
)
#Create bar chart of cells ratio and points showing the mean and standard deviation values of slope
TERR_RELEV_POT_FLAT[, MEAN.sc:=100/13*MEAN]
TERR_RELEV_POT_FLAT[, SD.sc:=100/13*SD]
cols <- c("proportion"="darkgrey", "SD"="orange", "mean"="yellow")
PLOT8_12 <- ggplot(TERR_RELEV_POT_FLAT, aes(x=RELEV))
PLOT8_12 <- PLOT8_12 +
    geom_area(aes(y=RATE, fill="proportion"), alpha=0.5) + scale_fill_manual(name=NULL, values=cols)
PLOT8_12 <- PLOT8_12 +
    geom_point(aes(y=SD.sc, colour="SD"), shape=25, fill=NA, size=1.5) +
    geom_point(aes( y=MEAN.sc, colour="mean"), shape=17, size=3) +
    scale_colour_manual(name=NULL, values=cols)
PLOT8_12 <- PLOT8_12 +
    geom_line(data=RELEV_FLAT, aes(x=RELEV, y=RATE), color="dimgrey", linetype="dashed", size=0.75)
PLOT8_12 <- PLOT8_12 +
    geom_line(data=RELEV_FLAT, aes(x=RELEV, y=MEAN.sc), color="yellow", linetype="dashed", size=0.75)
PLOT8_12 <- PLOT8_12 +
    scale_y_continuous(sec.axis=sec_axis(~.*0.13, name="Slope percent (%)"))
PLOT8_12 <- PLOT8_12 +
    ggtitle( paste ("Potential terrace surfaces (Rel. elev. & Mean slope(%)", i, "X=",trunc(SECTIONxmin),":",trunc(SECTIONxmax),", "," Y=", trunc(SECTIONymin),":",trunc(SECTIONymax), sep="")) +
    theme(text = element_text(size=14), plot.title=element_text(hjust=0.5), axis.text = element_text(size=14), axis.title=element_text(size=14), panel.background = element_rect(fill="whitesmoke", color="black"), panel.grid.minor = element_blank()) +
    xlim(0, xmaxFLAT) + labs(x="Potential rel. elev. (m)", y="Proportion (%)")
PLOT8_12 <- PLOT8_12 +
    theme(legend.position="bottom", legend.text=element_text(size=11))
print(PLOT8_12)
#
#Possible terrace cells based on the threshold defined over the total area
THRSH <- RELEV_FLAT[RELEV %in% TERR_RELEV_POT_FLAT$RELEV]
TERR_RELEV_POT_FLAT <- TERR_RELEV_POT_FLAT[RATE >= THRSH$RATE & MEAN <= THRSH$MEAN]
#Get back coordinates and elevation for these, use again the slope threshold
TERRACE_POTENTIAL <- TERRACE_SECT[RELEV %in% TERR_RELEV_POT_FLAT$RELEV]
TERRACE_POTENTIAL <- merge(TERRACE_POTENTIAL, THRSH[,c("RELEV","MEAN","RATE")], by="RELEV")
TERRACE_POTENTIAL <- TERRACE_POTENTIAL[SLOPE <= MEAN]
#Get values of every section to the TERRACE table
TERRACES <- rbind(TERRACES, TERRACE_POTENTIAL)
#Prepare data for plotting with watercourse altitudes and elevation values; set the middle of watercourse section as distance value for plotting
WATERCOURSE_SECT <- data.table("WATERCOURSE_STRAIGHT"=TERRACE_SECT$WATERCOURSE_STRAIGHT, "DISTANCE"=TERRACE_SECT$DISTANCE)
meanWATER <- mean(WATERCOURSE_SECT$WATERCOURSE_STRAIGHT)
distWATER <- mean(WATERCOURSE_SECT$DISTANCE)
if(distWATER < 0) {
    distWATER <- 0
}
seq.relev <- seq(0, (LIMIT-meanWATER), by=5)
TERR_RELEV_POT_FLAT[, BINNED:=findInterval(RELEV, seq.relev)][, BINNED:=(seq.relev[BINNED+1]+meanWATER)]
plotdata <- data.table(TERR_RELEV_POT_FLAT, meanWATER, distWATER)
TERRACES.plotdata <- rbind(TERRACES.plotdata, plotdata)
}
} , silent=TRUE)
#
##Remove duplicated values from TERRACES (due to overlapping analysis)
TERRACES <- TERRACES[!duplicated(TERRACES), ]
##Statistical plot
LEVELS <- as.numeric(unlist(strsplit(LEVELS, ",")))
if (LEVELS[1] != 999) {
ranges <- data.table(bottom=LEVELS[seq(1,length(LEVELS),2)],top=LEVELS[seq(2,length(LEVELS),2)]) #get bottom and top values of terrace levels
} else {
ranges <- data.table(bottom=0, top=0)}
#
ymaxH <- max(TERRACES[, .N, by=RELEV])
cols <- c("levels"="#67B87E", "frequency"="purple4")
PLOT13 <- ggplot(TERRACES)
PLOT13 <- PLOT13 + 
    geom_rect(data=ranges, aes(xmin=bottom, xmax=top, ymin=0, ymax=ymaxH, fill="levels"), colour="white", alpha=0.5) + scale_fill_manual(name=NULL, values=cols) +
    xlim(0, xmaxFLAT) + labs(x="Altitude above watercourse (m)", y="Number of cells") +
    ggtitle("Frequency of terrace-like surfaces by relative altitudes") +
    theme(text = element_text(size=14), plot.title=element_text(hjust=0.5), axis.text = element_text(size=14), axis.title=element_text(size=14), panel.background = element_rect(fill="whitesmoke", color="black"), panel.grid.major= element_line(), panel.grid.minor = element_blank())
PLOT13 <- PLOT13 +
    geom_freqpoly(aes(RELEV, colour="frequency"), binwidth=1, size=1.25) + scale_colour_manual(name=NULL, values=cols)
PLOT13 <- PLOT13 +
    theme(legend.position="bottom", legend.text=element_text(size=11))
print(PLOT13)
#
##Filter data in 5 m groups, as most studies consider 5 m as active floodplain and also the normal fluctuation of streams/rivers, it helps to distunguish parts of slopes on lower altitudes with less erosion
#Filter is based on the statistical approach when a value is considered outlier if it deviates with more than 1.5x from mean
FILTER <- TERRACES[, .N, by=RELEV]
seq.filter <- seq(0, max(FILTER$RELEV), by=5)
FILTER[, FILTGROUP:=findInterval(RELEV, seq.filter)]
FILTER[, MEAN:=mean(N), by=FILTGROUP]
FILTER <- FILTER[N*1.5 > MEAN, ]
TERRACES <- TERRACES[RELEV %in% FILTER$RELEV]
##Plot possible terrace elevation values to river profile
xmaxW <- max(TERRACES$DISTANCE)
ymaxE <- max(TERRACES$ELEVATION)
yminW <- min(TERRACES$WATERCOURSE_STRAIGHT)
PLOT14 <- ggplot()
PLOT14 <- PLOT14 +
    geom_line(data=TERRACES, aes(x=DISTANCE, y=WATERCOURSE_STRAIGHT), colour="dodgerblue3", size=0.75) + scale_colour_manual(name=NULL, values=cols) + 
    xlim(0, xmaxW) + xlab("Distance from local outlet point (m)") + ylim(yminW, ymaxE) + ylab("Elevation a.s.l.(m)") +
    scale_x_reverse() +
    ggtitle("Terrace-like surfaces along the long profile of the watercourse") +
    theme(text = element_text(size=14), plot.title=element_text(hjust=0.5), axis.text = element_text(size=14), axis.title=element_text(size=14), panel.background = element_rect(fill="whitesmoke", color="black"), panel.grid.major= element_line(), panel.grid.minor = element_blank())
PLOT14 <- PLOT14 +
    geom_point(data=TERRACES.plotdata, aes(x=distWATER, y=BINNED, colour=RELEV), shape=19, size=3, alpha=0.5) + scale_colour_gradientn(colours=terrain.colors(8), name="elevations a.s.l. (m)")
PLOT14 <- PLOT14 +
    theme(legend.position="bottom", legend.title=element_text(size=11), legend.text=element_text(size=11))
print(PLOT14)
#
dev.off()
#
#Drop columns of TERRACES except X,Y,Z to recreate map in GRASS GIS
TERRACESfile <-  file.path(dirname(REPORT), "tempmap.txt")
TERRACEMAP <- data.table(TERRACES$OLD_X, TERRACES$OLD_Y, TERRACES$ELEVATION)
write.table(TERRACEMAP, file=TERRACESfile, row.names=FALSE, col.names=FALSE)
execGRASS("r.in.xyz", input=TERRACESfile, output=paste("terracemap_", ELEVATION, sep=""), separator="space")
file.remove("tempout.txt")
file.remove(TERRACESfile)
sink()
