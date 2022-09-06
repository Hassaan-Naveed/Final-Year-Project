library(ggplot2)
library(hrbrthemes)
library(nortest)

id <- c("1", "2", "3")

suds_csv <- read.table("D:/Documents/Work/Uni Year 3/FYP/Submission/Code and Ethics/R Code/SUDS.csv", header=TRUE, sep=",", row.names=id)
psd_csv <- read.table("D:/Documents/Work/Uni Year 3/FYP/Submission/Code and Ethics/R Code/PSD.csv", header=TRUE, sep=",", row.names=id)


#PSD MEAN
new.psd_mean <- function() {
	before_mean <- mean(psd_csv$Before)
	after_mean <- mean(psd_csv$After)	
	change_mean <- mean(psd_csv$Change)
	
	
	paste("Before Mean (PSD): ", before_mean)
	paste("After Mean (PSD): ", after_mean)
	paste("Change Mean (PSD): ", change_mean)
}

#SUDS MEAN
new.suds_mean <- function() {
	before_mean <- mean(suds_csv$Before)
	after_mean <- mean(suds_csv$After)	
	change_mean <- mean(suds_csv$Change)

	paste("Before Mean (SUDS): ", before_mean)
	paste("After Mean (SUDS): ", after_mean)
	paste("Change Mean (SUDS): ", change_mean)	
}

#PSD STANDARD DEVIATION
new.psd_sd <- function() {
	before_sd <- sd(psd_csv$Before)
	after_sd <- sd(psd_csv$After)
	change_sd <- sd(psd_csv$Change)

	paste("Before SD (PSD): ", before_sd)
	paste("After SD (PSD): ", after_sd)
	paste("Change SD (PSD): ", change_sd)	
}

#SUDS STANDARD DEVIATION
new.suds_sd <- function() {
	before_sd <- sd(suds_csv$Before)
	after_sd <- sd(suds_csv$After)
	change_sd <- sd(suds_csv$Change)

	paste("Before SD (SUDS): ",before_sd)
	paste("After SD (SUDS): ",after_sd)
	paste("Change SD (SUDS): ",change_sd)
}


#BAR CHART FOR VISUALISING SUDS
new.suds_bar <- function() {
	value = c(suds_csv$Before, suds_csv$After)
	participant <- rep(c("P1", "P2", "P3"), 2)
	condition <- c(rep("Before", 3), rep("After", 3))

	data <- data.frame(participant,condition,value)

	ggplot(data, aes(fill=reorder(condition, -value), y=value, x=participant)) + geom_bar(position="dodge", stat="identity")
}

#BAR CHART FOR VISUALISING PSD
new.psd_bar <- function() {
	value = c(psd_csv$Before, psd_csv$After)
	participant <- rep(c("P1", "P2", "P3"), 2)
	condition <- c(rep("Before", 3), rep("After", 3))

	data <- data.frame(participant,condition,value)

	ggplot(data, aes(fill=reorder(condition, +value), y=value, x=participant)) + geom_bar(position="dodge", stat="identity")
}

#BAR CHART FOR VISUALISING CHANGE
new.change_bar <- function() {
	value = c(psd_csv$Change, suds_csv$Change)
	participant <- rep(c("P1", "P2", "P3"), 2)
	condition <- c(rep("PSD", 3), rep("SUDS", 3))

	change_data <- data.frame(participant,condition,value)

	ggplot(change_data, aes(fill=condition, y=value, x=participant)) + geom_bar(position="stack", stat="identity")
}


#PSD BOXPLOT FOR ASSESSING OUTLIERS
new.psd_boxplot <- function() {
	
	value <- c(psd_csv$Before, psd_csv$After, psd_csv$Change)
	participant <- rep(c("P1", "P2", "P3"), 3)
	condition <- c(rep("Before", 3), rep("After", 3), rep("Change", 3))

	data <- data.frame(participant,condition,value)

	ggplot(data, aes(x=condition, y=value, fill=participant)) + geom_boxplot() + facet_wrap(~participant)
}

#SUDS BOXPLOT FOR ASSESSING OUTLIERS
new.suds_boxplot <- function() {
	
	value <- c(suds_csv$Before, suds_csv$After, suds_csv$Change)
	participant <- rep(c("P1", "P2", "P3"), 3)
	condition <- c(rep("Before", 3), rep("After", 3), rep("Change", 3))

	data <- data.frame(participant,condition,value)

	ggplot(data, aes(x=condition, y=value, fill=participant)) + geom_boxplot() + facet_wrap(~participant)
}


#SCATTERGRAM FOR ASSESSING CORRELATION
new.scatter <- function() {
	
	psd_x <- c(psd_csv$Change)
	suds_y <- c(suds_csv$Change)
	participant <- rep(c("P1", "P2", "P3"), 2)

	scatter_data <- data.frame(participant,psd_x,suds_y)

	ggplot(scatter_data, aes(x=psd_x, y=suds_y)) + geom_point() + geom_smooth(method=lm , color="red", se=FALSE) + theme_ipsum()
}


#DENSITY DISTRIBUTION PSD
new.density_psd <- function() {
	
	before_x <- c(psd_csv$Before)
	after_x <- c(psd_csv$After)
	participant <- rep(c("P1", "P2", "P3"), 2)

	density_data <- data.frame(participant,before_x,after_x)

	ggplot(density_data, aes(x=x) ) +
  	  # Top
  	  geom_density( aes(x = before_x, y = ..density..), fill="#69b3a2" ) +
  	  geom_label( aes(x=4.5, y=0.25, label="Before"), color="#69b3a2") +
  	  # Bottom
  	  geom_density( aes(x = after_x, y = -..density..), fill= "#404080") +
  	  geom_label( aes(x=4.5, y=-0.25, label="After"), color="#404080") +
  	  theme_ipsum()
}

#DENSITY DISTRIBUTION SUDS
new.density_suds <- function() {
	
	before_x <- c(suds_csv$Before)
	after_x <- c(suds_csv$After)
	participant <- rep(c("P1", "P2", "P3"), 2)

	density_data <- data.frame(participant,before_x,after_x)

	ggplot(density_data, aes(x=x) ) +
  	  # Top
  	  geom_density( aes(x = before_x, y = ..density..), fill="#69b3a2" ) +
  	  geom_label( aes(x=4.5, y=0.25, label="Before"), color="#69b3a2") +
  	  # Bottom
  	  geom_density( aes(x = after_x, y = -..density..), fill= "#404080") +
  	  geom_label( aes(x=4.5, y=-0.25, label="After"), color="#404080") +
  	  theme_ipsum()
}

#DENSITY DISTRIBUTION CHANGE
new.density_change <- function() {
	
	psd_x <- c(psd_csv$Change)
	suds_x <- c(suds_csv$Change)
	participant <- rep(c("P1", "P2", "P3"), 2)

	density_data <- data.frame(participant,psd_x,suds_x)

	ggplot(density_data, aes(x=x) ) +
  	  # Top
  	  geom_density( aes(x = psd_x, y = ..density..), fill="#69b3a2" ) +
  	  geom_label( aes(x=4.5, y=0.25, label="change in psd"), color="#69b3a2") +
  	  # Bottom
  	  geom_density( aes(x = suds_x, y = -..density..), fill= "#404080") +
  	  geom_label( aes(x=4.5, y=-0.25, label="change in suds ratings"), color="#404080") +
  	  theme_ipsum()
}


#ANDERSON-DARLING PSD
new.ad_psd<- function() {
	set.seed(1)
	x <- psd_csv$Change
	
	ad.test(x)
}

#ANDERSON-DARLING SUDS
new.ad_suds <- function() {
	set.seed(1)
	x <- suds_csv$Change
	
	ad.test(x)
}


#WILCOXON PSD
new.wilc_psd <- function() {
  rm(list = ls())
  
  x <- psd_csv$Before
  y <- psd_csv$After
  
  wilcox.test(x, y, paired = TRUE, alternative = "less")
  
}

#WILCOXON SUDS
new.wilc_suds <- function() {
  rm(list = ls())
  
  x <- suds_csv$Before
  y <- suds_csv$After
  
  wilcox.test(x, y, paired = TRUE, alternative = "greater")
  
}

#TTEST PSD
new.wilc_psd <- function() {
  rm(list = ls())
  
  x <- psd_csv$Before
  y <- psd_csv$After
  
  t.test(x, y, paired = TRUE, alternative = "less")
  
}

#TTEST SUDS
new.wilc_suds <- function() {
  rm(list = ls())
  
  x <- suds_csv$Before
  y <- suds_csv$After
  
  t.test(x, y, paired = TRUE, alternative = "greater")
  
}

#PEARSONS R
new.pearson <- function() {
	rm(list = ls())

	x <- psd_csv$Change
	y <- suds_csv$Change
	
	cor.test(x, y,alternative = "greater", method = "pearson", exact=FALSE )
}

#SPEARMANS RANK
new.spearman <- function() {
	rm(list = ls())

	x <- psd_csv$Change
	y <- suds_csv$Change
	
	cor.test(x, y,alternative = "greater", method = "spearman", exact=FALSE )
}