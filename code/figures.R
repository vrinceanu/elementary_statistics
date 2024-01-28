#!/usr/bin/env Rscript
w = 6.673828
h = 8.607422
dy=1.8*h
point=72

col1 = "#6f0732"
col2 = "#EB6F71"
col3 = "#FEECF3"
col4 = "#0E5EB6"

if(!require('berryFunctions'))install.packages('berryFunctions')
library('berryFunctions')


fig2.7 <- function(){
	R = matrix(c(cos(2*pi/7), sin(2*pi/7),-sin(2*pi/7),cos(2*pi/7)), ncol=2)
	u1 = c(0,1);
	w <-c(u1); z<-u1;
	for(k in 1:6){z <- R%*%z; w <- c(w, z)}

	u2 = c(0,-1)*1/(1+2*sin(pi/14));
	w <- c(w, u2); z<-u2;
	for(k in 1:6){z <- R%*%z; w <- c(w, z)}

	u3 = c(0,1)*sin(pi/14)/cos(pi/7);
	w <- c(w, u3); z<-u3;
	for(k in 1:6){z <- R%*%z; w <- c(w, z)}

	x =matrix(w, nrow=2)[1,]
	y =matrix(w, nrow=2)[2,]

	pdf("fig2_7.pdf")
	par(ann=FALSE, bg="transparent", mai=c(0.2,0.2,0.2,0.2))
	plot.new()
	par(usr=c(-1.1,1.1,-1.1, 1.1), family="sans")
  
	for(k in 1:7){for(j in 1:7){if(j< k + 6 && j > k+1){
		lines(c(x[k],x[j]),c(y[k],y[j]), col=col4, lw=3)
	}}}

	for(k in 8:14){for(j in 8:14){if(j< k + 5 && j > k+2){
		lines(c(x[k],x[j]),c(y[k],y[j]), col=col4, lw=3)
	}}}
	
	points(x,y, pch=21, col=col1, bg=col2, cex=2, lwd=4)
	dev.off()
}


fig3.9 <- function(){
	TH = 10
	TW = 50
    pdf(file="fig3_9.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.1,0.1,0.1,0.1), adj=0.5)
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans")
	
	lines(c(2,48)*w,c(1,1)*dy)	
	for(k in (0:8)){
		lines(c(5+k*5,5+k*5)*w,c(0.8,1.2)*dy)
	}
	arrows(14*w, 2*dy, 31*w, 2*dy, code=3,length=0.1)
	lines(c(14,14)*w, c(1.7,3.4)*dy)
	lines(c(31,31)*w, c(1.7,3.4)*dy)
	text(22.5*w, 2.5*dy,"IQR")
	
	text(7*w, 9*dy, "Minimum")
	text(14*w, 4*dy, expression(Q[1]))
	text(20*w, 9*dy, "Median")
	text(20*w, 4*dy, expression(Q[2]))
	text(44*w, 9*dy, "Maximum")
	text(31*w, 4*dy, expression(Q[3]))
	lines(c(20,20)*w, c(5, 8)*dy, col=col1, lwd=3)
	rect(14*w, 5*dy, 31*w, 8*dy, border=col4, lwd=2)
	lines(c(7,14)*w, c(6.5,6.5)*dy, col=col4, lwd=2)
	lines(c(7,7)*w, c(6.2, 6.8)*dy, col=col4, lwd=2)
	lines(c(31,44)*w, c(6.5,6.5)*dy, col=col4, lwd=2)
	lines(c(44,44)*w, c(6.2, 6.8)*dy, col=col4, lwd=2)
    
	
	dev.off()
}

fig5.1 <- function(){
    pdf(file="fig5_1.pdf")
	par(family="sans")
	
	x = seq(0,10,by=0.04)
	y1 = dnorm(x, mean=2, sd=0.5)
	y2 = dnorm(x, mean=4, sd=0.5)
	y3 = dnorm(x, mean=5, sd=1)
	y4 = dnorm(x, mean=6, sd=2)
	
	plot(x, y1,type='l', col="black", lwd=2,
	  ylab=expression("Probability Density Function"~italic(dnorm(x, mu, sigma))))
	lines(x, y2,type='l', col=col2, lwd=2)
    lines(x, y3,type='l', col=col1, lwd=2)
	lines(x, y4,type='l', col=col4,lwd=2)
	grid()
	legend(5.8,0.8, legend=c(
		expression(list(mu == 2, sigma == 0.5)),
		expression(list(mu == 4, sigma == 0.5)),
		expression(list(mu == 5, sigma == 1)),
		expression(list(mu == 6, sigma == 2))
		),
	  col = c("black",col2, col1, col4), lty=1, lwd = 3, bg="white", cex=1.4)
 	dev.off()
}


fig5.6 <- function(){
    pdf(file="fig5_6.pdf")
	par(family="sans")
	
	x = seq(0,1,by=0.01)
    y = dbeta(x, 2, 10)
	
	plot(x, y,type='l', col=col4, lwd=3, ylab="F(x)", main= "Probability Distribution of x")
	grid()
 	dev.off()
}

fig5.7 <- function(){
    pdf(file="fig5_7.pdf")
	par(family="sans")
	
	x = seq(0,1,by=0.01)
    y = dnorm(x, 0.5, 0.13)
	
	plot(x, y,type='l', col=col4, lwd=3, ylab="F(x)", main= "Probability Distribution of x")
	grid()
 	dev.off()
}

fig6.1 <- function(){
    pdf(file="fig6_1.pdf")
	par(family="sans")
	
	x = 0:30
	y1 = dbinom(x, size= 20, prob=0.5)
	y2 = dbinom(x, size= 20, prob=0.75)
	y3 = dbinom(x, size= 40, prob=0.5)
	
	plot(x, y1,type="p", col="black", pch = 22, cex=1.4, lwd=2, ylim=c(0,0.21),
	  ylab=expression("Probability Distribution Function"~italic(dbinom(x, n, p))))
	for (k in x){
		lines(c(k,k),c(0, y1[k+1]), lwd=1, lty="dashed", col="black")
		lines(c(k+0.1,k+0.1),c(0, y2[k+1]), lwd=1, lty="dashed", col=col2)
		lines(c(k-0.1,k-0.1),c(0, y3[k+1]), lwd=1, lty="dashed", col=col4)
	}
	points(x,y2, pch = 21, cex=1.4, lwd=2, col=col2)
	points(x,y3, pch = 24, cex=1.4, lwd=2, col=col4)
	
	
	grid()
	legend(18.5,0.215, legend=c("n = 20, p = 0.5", "n = 20, p = 0.75", 
    "n = 40, p = 0.5"), col = c("black",col2, col4), 
	pch = c(22,21, 24), bg="white", cex=1.4, pt.lwd=2)
 	dev.off()
}


fig7.2 <- function(){
    TH = 30
    TW = 103
    y = TH*dy
    pdf(file="fig7_2.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.1,0.1,0.1,0.1), adj=0.5)
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans")
	s = 10*w
	x = seq(-40,40)
	y = exp(-x**2/475)
	lines((x + 60)*w, (8+20*y)*dy, col=col1, lwd=3)
	
	x95 = 19.6; y95 = exp(-x95**2/475)
	x99 = 25.8; y99 = exp(-x99**2/475)
	arrows((60 - x95)*w, (8+20*y95)*dy, (60+x95)*w, (8+20*y95)*dy, code=3, length=0.15)
	text((56 - x95)*w, (8+20*y95)*dy, bquote(paste("-1.96", sigma)))
	text((63.5 + x95)*w, (8+20*y95)*dy, bquote(paste("1.96", sigma)))
	arrows((60 - x99)*w, (8+20*y99)*dy, (60+x99)*w, (8+20*y99)*dy, code=3, length=0.15)
	text((56 - x99)*w, (8+20*y99)*dy, bquote(paste("-2.58", sigma)))
	text((64 + x99)*w, (8+20*y99)*dy, bquote(paste("2.58", sigma)))
    for(k in (-4:3)){
    	j = (4 + k)*10 + 1
		text((65+x[j])*w, (8.4 + 10*y99)*dy, sprintf("%6.4f", pnorm(k+1)-pnorm(k)))
    }

	arrows(20*w,8*dy, 100*w, 8*dy, lwd=2, code=3, length=0.1, angle=20)
	lines(c(20,100)*w, c(2,2)*dy,lwd=1)
	lines(c(20,100)*w, c(4,4)*dy,lwd=1)
	lines(c(20,100)*w, c(6,6)*dy,lwd=1)
	
	for(k in (-4:4)){
		j = (4 + k)*10 + 1
		lines((c(x[j],x[j])+60)*w,(8+20*c(0,y[j]))*dy, lty="dashed")
		lines((c(x[j],x[j])+60)*w,c(1.7, 2.3)*dy)
		lines((c(x[j],x[j])+60)*w,c(3.7, 4.3)*dy)
		lines((c(x[j],x[j])+60)*w,c(5.7, 6.3)*dy)
		if(k == 0){
			text((x[j]+60)*w, 1*dy, (k+5)*10)
			text((x[j]+60)*w, 3*dy, "0")
			text((x[j]+60)*w, 5*dy, "50%")
			text((x[j]+60)*w, 7*dy, "0")
		    } else{
			text((x[j]+60)*w, 1*dy, (k+5)*10)
		    text((x[j]+60)*w, 3*dy, sprintf("%+3.1f", k))
			text((x[j]+60)*w, 5*dy, sprintf("%3.1f%%", pnorm(k)*100))
			text((x[j]+60)*w, 7*dy, bquote(paste(.(sprintf("%+d",k)),sigma)))
	        }
	}
	
	lines(c(60*w,60*w), c(7.7, 8.3)*dy, lwd=3)
	lines(c(60*w,60*w), c(27.7, 28.3)*dy, lwd=3)
	text(60*w, 29*dy, "x", cex=1.3)
	lines(c(59.5*w,60.5*w), c(29.5, 29.5)*dy, lwd=2)

	rect(54*w, (8.1+20*y95)*dy, 66*w, (9+20*y95)*dy, col="white", border=NA)
	text(60*w, (8.5+20*y95)*dy,"95% of values", col=col2)
	rect(54*w, (8.1+20*y99)*dy, 66*w, (9+20*y99)*dy, col="white", border=NA)
	text(60*w, (8.5+20*y99)*dy,"99% of values", col=col2)
	
	text(28*w, 24*dy,"The Normal\nDistribution", cex=2)
    arrows(16*w, 21*dy, 39*w, 21*dy, length=0.1, col=col2)
	arrows(16*w, 21*dy, 16*w, 27*dy, length=0.1, col=col2)
	text(27.5*w, 20.5*dy, "Values", cex=0.8, col=col2)
	text(15.*w, 24*dy, "Probability", srt=90, cex=0.8, col=col2)

	text(17*w, (8 + 10*y99)*dy, "Probability of cases\nin portions of the curve", adj=1, cex=0.9)
	text(15*w, 7*dy, "Standard deviations\nfrom the mean", adj=1, cex=0.9)
	text(15*w, 5*dy, "Cumulative %", adj=1, cex=0.9)
	text(15*w, 3*dy, "Z scores", adj=1, cex=0.9)
	text(15*w, 1*dy, "T Scores", adj=1, cex=0.9)
	
	dev.off()
}

fig7.4 <- function(){
    pdf(file="fig7_4.pdf", width=6, height=4)	
	x = seq(-3,3,by=0.01)
    y = dnorm(x, 0., 1.)
	xr = x[x<=0.76]
	yr = y[x<=0.76]	
	plot.new()
	par(usr=c(-3,3,0,0.41), family="sans")
	polygon(c(xr, xr[length(xr)], xr[1]), c(yr, 0, 0), col=col2, border=NA)
	lines(x, y,type='l', col=col4, lwd=3)
	axis(side=1, at = c(-3,-2.5,-2,-1.5,-1,-0.5, 0,0.5,1,1.5,2,2.5,3), cex.axis=0.8)
	axis(side=2, at = c(0, 0.1, 0.2, 0.3, 0.4), cex.axis=0.8)
	title(main="Standard Normal Distribution", xlab="x", ylab="F(x)")
	grid()
 	dev.off()
}

fig7.5 <- function(){
    pdf(file="fig7_5.pdf", width=6, height=4)
	mai=c(0.1,0.1,0.1,0.1)
	x = seq(-3,3,by=0.01)
    y = dnorm(x, 0., 1.)
	xr = x[x<=-0.49]
	yr = y[x<=-0.49]	
	plot.new()
	par(usr=c(-3,3,0,0.41), family="sans")
	polygon(c(xr, xr[length(xr)], xr[1]), c(yr, 0, 0), col=col2, border=NA)
	lines(x, y,type='l', col=col4, lwd=3)
	axis(side=1, at = c(-3,-2.5,-2,-1.5,-1,-0.5, 0,0.5,1,1.5,2,2.5,3), cex.axis=0.8)
	axis(side=2, at = c(0, 0.1, 0.2, 0.3, 0.4), cex.axis=0.8)
	title(main="Standard Normal Distribution", xlab="x", ylab="F(x)")
	grid()
 	dev.off()
}

fig7.13 <- function(){
    pdf(file="fig7_13.pdf")	
	x = seq(20,180,by=2)
    y = dnorm(x, 100, 25)
 
    plot(x,y,type='l', col=col4, lwd=3, xlab='x', ylab="F(x)")
	grid()
 	dev.off()
}

fig10.2 <- function(){
    pdf(file="fig10_2.pdf", width=7, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	x = seq(0,8,by=0.04)
	y1 = dchisq(x, 1)
	y2 = dchisq(x, 2)
	y3 = dchisq(x, 3)
	y4 = dchisq(x, 4)	
	y5 = dchisq(x, 5)
	
	plot(x, y1,type='l', col="black", lwd=2, ylim=c(0.,1.),
	  ylab=expression("Probability Density Function"~italic(dchisq(x,k))))
	lines(x, y2,type='l', col=col2, lwd=2)
	lines(x, y3,type='l', col=col1, lwd=2)
	lines(x, y4,type='l', col=col4, lwd=2)
	lines(x, y5,type='l', col="orange",lwd=2)
	grid()
	legend(6.5,1.0, legend=c("k=1","k=2","k=3","k=4","k=5"),
	  col = c("black",col2, col1, col4,"orange"), lty=1, lwd = 3, bg="white")
 	dev.off()
}

fig10.3 <- function(){
    pdf(file="fig10_3.pdf", width=7, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	x = seq(0,8,by=0.04)
	y1 = pchisq(x, 1)
	y2 = pchisq(x, 2)
	y3 = pchisq(x, 3)
	y4 = pchisq(x, 4)	
	y5 = pchisq(x, 5)
	
	plot(x, y1,type='l', col="black", lwd=2, ylim=c(0.,1.),
	  ylab=expression("Probability Distribution Function"~italic(pchisq(x,k))))
	lines(x, y2,type='l', col=col2, lwd=2)
    lines(x, y3,type='l', col=col1, lwd=2)
	lines(x, y4,type='l', col=col4, lwd=2)
	lines(x, y5,type='l', col="orange",lwd=2)
	grid()
	legend(6.5,0.38, legend=c("k=1","k=2","k=3","k=4","k=5"),
	  col = c("black",col2, col1, col4,"orange"), lty=1, lwd = 3, bg="white")
 	dev.off()
}

fig10.6 <- function(){
    pdf(file="fig10_6.pdf", width=7, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	x = seq(0,20,by=0.04)
	y = dchisq(x, 4)
	
	a = 0.01
	xR = qchisq(1 - a, 4)
	xr = x[x>= xR]
	yr = y[x>= xR]	
	
	plot.new()
	par(usr=c(0,20,0,0.2), family="sans")
	polygon(c(xr, xr[length(xr)], xr[1]), c(yr, 0, 0), col=col2, border=NA)
	lines(x, y,type='l', col=col4, lwd=2)
	lines(c(xR,xR),c(0,0.07))
	text(14, 0.04, "Rejection region", adj=0)
	text(14, 0.03, expression(paste("to the right of ",italic(x)[R])), adj=0)
	text(14, 0.02, bquote("has an area of 0.01"), adj=0)
	text(16, 0.08, expression(paste(italic(x)[R]," = ", italic(qchisq(0.99, 4))," = 13.277")))
	axis(side=1, cex.axis=0.8)
	axis(side=2, cex.axis=0.8)
#	grid()
	title(xlab = expression(italic(x)), ylab=expression(paste("chi-squared distribution with ",italic(df)," = 4")))
 	dev.off()
}

fig10.8 <- function(){
    pdf(file="fig10_8.pdf", width=7, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	x = seq(0,20,by=0.04)
	y = dchisq(x, 1)
	
	a = 0.01
	xR = qchisq(1 - a, 1)
	xr = x[x>= xR]
	yr = y[x>= xR]	
	
	plot.new()
	par(usr=c(0,10,0,.2), family="sans")
	polygon(c(xr, xr[length(xr)], xr[1]), c(yr, 0, 0), col=col2, border=NA)
	lines(x, y,type='l', col=col4, lwd=2)
	lines(c(xR,xR),c(0,0.09))
	text(7, 0.05, "Rejection region", adj=0)
	text(7, 0.04, expression(paste("to the right of ",italic(x)[R])), adj=0)
	text(7, 0.03, bquote("has an area of 0.01"), adj=0)
	text(8, 0.1, expression(paste(italic(x)[R]," = ", italic(qchisq(0.99, 1))," = 6.635")))
	axis(side=1, cex.axis=0.8)
	axis(side=2, cex.axis=0.8)
#	grid()
	title(xlab = expression(italic(x)), ylab=expression(paste("chi-squared distribution with ",italic(df)," = 1")))
 	dev.off()
}

fig10.9 <- function(){
    pdf(file="fig10_9.pdf", width=7, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	x = seq(0,16,by=0.04)
	y = dchisq(x, 4)
	
	a = 0.05
	xR = qchisq(1 - a, 4)
	xr = x[x>= xR]
	yr = y[x>= xR]	
	
	plot.new()
	par(usr=c(0,16,0,.2), family="sans")
	polygon(c(xr, xr[length(xr)], xr[1]), c(yr, 0, 0), col=col2, border=NA)
	lines(x, y,type='l', col=col4, lwd=2)
	arrows(xr[1],0.08,xr[1],0.0, length=0.1)
	text(xR, 0.09, "9.488")
	axis(side=1, cex.axis=0.8)
	axis(side=2, cex.axis=0.8)
	grid()
	title(xlab = expression(italic(x)), ylab=expression(paste("chi-squared distribution with ",italic(df)," = 4")))
 	dev.off()
}


fig10.13 <- function(){
	pdf(file="fig10_13.pdf", width=7, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	x = seq(0,1.2,by=0.001)
	n1 = 2261
	n2 = 9601
	y = df(x,n1, n2)
	
	a = 0.05
	xR = qf(1 - a, n1, n2)
	xr = x[x>= xR]
	yr = y[x>= xR]	
	
	plot.new()
	par(usr=c(0,1.2,0,12.5), family="sans")
	polygon(c(xr, xr[length(xr)], xr[1]), c(yr, 0, 0), col=col2, border=NA)
	lines(x, y,type='l', col=col4, lwd=2)
	axis(side=1, cex.axis=0.8)
	axis(side=2, cex.axis=0.8)
	lines(c(xR,xR),c(0,6))
	grid()
	textField(0.6, 4.5, paste0("Rejection region area = ",a), fill=col3)
	arrows(0.6,4,1.07, 0.4, length=0.1)
	text(1.045, 6.5, expression(italic(F)[crit]),adj=0)
	title(xlab = expression(italic(F)),
	   ylab=expression(paste(italic(F),"-distribution with ",italic('df1')," = 2261 and ",italic('df2')," = 9601")))
 	dev.off()
}

fig10.16 <- function(){
	pdf(file="fig10_16.pdf", width=7, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	x = seq(0,12,by=0.001)
	n1 = 2
	n2 = 10
	y = df(x,n1, n2)
	
	a = 0.01
	xR = qf(1 - a, n1, n2)
	xr = x[x>= xR]
	yr = y[x>= xR]	
	
	plot.new()
	par(usr=c(0,12,0,0.2), family="sans")
	polygon(c(xr, xr[length(xr)], xr[1]), c(yr, 0, 0), col=col2, border=NA)
	lines(x, y,type='l', col=col4, lwd=2)
	axis(side=1, cex.axis=0.8)
	axis(side=2, cex.axis=0.8)
	lines(c(xR,xR),c(0,0.05))
	grid()
	textField(4.5, .15, paste0("Rejection region area = ",a), fill=col3)
	arrows(4.5,0.14,7.8, 0.002, length=0.1)
	text(7.5, 0.058, expression(italic(F)[crit]),adj=0)
	title(xlab = substitute(italic("F")),
	   ylab=expression(paste(italic(F),"-distribution with ",italic('df1')," = 2 and ",italic('df2')," = 10")))
 	dev.off()
}

fig10.18 <- function(){
    pdf(file="fig10_18.pdf", width=4.5, height=4.5)
	par(family="sans", mar=c(4,5,1,1))
	
	G = matrix(rnorm(50000), ncol=5)
	H = G[,1]^2 + G[,2]^2 + G[,3]^2 + G[,4]^2 + G[,5]^2
	q = seq(0, 20, 0.1); c2 = dchisq(q,5)
	hist(H, breaks=20, freq=FALSE, main="chi-square distribution", col = col3, 
	    xlim=c(0,18),ylim=c(0, 0.18), xlab=substitute(italic("H")));
	lines(q, c2, col = col4, lwd=2)	
	dev.off()
}

fig10.19 <- function(){
    pdf(file="fig10_19.pdf", width=7.5, height=7.5)
	par(family="sans", mar=c(4,5,1,1))
	N = 4*57
	ne = 100000
	probs = c(.24, .20, .16, .14, .13, .13)
	colors = c('blue','orange','green', 'yellow', 'red', 'brown')
	expected = round(N*probs)
	X = rmultinom(ne, N, probs)
	Z = apply(X, 2, function(x) (x-expected)/sqrt(expected))
	xvals=seq(-4,4,0.01)
	par(mfrow=c(3,2))
	for(k in 1:6){
	    hist(Z[k,],breaks=20, freq=FALSE, col=col3, main=colors[k], xlab="")
	    lines(xvals, dnorm(xvals), col = col4, lw = 2)
	    }
	
	dev.off()
}

fig10.21 <- function(){
    pdf(file="fig10_21.pdf", width=4.5, height=4.5)
	par(family="sans", mar=c(4,5,2,1))
	
	curve(dchisq(x, df=5), col=col2, main = "Chi-square distribution with 5 dof", 
      xlab = expression(chi^2), from=0,to=20, lwd=2)
    grid()
  	dev.off()
  }

fig11.4 <- function(){
    pdf(file="fig11_4.pdf",)
	par(family="sans")
	
	x = seq(20,60, by=1)
	y = x/3 + 5 + rnorm(length(x), 0, 3)
	plot(x,y, pch=21, lwd=2, col=col1, bg="lightgray")
	abline(5, 1/3, col=col4, lwd=2)
 	dev.off()
}

fig11.5 <- function(){
    pdf(file="fig11_5.pdf",width=4.5, height=4.5)
	par(family="sans")
	set.seed(42)
	x <- 4:20
	y <- 5 + 3*x + 10*runif(17)
	xy.data <- data.frame(x,y)
	fit <- lm(y ~ x, xy.data)
	plot(x, y, type="n",xlim=c(0,21), ylim=c(0,75))
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=col3)
	points(x, y, col=col1, xlim=c(0,21), ylim=c(0,75), pch=19)
	grid(col="white")
	abline(coef=fit$coefficients,col=col4)
	text(6, 60, sprintf("y = %4.2f + %4.2f x", fit$coefficients[1], fit$coefficients[2]))
	dev.off()
}

fig11.11 <- function(){
    pdf(file="fig11_11.pdf", width=4.5, height=5.5)
	par(family="sans")
	par(mfrow=c(2,2), mai=c(0.7,0.7,0.2,0.1))
	x<-1:8; y <- 2 + 3*x + 4*rnorm(8);plot(x,y)
	x<-1:8; y <- 20 - 3*x + 4*rnorm(8);plot(x,y)
	x<-1:8; y <- 2 + exp(x) + 4*rnorm(8);plot(x,y)
	x<-1:8; y <- 10 + 4*rnorm(8);plot(x,y)
	dev.off()
}

fig12.6 <- function(){
    pdf(file="fig12_6.pdf", width=4.5, height=5.5)
	par(family="sans")
	A <- c(117.1, 121.3, 127.8, 121.9, 117.4, 124.5, 119.5, 115.1)
	B <- c(123.5, 125.3, 126.5, 127.9, 122.1, 125.6, 129.8, 117.2)
	dat <- data.frame(weight = c(A,B), company = rep(c("A","B"), each=8))
	z <- boxplot(weight ~ company, data = dat, col=col3, pars=list(medcol=col4))
	dev.off()
}

if(TRUE){
fig2.7()
fig3.9()
fig5.1()
fig5.6()
fig5.7()
fig6.1()
fig7.2()
fig7.4()
fig7.5()
fig7.13()
fig10.2()
fig10.3()
fig10.6()
fig10.8()
fig10.9()
fig10.13()
fig10.16()
fig10.18()
fig10.19()
fig10.21()
fig11.4()
fig11.5()
fig11.11()
fig12.6()
}




