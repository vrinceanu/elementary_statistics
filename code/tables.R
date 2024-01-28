#!/usr/bin/env Rscript
library(gmp)

w = 6.673828
h = 8.607422
dy=1.8*h
point=72

col1 = "#6f0732"
col2 = "#EB6F71"
col3 = "#FEECF3"

table.1 <- function(){
  TH = 2.4+0.6+15+0.1
  TW = 60
  y = TH*dy
  pdf(file="table_1.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02), adj=1)
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans")

  rect(0,0,TW*w,y, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 1", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h,"Factorials", adj=c(0,0.5), cex=1.2)
  y = y - 2.4*dy
  text(2.5*w,y, "n", font=2);text(18*w, y, "n!", font=2)
  text(23*w,y, "n", font=2);text(59*w, y, "n!", font=2)
  y = y-0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y+0.4*dy
  for(n in 1:15){
    y = y - dy; text(2.5*w, y, n); text(18*w, y, factorialZ(n))
    text(23*w, y, n+15);text(59*w, y, factorialZ(n+15))
    }
	dev.off()
}

table.2A <- function(){
  TH = 2.4 + 0.7 + sum((2:8)+1) + 7*1.3
  TW = 69
  y = TH*dy
  pdf(file="table_2A.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02), adj=1)
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans")

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 2A", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h, expression(paste("The Binomial Distribution ",italic("dbinom(k,n,p)"))), adj=c(0,0.5), cex=1.2)
  y = y - 2.4*dy
  text(2*w,y,"p",adj=c(1, 0.5), font=2)
  p = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
  x=(1:11)*(6*w) + 2*w
  text(x,rep(y,11), p, font=2)
  y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), lwd=1)
  for (n in 2:8){
    y = y - 0.5*dy; text(w, y, sprintf("k  n = %2d", n), adj=0);
    y = y - 0.6*dy; lines(c(0,9*w),c(y,y), col="gray");
    y = y + 0.4*dy
    for (k in 0:n){
      y = y - dy
      line = sprintf("%.3f",dbinom(k, n, p))
      text(0.4+2*w,y,k,adj=1)
      text(x,rep(y,10),line)
      }
    y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), col="gray")
  }
  dev.off()
}

table.2B <- function(){
  TH = 2.4 + 0.7 + sum((9:12)+1) + 4*1.3
  TW = 69
  y = TH*dy
  pdf(file="table_2B.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02), adj=1)
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans")

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 2B", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h,expression(paste("The Binomial Distribution ",italic("dbinom(k,n,p)"))), adj=c(0,0.5), cex=1.2)
  y = y - 2.4*dy
  text(2*w,y,"p",adj=c(1, 0.5), font=2)
  p = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
  x=(1:11)*(6*w) + 2*w
  text(x,rep(y,11), p, font=2)
  y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), lwd=1)
  for (n in 9:12){
    y = y - 0.5*dy; text(w, y, sprintf("k  n = %2d", n), adj=0);
    y = y - 0.6*dy; lines(c(0,9*w),c(y,y), col="gray");
    y = y + 0.4*dy
    for (k in 0:n){
      y = y - dy
      line = sprintf("%.3f",dbinom(k, n, p))
      text(0.4+2*w,y,k,adj=1)
      text(x,rep(y,10),line)
      }
    y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), col="gray")
  }
  dev.off()
}

table.3 <- function(){
  TH = 2.4 + 0.7 + 6*7 + 6*1.3
  TW = 69
  y = TH*dy
  pdf(file="table_3.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02), adj=1)
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans")

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 3", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h, expression(paste("The Negative Binomial Distribution ",italic("dnbinom(k,r,p)"))), adj=c(0,0.5), cex=1.2)
  y = y - 2.4*dy
  text(2*w,y,"p",adj=c(1, 0.5), font=2)
  p = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
  x=(1:11)*(6*w) + 2*w
  text(x,rep(y,11), p, font=2)
  y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), lwd=1)
  for (n in 1:6){
    y = y - 0.5*dy; text(w, y, sprintf("k  r = %2d", n), adj=0);
    y = y - 0.6*dy; lines(c(0,9*w),c(y,y), col="gray");
    y = y + 0.4*dy
    for (k in 0:6){
      y = y - dy
      line = sprintf("%.3f",dnbinom(k, n, p))
      text(0.4+2*w,y,k,adj=1)
      text(x,rep(y,10),line)
      }
    y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), col="gray")
  }
  dev.off()
}


table.4A <- function(){
  TH = 2.7 + 3*2 + (8+10+13)
  TW = 73
  y = TH*dy
  pdf(file="table_4A.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans")

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 4A", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h, expression(paste("The Poisson Distribution ",italic("dpois(x, lambda)"))), adj=c(0,0.5), cex=1.2)
  y = y - 2.4*dy
  x = (1:10)*7*w - 0.5*w
  lines(c(3*w, 3*w),c(0, TH*dy-3*h), lwd=1)
  text(1.5*w, y, "x", font=2)
#
  text(35*w, y, expression(lambda), font=2)
  y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), lwd=1)
  lambda = seq(0.1, 1.0, by=0.1)
  y = y - 0.5*dy; text(x, rep(y, 10), lambda, font=2)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy;
  for(k in 0:7){
    y = y - dy; text(2.5*w, y, k, adj=1);
    line = sprintf("%.4f", dpois(k, lambda))
    text(x, rep(y,10), line)
  }
#
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.5*dy;
  text(35*w, y, expression(lambda), font=2)
  y = y - 0.6*dy; lines(c(3*w,TW*w),c(y,y), lwd=1)
  lambda = seq(1.1, 2.0, by=0.1)
  y = y - 0.5*dy; text(x, rep(y, 10), lambda, font=2)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy;
  for(k in 0:9){
    y = y - dy; text(2.5*w, y, k, adj=1);
    line = sprintf("%.4f", dpois(k, lambda))
    text(x, rep(y,10), line)
  }
#
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.5*dy;
  text(35*w, y, expression(lambda), font=2)
  y = y - 0.6*dy; lines(c(3*w,TW*w),c(y,y), lwd=1)
  lambda = seq(2.1, 3.0, by=0.1)
  y = y - 0.5*dy; text(x, rep(y, 10), lambda, font=2)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy;
  for(k in 0:12){
    y = y - dy; text(2.5*w, y, k, adj=1);
    line = sprintf("%.4f", dpois(k, lambda))
    text(x, rep(y,10), line)
  }
  dev.off()
}

table.4B <- function(){
  TH = 2.7 + 3*2 + (15 + 16 + 17)
  TW = 73
  y = TH*dy
  pdf(file="table_4B.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans")

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 4B", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h,expression(paste("The Poisson Distribution ",italic("dpois(x, lambda)"))), adj=c(0,0.5), cex=1.2)
  y = y - 2.4*dy
  x = (1:10)*7*w - 0.5*w
  lines(c(3*w, 3*w),c(0, TH*dy-3*h), lwd=1)
  text(1.5*w, y, "x", font=2)
#
  text(35*w, y, expression(lambda), font=2)
  y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), lwd=1)
  lambda = seq(3.1, 4.0, by=0.1)
  y = y - 0.5*dy; text(x, rep(y, 10), lambda, font=2)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy;
  for(k in 0:14){
    y = y - dy; text(2.5*w, y, k, adj=1);
    line = sprintf("%.4f", dpois(k, lambda))
    text(x, rep(y,10), line)
  }
#
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.5*dy;
  text(35*w, y, expression(lambda), font=2)
  y = y - 0.6*dy; lines(c(3*w,TW*w),c(y,y), lwd=1)
  lambda = seq(4.1, 5.0, by=0.1)
  y = y - 0.5*dy; text(x, rep(y, 10), lambda, font=2)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy;
  for(k in 0:15){
    y = y - dy; text(2.5*w, y, k, adj=1);
    line = sprintf("%.4f", dpois(k, lambda))
    text(x, rep(y,10), line)
  }
#
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.5*dy;
  text(35*w, y, expression(lambda), font=2)
  y = y - 0.6*dy; lines(c(3*w,TW*w),c(y,y), lwd=1)
  lambda = seq(5.1, 6.0, by=0.1)
  y = y - 0.5*dy; text(x, rep(y, 10), lambda, font=2)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy;
  for(k in 0:16){
    y = y - dy; text(2.5*w, y, k, adj=1);
    line = sprintf("%.4f", dpois(k, lambda))
    text(x, rep(y,10), line)
  }
  dev.off()
}

table.5 <- function(){
  TH = 2.5 + 50
  TW = 91
  y = TH*dy
  pdf(file="table_5.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans")

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 5", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h,expression(paste("Uniform Random Numbers ", italic("runif()"))), adj=c(0,0.5), cex=1.2)
  y = y - 3*h
  h = 1:12
  x = h*7.6*w - 3.8*w
  XR = runif(12*50)
  for(k in (1:50)){
    y = y - dy
    line = sprintf("%.5f", XR[h])
    h = h + 12
    text(x, rep(y,12), line)
  }
  dev.off()
}

table.6 <- function(){
  TH = 2.5 + 50
  TW = 91
  y = TH*dy
  pdf(file="table_6.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 6", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h,expression(paste("Normal Random Numbers ", italic("rnorm()"))), adj=c(0,0.5), cex=1.2)
  y = y - 3*h
  h = 1:11
  x = h*8.2*w
  XR = rnorm(11*50)
  for(k in (1:50)){
    y = y - dy
    line = sprintf("%.05f", XR[h])
    h = h + 11
    text(x, rep(y,11), line)
  }
  dev.off()
}

table.7A <- function(){
  TH = 3.5 + 35
  TW = 76
  y = TH*dy
  pdf("table_7A.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 7A", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h,expression(paste("Cumulative Standard Normal Distribution ", italic("pnorm()"))), adj=c(0,0.5), cex=1.2)
  dz = 0:9
  x = dz*7*w + 9*w
  z = 3.4
  y = y - 2.4*dy
  text(3*w, y, "z")
  text(x, y, sprintf("%.2f", dz*0.01), adj=0.5)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.3*dy
  lines(c(5*w,5*w),c(0,TH*dy-3*h), lwd=1)
  for(k in (1:35)){
    y = y - dy
    text(4*w, y, paste("-",sprintf("%.1f",z),sep=""))
    line = sprintf("%.4f", pnorm(-(z+dz*0.01)))
    z = signif(z - 0.1,8)
    text(x, rep(y,11), line, adj=0.5)
  }
  dev.off()
}

table.7B <- function(){
  TH = 3.5 + 35
  TW = 76
  y = TH*dy
  pdf("table_7B.pdf", width=TW*w/point, height=TH*dy/point)
  par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
  plot.new()
  par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

  rect(0,0,TW*w,TH*dy, col=col3)
  rect(0,y-3*h,15*w,y, col=col1)
  text(7.5*w, y-1.5*h,"Table 7B", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
  rect(15*w,y-3*h,TW*w,y, col=col2)
  text(16*w,y-1.5*h,expression(paste("Cumulative Standard Normal Distribution ", italic("pnorm()"))), adj=c(0,0.5), cex=1.2)
  dz = 0:9
  x = dz*7*w + 9*w
  z = 0
  y = y - 2.4*dy
  text(3*w, y, "z")
  text(x, y, sprintf("%.2f", dz*0.01), adj=0.5)
  y = y - 0.5*dy;lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.3*dy
  lines(c(5*w,5*w),c(0,TH*dy-3*h), lwd=1)
  for(k in (1:35)){
    y = y - dy
    text(4*w, y, sprintf("%.1f",z))
    line = sprintf("%.4f", pnorm(z+dz*0.01))
    z = z + 0.1
    text(x, rep(y,11), line, adj=0.5)
  }
  dev.off()
}

table.8 <- function(){
    TH = 3.5 + 48
    TW = 62
    y = TH*dy
    pdf("table_8.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 8", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,expression(paste("The t Distribution ", italic("qt((1+c)/2, df)"))), adj=c(0,0.5), cex=1.2)
    lines(c(12*w,12*w),c(0, y-3*h), lwd=1)
	y = y - 2.4*dy
	cl = c(0.8, 0.85, 0.9, 0.95, 0.98, 0.99)
	df =  c(seq(1,30), seq(35, 80, by=5), c(90),c(100),c(500),c(1000))
	text(6*w, y, "C. I.", font=2, adj=0.5)
	text(7*w + 10*w*(1:6), rep(y,6), cl, adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(6*w, y, expression(paste("one-tailed ",alpha)), font=2, adj=0.5)
	ota  = c(0.1, 0.075, 0.05,0.025,0.01, 0.005)
    text(7*w + 10*w*(1:6), rep(y,6), ota, adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(6*w, y, expression(paste("two-tailed ",alpha)), font=2, adj=0.5)
	text(7*w + 10*w*(1:6), rep(y,6), 2*ota, adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(6*w, y, "df", adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.3*dy
	for(k in df){
		y = y-dy
		text(10*w, y, k, adj=1)
		line = formatC(qt(0.5*(1+cl),k),digits=5,format="fg", flag="#")
		text(10*w + 10*w*(1:6), rep(y,6), substr(line,1,6), adj=1)
	}
	y = y-dy
	text(10*w, y, expression(infinity), adj=1, cex=1.4)
	line = formatC(qt(0.5*(1+cl),1.e6),digits=5,format="fg", flag="#")
	text(10*w + 10*w*(1:6), rep(y,6), substr(line,1,6), adj=1)
	dev.off()
}
table.9 <- function(){	
    TH = 3.5 + 38
    TW = 76
    y = TH*dy
    pdf("table_9.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 9", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,expression(paste("The Chi-Square Distribution ", italic("qchisq(alpha, df)"))), adj=c(0,0.5), cex=1.2)
    lines(c(5*w,5*w),c(0, y-3*h), lwd=1)
	y = y - 2.4*dy
	text(41*w,y,expression(alpha), adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(5*w,TW*w),c(y,y), lwd=1); y = y + 0.3*dy
	y = y - dy
	text(2.5*w, y, "df", adj=0.5, font=2)
	al = c(0.005, 0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99, 0.995)
	text(2*w + 7*w*(1:10), rep(y,10), al, adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.3*dy
	
	df =  c(c(1:30),c(4:10)*10)
	for(k in df){
		y = y - dy
		text(4*w, y, k, adj=1)
		line = formatC(qchisq(al,k),digits=5,format="fg", flag="#")
		text(5*w + 7*w*(1:10), rep(y,10), substr(line,1,6), adj=1)
	}
	dev.off()
}

table.10A <- function(){	
    TH = 3.5 + 46
    TW = 70
    y = TH*dy
    pdf("table_10A.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 10A", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,expression(paste("The F Distribution ", 
	    italic("qf(1-alpha, df1, df2)"), " for ", alpha," = 0.005")), adj=c(0,0.5), cex=1.2)
    lines(c(4*w,4*w),c(0, y-3*h), lwd=1)
	y = y - 2.4*dy
	alpha = 0.005
	text(2.5*w, y, "df1", font=2, adj=0.5)
	text(35*w, y, "df2", font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(8*w, y, 1, font=2); text(14*w, y, 2, font=2);
	text(13*w + 6*w*(1:12), y, c(3,4,5,6,7,8,9,10,11), font=2)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	df1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,20,30,40,50,60,120,Inf)
	for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		text(10*w,  y, formatC(qf(1-alpha, k, 1), format="f", digits=0), adj=1)
		text(15*w, y, formatC(qf(1-alpha, k, 2), format="f", digits=1), adj=1)
		line = formatC(qf(1-alpha, k, c(3,4,5,6,7,8,9,10,11)), digits=3, format="f", flag="#")
		text(14.5*w + 6*w*(1:9), rep(y, 9), substr(line,1,5), adj=1)
	}

	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(2*w + 6*w*(1:10), y, c(12, 13, 14, 15,20,30,40,50,60,120), font=2)
	text(68*w, y, expression(infinity), font=2, cex=1.6)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	
   for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		line = formatC(qf(1-alpha, k, c(12, 13, 14, 15,20,30,40,50,60,120,Inf)), digits=3, format="f", flag="#")
		text(3*w + 6*w*(1:11), rep(y, 11), substr(line,1,5), adj=1)
	}
	dev.off()
}

table.10B <- function(){	
    TH = 3.5 + 46
    TW = 70
    y = TH*dy
    pdf("table_10B.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 10B", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,expression(paste("The F Distribution ", 
	    italic("qf(1-alpha, df1, df2)"), " for ", alpha," = 0.01")), adj=c(0,0.5), cex=1.2)
    lines(c(4*w,4*w),c(0, y-3*h), lwd=1)
	y = y - 2.4*dy
	alpha = 0.01
	text(2.5*w, y, "df1", font=2, adj=0.5)
	text(35*w, y, "df2", font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(8*w, y, 1, font=2); text(14*w, y, 2, font=2);
	text(13*w + 6*w*(1:12), y, c(3,4,5,6,7,8,9,10,11), font=2)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	df1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,20,30,40,50,60,120,Inf)
	for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		text(9*w,  y, formatC(qf(1-alpha, k, 1), format="f", digits=0), adj=1)
		text(15*w, y, formatC(qf(1-alpha, k, 2), format="f", digits=2), adj=1)
		line = formatC(qf(1-alpha, k, c(3,4,5,6,7,8,9,10,11)), digits=3, format="f", flag="#")
		text(14.5*w + 6*w*(1:9), rep(y, 9), substr(line,1,5), adj=1)
	}

	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(2*w + 6*w*(1:10), y, c(12, 13, 14, 15,20,30,40,50,60,120), font=2)
	text(68*w, y, expression(infinity), font=2, cex=1.6)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	
   for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		line = formatC(qf(1-alpha, k, c(12, 13, 14, 15,20,30,40,50,60,120,Inf)), digits=3, format="f", flag="#")
		text(3*w + 6*w*(1:11), rep(y, 11), substr(line,1,5), adj=1)
	}
	dev.off()
}

table.10C <- function(){	
    TH = 3.5 + 46
    TW = 70
    y = TH*dy
    pdf("table_10C.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 10C", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,expression(paste("The F Distribution ", 
	    italic("qf(1-alpha, df1, df2)"), " for ", alpha," = 0.05")), adj=c(0,0.5), cex=1.2)
    lines(c(4*w,4*w),c(0, y-3*h), lwd=1)
	y = y - 2.4*dy
	alpha = 0.05
	text(2.5*w, y, "df1", font=2, adj=0.5)
	text(35*w, y, "df2", font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(8*w, y, 1, font=2); text(14*w, y, 2, font=2);
	text(13*w + 6*w*(1:12), y, c(3,4,5,6,7,8,9,10,11), font=2)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	df1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,20,30,40,50,60,120,Inf)
	for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		text(9*w,  y, formatC(qf(1-alpha, k, 1), format="f", digits=1), adj=1)
		text(15*w, y, formatC(qf(1-alpha, k, 2), format="f", digits=2), adj=1)
		line = formatC(qf(1-alpha, k, c(3,4,5,6,7,8,9,10,11)), digits=3, format="f", flag="#")
		text(14.5*w + 6*w*(1:9), rep(y, 9), substr(line,1,5), adj=1)
	}

	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(2*w + 6*w*(1:10), y, c(12, 13, 14, 15,20,30,40,50,60,120), font=2)
	text(68*w, y, expression(infinity), font=2, cex=1.6)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	
   for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		line = formatC(qf(1-alpha, k, c(12, 13, 14, 15,20,30,40,50,60,120,Inf)), digits=3, format="f", flag="#")
		text(3*w + 6*w*(1:11), rep(y, 11), substr(line,1,5), adj=1)
	}
	dev.off()
}

table.10D <- function(){	
    TH = 3.5 + 46
    TW = 70
    y = TH*dy
    pdf("table_10D.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 10D", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,expression(paste("The F Distribution ", 
	    italic("qf(1-alpha, df1, df2)"), " for ", alpha," = 0.1")), adj=c(0,0.5), cex=1.2)
    lines(c(4*w,4*w),c(0, y-3*h), lwd=1)
	y = y - 2.4*dy
	alpha = 0.1
	text(2.5*w, y, "df1", font=2, adj=0.5)
	text(35*w, y, "df2", font=2)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(8*w, y, 1, font=2); text(14*w, y, 2, font=2);
	text(13*w + 6*w*(1:12), y, c(3,4,5,6,7,8,9,10,11), font=2)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	df1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,20,30,40,50,60,120,Inf)
	for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		text(9*w,  y, formatC(qf(1-alpha, k, 1), format="f", digits=2), adj=1)
		text(15*w, y, formatC(qf(1-alpha, k, 2), format="f", digits=3), adj=1)
		line = formatC(qf(1-alpha, k, c(3,4,5,6,7,8,9,10,11)), digits=3, format="f", flag="#")
		text(14.5*w + 6*w*(1:9), rep(y, 9), substr(line,1,5), adj=1)
	}

	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(2*w + 6*w*(1:10), y, c(12, 13, 14, 15,20,30,40,50,60,120), font=2)
	text(68*w, y, expression(infinity), font=2, cex=1.6)
	y = y - 0.5*dy; lines(c(4*w,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	
   for(k in df1){
		y = y - dy
		if(k!=Inf) text(3.5*w, y, k, adj=1, font=2) else 
		 {text(3.5*w, y, expression(infinity), adj=1, font=2, cex=1.6)}
		line = formatC(qf(1-alpha, k, c(12, 13, 14, 15,20,30,40,50,60,120,Inf)), digits=3, format="f", flag="#")
		text(3*w + 6*w*(1:11), rep(y, 11), substr(line,1,5), adj=1)
	}
	dev.off()
}

table.11 <- function(){	
    TH = 8 + 20
    TW = 64
    y = TH*dy
    pdf("table_11.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 11", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,"Critical values for Pearson Correlation Coefficient", adj=c(0,0.5), cex=1.2)
	y = y - 2.8*dy
    al = c(0.1, 0.05, 0.01)
	
	text(2*w, y, 
	  expression(paste("Reject ",italic(H)[0], " : ", rho," = 0 if the absolute value of ",
	  italic(r)," is greater than the value given")), adj=0)
    y = y - dy
	text(2*w, y, expression(paste("in the table. The values are for a two-tailed test; df = ", italic(n - 2))), adj=0)
	y = y - dy
	lines(c(6*w, 6*w), c(0,y), lwd=1)
	lines(c(38*w, 38*w), c(0,y), lwd=1)
	lines(c(32*w, 32*w), c(0,y), lwd=1)
	lines(c(32.2*w, 32.2*w), c(0,y), lwd=1)
	lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.7*dy
	text(19*w, y, expression(alpha))
	text(51*w, y, expression(alpha))
	y = y -0.5*dy; lines(c(6*w,32*w),c(y,y), lwd=1); 
	lines(c(38*w,TW*w),c(y,y), lwd=1); 
	y = y - 0.7*dy
	text(3*w, y, "df", font=2, adj=0.5)
	text(35*w, y, "df", font=2, adj=0.5)
	text(2*w + 8*w*(1:3), y, c("0.1", "0.05", "0.01"), font=2, adj=0.5)
	text(34*w + 8*w*(1:3), y, c("0.1", "0.05", "0.01"), font=2, adj=0.5)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
   
    y0=y	
	for(k in 1:20){
 	   	y = y - dy
		text(4*w, y, k)
		line = formatC(qt(1-al/2, k)/sqrt(k + qt(1-al/2, k)**2), format="f", digits=4)
		text(5*w + 8*w*(1:3),y, line, adj=1)
	}
	y=y0
	for(k in c(c(20:30),35, 40, 45, 10*(5:10))){
 	   	y = y - dy
		text(36*w, y, k)
		line = formatC(qt(1-al/2, k)/sqrt(k + qt(1-al/2, k)**2), format="f", digits=4)
		text(37*w + 8*w*(1:3),y, line, adj=1)
	}		
    dev.off()	
}

table.12 <- function(){	
    TH = 8 + 26
    TW = 46
    y = TH*dy
    pdf("table_12.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 12", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,"Critical values for the Sign Test", adj=c(0,0.5), cex=1.2)
	y = y - 2.8*dy
    al = c(0.005, 0.01, 0.025, 0.05, 0.1)
	
	text(2*w, y, "Reject the null hypothesis if the smaller number of", adj=0)
    y = y - dy
	text(2*w, y, "positive or negative signs is less than or equal to the", adj=0)
	y = y - dy
	text(2*w, y, "value in the table", adj=0)
	y = y - dy	
    lines(c(8*w, 8*w),c(0, y), lwd=1)
    lines(c(0,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(28*w, y, expression(paste("one-tailed ", alpha)), adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(10*w,(TW-2)*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(6*w + 7*w*(1:5), y, c("0.005", "0.01", "0.025", "0.05", "0.10"), font=2, adj=0.5)
	y = y - 0.5*dy; lines(c(8*w,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(28*w, y, expression(paste("two-tailed ", alpha)), adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(10*w,(TW-2)*w),c(y,y), lwd=1); y = y - 0.6*dy
	
    text(6*w, y, "n", font=2)
	text(6*w + 7*w*(1:5), y, c("0.01", "0.02", "0.05", "0.10", "0.20"), font=2, adj=0.5)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	for(k in 8:30){
		y = y - dy
		text(6*w, y, k, adj=1)
		line = qbinom(al, k, 0.5) - 1
		text(6*w + 7*w*(1:5), y, line, adj=0.5)
	}
    dev.off()	
}

table.13 <- function(){	
    TH = 8 + 25
    TW = 64
    y = TH*dy
    pdf("table_13.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-3*h,15*w,y, col=col1)
    text(7.5*w, y-1.5*h,"Table 13", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-3*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,"Critical values for the Wilcoxon Signed-Rank Test", adj=c(0,0.5), cex=1.2)
	y = y - 1.8*dy
    al=c(0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.2)

    lines(c(8*w, 8*w),c(0, TH*dy-3*h), lwd=1)
    y = y - 0.5*dy
	text(35*w, y, expression(paste("one-tailed ", alpha)), adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(10*w,(TW-2)*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(5*w + 8*w*(1:7), y, c("0.0005", "0.0025", "0.005","0.0125", "0.025","0.05", "0.10"), font=2, adj=0.5)
	y = y - 0.5*dy; lines(c(8*w,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(35*w, y, expression(paste("two-tailed ", alpha)), adj=0.5, font=2)
	y = y - 0.5*dy; lines(c(10*w,(TW-2)*w),c(y,y), lwd=1); y = y - 0.6*dy
	
    text(4*w, y, "n", font=2, adj=0.5)
	text(5*w + 8*w*(1:7), y, c("0.001", "0.005", "0.01","0.025","0.05", "0.10", "0.20"), font=2, adj=0.5)
	y = y - 0.5*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	for(k in 5:30){
		y = y - dy
		text(5*w, y, k, adj=1)
		line = qsignrank(al/2, k)-1
		lt = sprintf("%d", line)
		lt[line<0] = "--"
		text(5*w + 8*w*(1:7), y, lt, adj=0.5)		
	}	
    dev.off()
}

if(!require(SuppDists)){install.packages("SuppDists")}
library(SuppDists)

table.14 <- function(){	
    TH = 8 + 24
    TW = 50
    y = TH*dy
    pdf("table_14.pdf", width=TW*w/point, height=TH*dy/point)
    par(ann=FALSE, bg="transparent", mai=c(0.02,0.02,0.02,0.02))
    plot.new()
    par(usr=c(0,TW*w,0,TH*dy), family="sans", adj=1)

    rect(0,0,TW*w,TH*dy, col=col3)
    rect(0,y-5*h,15*w,y, col=col1)
    text(7.5*w, y-2.5*h,"Table 14", adj=c(0.5, 0.5), col="white", font=2, cex=1.2)
    rect(15*w,y-5*h,TW*w,y, col=col2)
    text(16*w,y-1.5*h,"Critical values for Spearman's", adj=c(0,0.5), cex=1.2)
	text(16*w,y-3.5*h,"Rank Correlation Coefficient", adj=c(0,0.5), cex=1.2)
	y = y - 3.6*dy
	lines(c(8*w, 8*w), c(0, TH*dy-5*h),lwd=1)
	text(29*w, y, expression(paste("two-tailed ", alpha)), adj=0.5)
	y = y - 0.5*dy; lines(c(8*w,TW*w),c(y,y), lwd=1); y = y - 0.6*dy
	text(5*w + 8*w*(1:5), y, c("0.20","0.10","0.05","0.02","0.01"), font=2, adj=0.5)
	text(5*w, y, "n", font=2)
	y = y - 0.6*dy; lines(c(0,TW*w),c(y,y), lwd=1); y = y + 0.4*dy
	alpha =  c(0.2,0.1,0.05,0.02,0.01)
	for(k in 5:30){
		y = y - dy
		text(5*w, y, k, adj=1)
		line =  formatC(qSpearman(1 - alpha/2, k), format="f", digits=3, flag="#")
		text(5*w + 8*w*(1:5), y, line, adj=0.5)
		
	}
    dev.off()	
}

if(TRUE){
table.1()
table.2A()
table.2B()
table.3() 
table.4A()
table.4B()
table.5()
table.6()
table.7A()
table.7B()
table.8()
table.9()
table.10A()
table.10B()
table.10C()
table.10D()
table.11()
table.12()
table.13()
table.14()
}
