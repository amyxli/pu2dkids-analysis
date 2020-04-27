rotate_space = function(data, xangle, yangle, zangle) {
	#browser()
	xmat = cbind(c(1, 0, 0), c(0, cos(xangle), sin(xangle)), c(0, -sin(xangle), cos(xangle)))
	ymat = cbind(c(cos(yangle), 0, -sin(yangle)), c(0, 1, 0), c(sin(yangle), 0, cos(yangle)))
	zmat = cbind(c(cos(zangle), sin(zangle), 0), c(-sin(zangle), cos(zangle), 0), c(0, 0, 1))
	data = data%*%xmat
	data = data%*%ymat
	data = data%*%zmat
	return(data)
}

diff.corr <- function( r1, n1, r2, n2 ){ 

    Z1 <- 0.5 * log( (1+r1)/(1-r1) ) 
    Z2 <- 0.5 * log( (1+r2)/(1-r2) ) 

    diff   <- Z1 - Z2 
    SEdiff <- sqrt( 1/(n1 - 3) + 1/(n2 - 3) ) 
    diff.Z  <- diff/SEdiff 

    p <- 2*pnorm( abs(diff.Z), lower=F) 
    return(p)
  } 

count <- function(x) { 
  length(na.omit(x)) 
} 


make_inds = function(k, y, se, mar=0.5, marT=0.5, lwd = 1, sl = 2, cex = 1, col = 1, offset = 0, horiz = FALSE, upside = 0) {
  #browser()
  k = as.matrix(k)
  y = as.matrix(y)
  se = as.matrix(se)
  ncomparisons = dim(k)[which(dim(k) != 2)[1]]
  upsidedown = upside*-2 + 1

  
  if (sl==0) ctext = 'n.s.'
  if (sl==1) ctext = '+'  
  if (sl==2) ctext = '*'  
  if (sl==3) ctext = '**'
  if (sl==4) ctext = '***'    

  if (!horiz) {
    for (i in 1:ncomparisons) {
      if (!upside) {
        mi = which(y[,i]==max(y[,i]))[1]} 
        else {mi = which(y[,i]==min(y[,i]))[1]}
      nmi = setdiff(c(1, 2), mi)
      cy = y[mi, i] + (se[mi, i] + mar*se[mi, i])*upsidedown
      segments(k[1, i] + offset, cy, k[2, i] - offset, cy, lwd = lwd, col = col)
      segments(k[1, i] + offset, cy, k[1, i] + offset, y[1, i] + (se[1, i] + 0.5*se[1, i])*upsidedown, lwd = lwd, col = col)
      segments(k[2 ,i] - offset, cy, k[2, i] - offset, y[2, i] + (se[2, i] + 0.5*se[2, i])*upsidedown, lwd = lwd, col = col)
      text(mean(k[,i]), y=cy+(marT*se[mi, i])*upsidedown, labels=ctext, cex = cex, col = col)
    }
  } else {
    for (i in 1:ncomparisons) {
      if (!upside) {
        mi = which(y[,i]==max(y[,i]))[1]} 
        else {mi = which(y[,i]==min(y[,i]))[1]}
      nmi = setdiff(c(1, 2), mi)
      cy = y[mi, i] + (se[mi, i] + mar*se[mi, i])*upsidedown
      segments(cy, k[1, i] + offset, cy, k[2, i] - offset, lwd = lwd, col = col)
      segments(cy, k[1, i] + offset,  y[1, i] + (se[1, i] + 0.5*mar*se[1, i])*upsidedown, k[1, i] + offset, lwd = lwd, col = col)
      segments(cy, k[2, i] - offset, y[2, i] + (se[2, i] + 0.5*mar*se[2, i])*upsidedown, k[2 ,i] - offset, lwd = lwd, col = col)
      text(cy+(marT*se[mi, i])*upsidedown, y=mean(k[,i]), labels=ctext, cex = cex, col = col)
    }

  }
}


# clear screen 
cls<-function () {for (i in 1:80) {cat('\n')}}

	
hypergeometric<-function(k, n, N, M, order=T) {
	## calculates the probabilities for urn experiments WITHOUT replacement
	## when order == T, order is assumed
	## N=size of population 
	## M=number of targets in urn
	## k=number of targets drawn
	## n=size of sample
	if (order==F) {
		p<-((choose(M, k)*choose((N-M), (n-k)))/(choose(N, n)))
		} else {
		p<-((choose(M, k)*choose((N-M), (n-k)))/(factorial(N)/factorial(N-n)))			
		}
		return(p)
	}
	
se_bars<-function(x, y, se, col=1, wf=0, bi=TRUE, lwd = 2, horiz = FALSE) {
	# adds se bars to data x
	n=length(x)

	w=0.02*max(x)*wf
	y = as.numeric(y)
	se = as.numeric(se)
  col = rep(col, ceiling(n/length(col)))
  if (horiz == FALSE) {
	  for (i in 1:n) {
		  segments(x[i]+w, y[i]+se[i], x[i]-w, y[i]+se[i], col=col[i], lwd = lwd)
      if (bi==TRUE) {
    	   segments(x[i], y[i]-se[i], x[i], y[i]+se[i], col=col[i], lwd = lwd)
    	   segments(x[i]+w, y[i]-se[i], x[i]-w, y[i]-se[i], col=col[i], lwd = lwd)
    	 }	else {
    		 segments(x[i], y[i], x[i], y[i]+se[i], col=col[i], lwd = lwd)
    	 }
		  }
    } else {
    for (i in 1:n) {
      segments(x[i]+se[i], y[i]+w, x[i]+se[i], y[i]+w, col=col[i], lwd = lwd)
      if (bi==TRUE) {
         segments(x[i]-se[i], y[i], x[i]+se[i], y[i], col=col[i], lwd = lwd)
         segments(x[i]-se[i], y[i]+w, x[i]-se[i], y[i]-w, col=col[i], lwd = lwd)
        }  else {
         segments(x[i], y[i], x[i]+se[i], y[i], col=col[i], lwd = lwd)
         }

      }
    }
  }	

se_shadows<-function(x, y, se, ccol='#66666666', border = NA) {
	# adds se shadows to data x
	polygon(c(x, rev(x)), c(y + se, rev(y - se)), col = ccol, border = border)
}

std.error <- function(X, na.rm = TRUE, within = FALSE ) {
	
  if (within == FALSE) {
    sd(X, na.rm = TRUE)/sqrt(count(X))
  } else {
    # assumes 
    Y = X - rowMeans(X) + mean(X) 
    sd(X, na.rm = TRUE)/sqrt(count(X))
  }
}
	
runmean<-function(data, length) {
	n=length(data-length)
	y=rep(NA, n)
	for (i in 1:n) {
		end<-i+length
		y[i]<-mean(data[i:end])
		} 
		return(y)
	}
	
	#WSCI_transform<-function(x) {
#	#removes between-subject variablitx from data-set, so that normal se-calculation can be applied to within-subject data (see Cousineau, 2005)
#	if (length(dim(x))>2) {
#		f2<-dim(x)[3]
#		f1<-dim(x)[2]
#		nid<-dim(x)[1]
#		y<-x
#		for (i in 1:f2) {
#			tab<-x[,,i]
#			m<-mean(tab)
#			for (j in 1:nid){
#				m_id<-mean(tab[j,])
#				for (k in 1:f1) {
#					tab[j, k]<-tab[j, k]-m_id+m
#					}
#				}
#			y[,,i]<-tab
#			}
#		} else {
#			f1<-dim(x)[2]
#			nid<-dim(x)[1]
#			y<-x
#			m<-mean(y)			# grand mean 
#		    for (j in 1:nid){
#				m_id<-mean(y[j,]) # subject mean 
#				for (k in 1:f1) {
#					y[j, k]<-y[j, k]-m_id+m
#					}
#				}
#			} 
#		return(y)
#	}
#
#WSV<-function(x) {
#	#removes between-subject variablity from data-set
#	nid<-dim(x)[1]
#	f<-dim(x)[2]
#	fm<-colMeans(x)
#	rm<-rowMeans(x)
#	m<-mean(x)
#	fe<-fm-m
#	re<-rm-m
#	y<-xe<-matrix(ncol=f, nrow=nid)
#	for (i in 1: nid) {
#		for (j in 1:f) {
#			y[i, j]<-x[i, j]-rm[i]
#			#xe[i, j]<-x[i, j]+m-fm[j]-rm[i]
#			}
#		}
#	suppressWarnings(return(y, fe))
#	}


LCS2 = function(s1, s2) {
  # LONGEST common substring v2 
  # function that detects common substrings (subsequences) of min length 3
  # based on LCS algrithms form the DNA sequencing literature 
  # written by NWS, 2012
  # improved Jan 2013
  
  # s1 is the ORIGINAL sequence agianst which it is compared 
  # s1 should already be fully wrapped around itself, e.g. s1[c(1:len1, 1:(len1-1))]
  # s2 is the COMPARING sequence (sequence which is compared to s1). it should be onyl wrapped around by 2 such that the trailing triplett can also be detected 
  
  # initilize and get some vars 
  len1 = length(s1)
  len2 = length(s2)  
  clcs = seqct = 0
  idxs = matrix(0, len1, len2)
  cseqs1 = cseqs2 = matrix(0, 100, 2) # empty matrix for start/stop positions of running matches (maximum of 100 matches) 
  LCS = NULL	
  LCS$idxtab = idxs
  LCS$maxlen = 0
  LCS$runs = 0
  LCS$correcttripletts = 0 
  	
  # cicle through sequences and note running co-occurences 
  for (i1 in 1:len1) {
  	for (i2 in 1:len2) {
  		if (s1[i1] == s2[i2]) {
  			if (i1==1 | i2 ==1) {
  				idxs[i1, i2] = 1
  				} else {
  				idxs[i1, i2] = idxs[i1-1, i2-1] +1 # if there's a match, count up from previous match-count 
  				# if we are not already at the thrid or higher match in a row, note that we have a tripplet and where it is 			
  				if ((idxs[i1, i2]) > 2) { 
  					seqct = seqct + 1 
  					cseqs2[seqct, 1:2] = c(i2 - idxs[i1, i2] + 1, i2)	# indexes with s2
  					cseqs1[seqct, 1:2] = c(i1 - idxs[i1, i2] + 1, i1)	# indexes with s2
  					}
  				}
  			}
  		}
  	}
  	cseqs1[which(cseqs1[,1]>12),] = 0
  	runs = 0
  	# idxs: now, we have a big index matrix IDXS of co-occurences and their running count 
  	# seqc: number of the co-occurences that have length >2
  	# cseqs: where do the co-occurences lie within the comparing sequence 
    if  (max(cseqs1[,2] - cseqs1[,1], na.rm = TRUE) <12) { # how long are the matches ? 
  		if (seqct > 0) { # any matches at all? 
  			runslens = cseqs1[1:seqct,2] - cseqs1[1:seqct,1] # how long are the matches ? 
	  		sstop = FALSE 
  			runsct = 0
  			runs = NULL
			while (!sstop) {
				seqlist = which(runslens>0)
				ctarg = which.max(runslens)	# take the longest (remaining, see below) match 
				ctt = cseqs1[ctarg,1]:cseqs1[ctarg,2] # index of position of within COMPARING sequence   
				runs = c(runs, min(c(cseqs1[ctarg,2] - cseqs1[ctarg,1] + 1, 12))) # note how long this is   
				for (i in setdiff(seqlist, ctarg)) { # check all other sequences in the loop if they are the same # (don't compare against itself )		
					ca = cseqs1[i,1]:cseqs1[i,2] # 
					if (length(ca)<3) {next}
					if (mean(ca%in%ctt)==1) { # if another sequence is the same, remove it so we don't count double  
						cseqs1[i,] = 0
						runslens[i] = 0
						next
						} else if (sum(ca%in%ctt)>2) {
						tmp = (min(which(ca%in%ctt == FALSE)) == 1)
						if (tmp) { 
							cseqs1[i,2] = cseqs1[i,2] - sum(ca%in%ctt) + 2
						} else {
							cseqs1[i,1] = cseqs1[i,1] + sum(ca%in%ctt) - 2
						}			
						runslens[i] = runslens[i] - sum(ca%in%ctt) + 2
						next
						}	
					ca = ca - 12
					if (mean(ca%in%ctt)==1) { # if another sequence is the same, remove it so we don't count double  
						cseqs1[i,] = 0
						runslens[i] = 0
						next
						} else if (sum(ca%in%ctt)>2) {
						tmp = (min(which(ca%in%ctt == FALSE)) == 1)
						if (tmp) { 
							cseqs1[i,2] = cseqs1[i,2] - sum(ca%in%ctt) + 2
						} else {
							cseqs1[i,1] = cseqs1[i,1] + sum(ca%in%ctt) - 2
						}			
						runslens[i] = runslens[i] - sum(ca%in%ctt) + 2
						next
						}	
					ca = ca + 24
					if (mean(ca%in%ctt)==1) { # if another sequence is the same, remove it so we don't count double  
						cseqs1[i,] = 0
						runslens[i] = 0
						next
						} else if (sum(ca%in%ctt)>2) {
						tmp = (min(which(ca%in%ctt == FALSE)) == 1)
						if (tmp) { cseqs1[i,2] = cseqs1[i,2] - sum(ca%in%ctt) + 2
						} else {
						cseqs1[i,1] = cseqs1[i,1] + sum(ca%in%ctt) - 2
						}			
						runslens[i] = runslens[i] - sum(ca%in%ctt) + 2
						next
						}	
					} # end for loop 
				runslens[ctarg] = 0 # OK, finished with this match. The match was stored in runs (length of it), and all duplicates were remove. Now, it can be removed form the list of matches and the loop continues to check remaining matches  
				if (sum(runslens) <1) {sstop = TRUE} # until there are no more matches 
				} 	# end while loop 
  			} # end if (seqct > 0)
  		} else { # end if max .. >12
  		runs = 12
  		}

  # he, finished! store everything we need in output structure and return it	
  LCS = NULL	
  LCS$idxtab = idxs
  LCS$maxlen = max(runs)
  LCS$runs = runs
  if (runs == 0) { 
  	LCS$correcttripletts = 0
  } else {
	  LCS$correcttripletts = length(LCS$runs) + sum(LCS$runs - 3)
	  }
  #if (max(runs)==12) {LCS$correcttripletts = 12} 
  return(LCS)
}


# correlation plot
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor.test(x, y)$estimate
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    #if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    if(missing(cex.cor)) cex <- 3.8
    
    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
    
    text(0.5, 0.5, txt, cex = cex * abs(r))
    text(.8, .8, Signif, cex=cex, col=2)
}

panel.lin<- function(x, y)
     {
         points(x,y, pch=1, col = "black",cex=1)
         fit = lm(y~x)
         abline(fit,lwd=1.4, col="blue", lty = 2)
     }
     

# ----- Define a function for plotting a matrix ----- #
MatrixPlot <- function(x, ...){
     min <- min(x)
     max <- max(x)
     yLabels <- rownames(x)
     xLabels <- colnames(x)
     title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
       min <- Lst$zlim[1]
       max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
       yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
       xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
       title <- Lst$title
    }
  }
# check for null values
if( is.null(xLabels) ){
   xLabels <- c(1:ncol(x))
}
if( is.null(yLabels) ){
   yLabels <- c(1:nrow(x))
}

layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))

 # Red and green range from 0 to 1 while Blue ranges from 1 to 0
 ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
 ColorLevels <- seq(min, max, length=length(ColorRamp))

 # Reverse Y axis
 reverse <- nrow(x) : 1
 yLabels <- yLabels[reverse]
 x <- x[reverse,]

 # Data Map
 par(mar = c(3,5,2.5,2))
 image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
 ylab="", axes=FALSE, zlim=c(min,max))
 if( !is.null(title) ){
    title(main=title)
 }
axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
 axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
 cex.axis=0.7)

 # Color Scale
 par(mar = c(3,2.5,2.5,2))
 image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
      col=ColorRamp,
      xlab="",ylab="",
      xaxt="n")

 layout(1)
}
# ----- END plot function ----- #

