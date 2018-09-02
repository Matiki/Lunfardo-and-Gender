quantFun = function(data, groups, quantN){
  ## quantiles
  quant = c()
  for(i in 1:quantN){
    quant[i] = i/quantN
  }
  quant = c(0, quant)
  ## how many grouping variables -- take indices
  group_ind = c()
  for(i in 1:length(groups)){
    group_ind[i] = which(names(data)==groups[i])
  }
  ## For only 1 group
  if(length(groups) == 1){
    for(i in unique(data[group_ind][[1]])){
      dat = data[data[group_ind]==i, "Response.RT"]
      bined_dat = cut(dat, quantile(dat, probs = quant), labels=c(1:(quantN)), include.lowest = T)
      data[data[group_ind]==i, "binRT"] = factor(bined_dat)
    }
  }
  ## For 2 groups
  if(length(groups) == 2){
    for(k in unique(data[group_ind[1]])[[1]]){
      for(i in unique(data[group_ind[2]])[[1]]){
        dat = data[data[group_ind[1]]==k & data[group_ind[2]]==i, "Response.RT"]
        bined_dat = cut(dat, quantile(dat, probs = quant), labels=c(1:(quantN)), include.lowest = T)
        data[data[group_ind[1]]==k & data[group_ind[2]]==i, "binRT"] = factor(bined_dat)
      }
    }
  }
  
  ## For 3 groups
  if(length(groups) == 3){
    for(k in unique(data[group_ind[1]])[[1]]){
      for(i in unique(data[group_ind[2]])[[1]]){
        for(o in unique(data[group_ind[3]])[[1]]){
          dat = data[data[group_ind[1]]==k & data[group_ind[2]]==i & data[group_ind[3]]==o, "Response.RT"]
          bined_dat = cut(dat, quantile(dat, probs = quant), labels=c(1:(quantN)), include.lowest = T)
          data[data[group_ind[1]]==k & data[group_ind[2]]==i & data[group_ind[3]]==o, "binRT"] = factor(bined_dat)
        }
      }
    }
  }
  
  
  ## For 4 groups
  if(length(groups) == 4){
    for(k in unique(data[group_ind[1]])[[1]]){
      for(i in unique(data[group_ind[2]])[[1]]){
        for(o in unique(data[group_ind[3]])[[1]]){
          for(p in unique(data[group_ind[4]])[[1]]){
            dat = data[data[group_ind[1]]==k & data[group_ind[2]]==i & data[group_ind[3]]==o & data[group_ind[4]]==p, "Response.RT"]
            bined_dat = cut(dat, quantile(dat, probs = quant), labels=c(1:(quantN)), include.lowest = T)
            data[data[group_ind[1]]==k & data[group_ind[2]]==i & data[group_ind[3]]==o & data[group_ind[4]]==p, "binRT"] = factor(bined_dat)
          }
        }
      }
    }
  }
  
  
  ## For 5 groups
  if(length(groups) == 5){
    for(k in unique(data[group_ind[1]])[[1]]){
      for(i in unique(data[group_ind[2]])[[1]]){
        for(o in unique(data[group_ind[3]])[[1]]){
          for(p in unique(data[group_ind[4]])[[1]]){
            for(d in unique(data[group_ind[5]])[[1]]){
              dat = data[data[group_ind[1]]==k & data[group_ind[2]]==i & data[group_ind[3]]==o & data[group_ind[4]]==p & data[group_ind[5]]==d, "Response.RT"]
              bined_dat = cut(dat, quantile(dat, probs = quant), labels=c(1:(quantN)), include.lowest = T)
              data[data[group_ind[1]]==k & data[group_ind[2]]==i & data[group_ind[3]]==o & data[group_ind[4]]==p & data[group_ind[5]]==d, "binRT"] = factor(bined_dat)
            }
          }
        }
      }
    }
    
    
  }
  data$Subject = factor(data$Subject)
  data$binRT = factor(data$binRT)
  return(data[,c(groups, "Response.RT","binRT")])
}

fot.fun <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE, id=NULL) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  if(is.null(id)){
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                     c(N    = length2(xx[[col]], na.rm=na.rm),
                       mean = mean   (xx[[col]], na.rm=na.rm),
                       median = median (xx[[col]], na.rm=na.rm),
                       sum = sum (xx[[col]], na.rm=na.rm),
                       sd   = sd     (xx[[col]], na.rm=na.rm)
                     )
                   },
                   measurevar
    )
    
  } else {
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                     c(id = id,
                       N    = length2(xx[[col]], na.rm=na.rm),
                       mean = mean   (xx[[col]], na.rm=na.rm),
                       median = median (xx[[col]], na.rm=na.rm),
                       sum = sum (xx[[col]], na.rm=na.rm),
                       sd   = sd     (xx[[col]], na.rm=na.rm)
                     )
                   },
                   measurevar
    )
    datac<-datac[,c(3,1,2,4,5,6,7,8)]
  }
  
  # Rename the "mean" column    
  datac <- reshape:::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}




outliersALL <- function(data) {
  extr_upp<<-norm_upp<<-extr_low<<-norm_low<<-NULL
  data1 <- data
  data <- data[!is.na(data)]
  OU<-data[data>quantile(data, na.rm = T)[4]+IQR(data, na.rm = T)*3]
  OL<-data[data<quantile(data, na.rm = T)[2]-IQR(data, na.rm = T)*3]
  IU<-data[data>quantile(data, na.rm = T)[4]+IQR(data, na.rm = T)*1.5 & 
             data<quantile(data, na.rm = T)[4]+IQR(data, na.rm = T)*3]
  IL<-data[data<quantile(data, na.rm = T)[2]-IQR(data, na.rm = T)*1.5 & 
             data>quantile(data, na.rm = T)[2]-IQR(data, na.rm = T)*3]
  x<-list(" EXTREME_UPPER"=OU,  " NORMAL_UPPER"=IU," NORMAL_LOWER"=IL, " EXTREME_LOWER"=OL)
  
  extr_upp <<- data1>quantile(data1, na.rm = T)[4]+IQR(data1, na.rm = T)*3 & !is.na(data1)
  norm_upp <<- data1>quantile(data1, na.rm = T)[4]+IQR(data1, na.rm = T)*1.5 & 
    data1<quantile(data1, na.rm = T)[4]+IQR(data1, na.rm = T)*3 & !is.na(data1)
  extr_low <<-data1<quantile(data1, na.rm = T)[2]-IQR(data1, na.rm = T)*3 & !is.na(data1)
  norm_low <<-data1<quantile(data1, na.rm = T)[2]-IQR(data1, na.rm = T)*1.5 & 
    data1>quantile(data1, na.rm = T)[2]-IQR(data1, na.rm = T)*3 & !is.na(data1)
  return(x)
}

#---Explanation---
#I used the interqurtile ranges (IQRs) to detect outliers 
#(Q1-/+ 1.5*IQR  for the inner-fence outliers)
#(Q1-/+ 3*IQR for the outter-fence outliers).
#I prefered this method instead of using 
#standard deviations (any value more than 2 SDs distance from the mean)
#because it's affected less by the extreme values 
#and it's more accurate (Field, 2012)


##TEST FOR CORRELATIONS function (gives me the estimates and p values for all possible combinations between my variables)
corFUN <- function(x, ...){
  nco <- ncol(x)
  res <- matrix(nrow = nco, ncol = nco, dimnames = list(names(x), names(x)))
  for(i in 1:nco){
    for(k in 1:nco){
      if(i==k) i
      else{
        corre <-  cor.test(x[,i], x[,k],...)
        p <- corre$p.value
        #statistic <- corre$statistic
        esti <- corre$estimate
        res[k, i] <- round(p, 7)
        res[i, k] <- round(esti, 7)
      }
    }
  }
  return(res)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#A Simple-Coding function
###A Function for simple-coding categorical variables quick and dirty: 
simple.coding<-function(k, data){
  c<-contr.treatment(k)
  my.coding<-matrix(rep(1/k, k*(k-1)), ncol=k-1)
  my.simple<-c-my.coding
  assign(paste(substitute(contrastsv)), data)
  contrasts(contrastsv)<-my.simple
  return(contrastsv)
}


