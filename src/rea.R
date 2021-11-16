#####################################
## Functions for calculating pest eradication
## Dean Anderson
## Landcare Research
## 14 March 2011
##

## Modified by:
## Jimmy Oh
## University of Auckland
## 21 May 2015

## Added codings for two types of static devices, incursion and mobile tracks
## Juliana Kim
## University of Auckland
## 2017

## Fixed some bugs
## Juliana Kim
## University of Auckland
## 2020
######################################
library(mc2d)

distmatsq <- function(x1, y1, x2, y2){
  xd <- outer(x1, x2, function(x1,x2) (x1 - x2)^2)
  yd <- outer(y1, y2, function(y1,y2) (y1 - y2)^2)
  t(xd + yd)
}

civFX <- function(x, threshold)
  mean(x >= threshold)
 
combineFX <- function(x, nights)
  1-prod((1 - x)^nights) 

#calculating probabilities of detecting rats by mobile device (e.g. dog)
detectByMobile <- function(xRat, yRat, mobiletrack, sigma){
  probs <- vector()
  for(i in 1:length(xRat)){
    sp = SpatialPoints(cbind(xRat[i], yRat[i]))
    dist = min(sapply(mobiletrack, function(x) gDistance(sp, x)))
    probs[i] <- ifelse(dist <= 2*sigma, 1, 0)
  }
  probs
}

#making spatial polygon object for a circle
makePoly <- function(xpos, ypos, edges, radius){
  
  x <- radius * cos(seq(0, 2 * pi, length = edges)) + xpos
  y <- radius * sin(seq(0, 2 * pi, length = edges)) + ypos
  circle <- cbind(x, y)
  
  p <- Polygon(circle)
  ps <- Polygons(list(p), 1)
  sps <- SpatialPolygons(list(ps))
}

#getting the area of device influence on rats
calcArea <- function(device, sigma){
  
  cor = do.call(rbind, device)
  radius = sigma * 2
  edges <- 50
  polys <- mapply(makePoly, cor[,1], cor[,2], edges, radius)
  
  l= length(polys)
  while(l != 1){
    
    index = (1:l) %% 2 == 0
    
    if(l %% 2 == 0){
      polys = mapply(gUnion, polys[index], polys[!index])
    }else{
      b = polys[l]
      polys = mapply(gUnion, polys[index], polys[!index][1:sum(index)])
      polys[length(polys)][[1]]= gUnion(polys[length(polys)][[1]], b[[1]])
    }
    l= length(polys)
  }
  polys[[1]] 
}

#getting all coverage of devices
coverage <- function(static, mobile, tmpshape, sigma){
  #for static device
  if(!is.null(static)){
    staticDev <- calcArea(static, sigma)
    s <- gIntersection(tmpshape, staticDev)
    staticP = gArea(s)/gArea(tmpshape)
  
    if(!is.null(mobile)){
      mobileCor= lapply(mobile, function(x) x@lines[[1]]@Lines[[1]]@coords)
      mobileDev<-calcArea(mobileCor, sigma) 
    
      #for mobile devices
      m<- gIntersection(tmpshape, mobileDev)
      mobileP = gArea(m)/gArea(tmpshape)
    
      #for both devices
      b<- gUnion(mobileDev, staticDev)
      b<- gIntersection(tmpshape, b)
      bothP = gArea(b)/gArea(tmpshape)
    
      result = matrix(0, ncol=3, nrow=1)
      result[1,] = c(staticP,mobileP, bothP)
      colnames(result) = c("static", "mobile", "both")
    
    }else{
      result = matrix(0, ncol=1, nrow=1)
      result[1,] = staticP
      colnames(result) = c("static")
    }
  }else{
    if(!is.null(mobile)){
      mobileCor= lapply(mobile, function(x) x@lines[[1]]@Lines[[1]]@coords)
      mobileDev<-calcArea(mobileCor, sigma) 
      
      #for mobile devices
      m<- gIntersection(tmpshape, mobileDev)
      mobileP = gArea(m)/gArea(tmpshape)
      
      result = matrix(0, ncol=1, nrow=1)
      result[1,] = mobileP
      colnames(result) = c("mobile")
    }  
  } 
  rownames(result) = "coverage"
  result
}

#getting the area of possible existence of rats from the center of incursion
findAreaByIncur <- function(cent, radius, tmpshape){
  edges <- 200; xpos <- cent[1]; ypos <- cent[2]
  sps <- makePoly(xpos, ypos, edges, radius)
  incurarea <- gIntersection(tmpshape, sps, unaryUnion_if_byid_false=FALSE)
}

POFx <- function(alltags, mobiletrack, incursion, tmpshape, shape, g0param, parameters, sessiondat, type = "fast"){
  
  #The number of incursion rats. Only one at the moment.
  nIncurRats = 0
  if(!is.null(incursion)) { 
    nIncurRats = dim(incursion)[1]
  }
  
  nSession <- dim(sessiondat)[2] - 1
  nRats <- sessiondat[3, 2]
  
  #two tyeps of static devices
  if (nrow(g0param) > 1)
    g0Distr <- cbind(rpert(nRats, g0param[1,2], g0param[1,3], g0param[1,4]),
                     rpert(nRats, g0param[2,2], g0param[2,3], g0param[2,4]))
  else
    #single type
    g0Distr <- as.matrix(rpert(nRats, g0param[1,2], g0param[1,3], g0param[1,4]), ncol = 1)
  
  sigDistr <- rpert(nRats, parameters[1,2], parameters[1,3], parameters[1,4])
  
  Pi <- matrix(0, nRats, nSession)
  posterior  <- matrix(0, nRats, nSession)
  
  #no incursion (surveillance): sampling the location of rats in whole island
  if(nIncurRats == 0) {
    sampAdultLocs <- as.data.frame(spsample(tmpshape, nRats, "random"))
  #incursion: sampling the location of rats in incursion area 
    }else{
    incurRange <- findAreaByIncur(as.numeric(incursion), parameters[6,3] * 4, tmpshape)
    sampAdultLocs <- as.data.frame(spsample(incurRange, nRats, "random"))
  }
  
  sampSimLocs = NULL
  
  TotalBabiesPlaced = 0
  TotalBabiesNotPlaced = 0
  
  loopsim = function(adultLocDF, currat, curses, xtag, ytag, type){
    nAdults <- dim(adultLocDF)[1]
    
    babyRats = NULL
    babyRats <- round(rpert(nAdults, parameters[4, 2], parameters[4, 3], parameters[4, 4]) * sessiondat[5, (curses + 1)])
    
    Pij = list()
    Pmobile = list()
    
    for(k in 1:nAdults){
      if(babyRats[k] > 0){
        if(type == "fast"){
          ## Approximate fast sampling method
          ## Premade points
          if(is.null(sampSimLocs)){
            cat("Making sampSimLocs\n")
            timea = proc.time()
            
            sampSimLocs <<- as.data.frame(spsample(tmpshape, nRats * 10, "stratified"))
           
            cat("Done, took", (proc.time() - timea)[3], "seconds\n")
          }
         
          ## Get points within sdm of location
          if(nIncurRats == 0) 
            sdm = abs(rnorm(babyRats[k], 0, parameters[5, 3])) #surveillance
          else
            sdm = abs(rnorm(babyRats[k], 0, parameters[6, 3])) #incursion
         
          iscloseMax = max(sdm) >= sqrt(
              (sampSimLocs[,1] - adultLocDF[k, 1])^2 +
              (sampSimLocs[,2] - adultLocDF[k, 2])^2)
            closeLocsMax = sampSimLocs[iscloseMax,]
         
          dispXYList = list()
         
          for(i in 1:babyRats[k]){
         
            isclose = sdm[i] >= sqrt(
              (closeLocsMax[,1] - adultLocDF[k, 1])^2 +
              (closeLocsMax[,2] - adultLocDF[k, 2])^2
            )
            closeLocs = closeLocsMax[isclose,]
            if(nrow(closeLocs) > 1){
              dispXYList[[i]] = closeLocs[sample(1:nrow(closeLocs), 1),]
              TotalBabiesPlaced <<- TotalBabiesPlaced + 1
            } else{
              # warning(paste("No room found to place baby rat",
                            # "current sdm:", sdm[i],
                            # "max sdm:", max(sdm)))
              TotalBabiesNotPlaced <<- TotalBabiesNotPlaced + 1
            }
          }
          dispXY1 = do.call(rbind, dispXYList)
        } else{
          ## Original sampling method
          dx <- rnorm(5000, 0, parameters[5, 3])
          dy <- rnorm(5000, 0, parameters[5, 3])
          dispXY <- as.data.frame(cbind((adultLocDF[k, 1] + dx),(adultLocDF[k, 2] + dy)))
          inShape <- inside.owin(dispXY[,1], dispXY[,2], shape)
          dispXY1 <- dispXY[inShape,][1:babyRats[k],]
          ## Just in case, checking if current method of generating 5000 is enough
          inn = length(dispXY[inShape,])
          if(inn < babyRats[k])
            cat("Ran out of inShape\n",
                "Have:", inn, "     Need:", babyRats[k], "\n")
          ## End Just in case
        }
        ratX <- c(adultLocDF[k, 1], dispXY1[,1])
        ratY <- c(adultLocDF[k, 2], dispXY1[,2])
       
      }else{
        ratX <- adultLocDF[k, 1]
        ratY <- adultLocDF[k, 2]
      }
      #for multiple traps
      if(length(xtag) != 0){
        pb <- list()
        for(i in 1:length(xtag)){
          tagRatDistsq <- distmatsq(xtag[[i]], ytag[[i]], ratX, ratY) 
          pb[[i]] <- as.vector(g0Distr[currat,i] * exp(-0.5 * tagRatDistsq/(sigDistr[currat]^2)))
        }
        Pij[[k]] <- unlist(pb)
      }
      
      #for mobile track
      if(!is.null(mobiletrack))
         Pmobile[[k]] <- detectByMobile(ratX, ratY, mobiletrack, sigDistr[currat])
      else
         Pmobile[[k]] <- 0
    
    }
    
    # 26/04/2020 The values getting from functions were not saved. This was fixed by changing arrows to equal signs.
    list(
      #multiple static devices + mobile track 
      combFX = combineFX(c(unlist(Pij), unlist(Pmobile)), nights = sessiondat[2, (curses + 1)]),
      adultLocDF = as.data.frame(cbind(ratX, ratY))
    )

  }
  
  for(curses in 1:nSession){
    for(currat in 1:nRats){
      
      xtag <- list()
      ytag <- list()
      
      if(sessiondat[1, (curses + 1)] == 0){
       
        if(!is.null(alltags)){
          for(i in 1:length(alltags)){
             xtag[[i]] <- alltags[[i]][,1]
             ytag[[i]] <- alltags[[i]][,2]
          }
        }
       
      } else{
        s_points <- as.data.frame(spsample(tmpshape, n=1, "regular", cellsize = sessiondat[1, (curses + 1)]))
        # 18/06/2020. changed xtag and ytag to a list (This works for not uploading the locations of tags)
        xtag[[1]] <- s_points[,1]
        ytag[[1]] <- s_points[,2]
         
      }
      
      priorDistr <- rpert(1, parameters[2, 2], parameters[2, 3], parameters[2, 4])
      if(curses == 1){
        adultLocDF = sampAdultLocs[currat,]
       
        if(sessiondat[5, 2] == 0){
          ## session 1 with immediate survey
         
          ratX <- adultLocDF[,1]
          ratY <- adultLocDF[,2]
          
          Pij <- numeric()
          Pmobile <- numeric()
          
          if(length(xtag) !=0){
            pb <- list()
            for(i in 1:length(xtag)){
              tagRatDistsq <- distmatsq(xtag[[i]], ytag[[i]], ratX, ratY) 
              pb[[i]] <- g0Distr[currat,i] * exp(-0.5 * tagRatDistsq/(sigDistr[currat]^2))
            }
            Pij <- unlist(pb)
          }
          
          if(!is.null(mobiletrack))
            Pmobile <- detectByMobile(ratX, ratY, mobiletrack, sigDistr[currat])
          else
            Pmobile <- 0
          
          Pi[currat, curses] <- combineFX(c(Pij, Pmobile), nights = sessiondat[2, (curses + 1)])
          #J
          adultLocDF <- as.data.frame(cbind(ratX, ratY))
        } else{
          ## repro from 1 female
          outsim = loopsim(adultLocDF, currat, curses, xtag, ytag, type)
          Pi[currat, curses] = outsim$combFX
          adultLocDF = outsim$adultLocDF
        }
        
        posterior[currat, curses] <- priorDistr/(1 - (Pi[currat, curses] * (1 - priorDistr)))
        pIntro <- rpert(1, parameters[3,2], parameters[3,3], parameters[3,4])
        priorDistr <- 1 - ((1 - posterior[currat, curses]) + pIntro - (1 - posterior[currat, curses]) * pIntro)
      } else{
        outsim = loopsim(adultLocDF, currat, curses, xtag, ytag, type)
        Pi[currat, curses] = outsim$combFX
        adultLocDF = outsim$adultLocDF
       
        posterior[currat, curses] <- priorDistr/(1 - (Pi[currat, curses] * (1 - priorDistr)))
        pIntro <- rpert(1, parameters[3,2], parameters[3,3], parameters[3,4])
        priorDistr <- 1 - ((1 - posterior[currat, curses]) + pIntro - (1 - posterior[currat, curses]) * pIntro)
      }
    }
  }
  cat("TotalBabiesPlaced:   ", TotalBabiesPlaced, "\n")
  cat("TotalBabiesNotPlaced:", TotalBabiesNotPlaced, "\n")
  
  outPred <- matrix(0, nSession, 8)
  colnames(outPred) <- c("Session", "Sens 2.5Q", "Sens 50.0Q", "Sens 97.5Q",
                         "Post. 2.5Q", "Post.50.0Q", "Post. 97.5Q", "CIV")
  outPred[,1] <- 1:nSession
  outPred[,2:8] <- c(
    t(apply(Pi, 2, quantile, c(.025, .5, .975))),
    t(apply(posterior, 2, quantile, c(.025, .5, .975))),
    t(apply((posterior), 2, civFX, threshold = sessiondat[4, 2]))
  )
  list(posterior, Pi, outPred, xtag, ytag, adultLocDF[,1], adultLocDF[,2])
}

histAll <- function(posterior,sessiondat,parameters,shape,xtag,ytag,ratx,raty) { 
             nSession <- dim(sessiondat)[2]-1
             nRats <- sessiondat[3,2]
             ncols = ceiling(sqrt(nSession+1))
             nrows = ceiling((nSession+1)/ncols)
             par(mfrow=c(nrows, ncols),pty="s")	
             for(i in 1:nSession)
               {
                 hx <- hist(posterior[,i],xlab=paste("Session",i,"PFree"),main="",cex.axis=1.4,cex.lab=1.4)
                 posx <- seq(min(1-posterior[,i]),1,length.out=100)[1] # Shifted to after main plot
                 posy <- seq(1,max(hx$counts),length.out=100)[98] # Shifted to after main plot
                 abline(v=mean(posterior[,i]),lwd = 3,col="blue")
                 abline(v=quantile(posterior[,i],.025),lwd = 3,col="red")
                 legend(posx,posy,legend=c("Mean","2.5th Quantile"),lwd=c(3,3),col=c("blue","red"))
               }
}