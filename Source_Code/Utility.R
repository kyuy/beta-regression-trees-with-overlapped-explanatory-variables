#########################
#
# Utility Functions
#
#########################

##########################################
#Transform long format into wide format
dataformat.long.to.wide <- function(data, id, choice, alt.var, f.var, s.var) {
#id = individual ID in long format (not exist in wide format)
#choice = choice of alternatives (no/yes or FALSE/TRUE) in long format (alt class of choice in wide format)
#alt.var = alternative variable in long format (not exist in wide format, so alt.var=choice)
#f.var=alternative specific variables for fitting (always numeric)
#s.var=individual specific variables for splitting (n=numeric, c=categorical)
 

#print("format.long.to.wide...ok")

   if(!is.data.frame(data)) data = data.frame(data)

   i = sort.list(data[,alt.var]); data = data[i,] #sorting by alt.var
   i = sort.list(data[,id]);      data = data[i,] #sorting by id

   id.unique = sort(unique(data[,id]))
   n = length(id.unique) # N of individuals

   alt.levels = sort(unique(data[,alt.var])) 
   nlevels = length(alt.levels) #N of levels for alt.var

   if(nrow(data) != n*nlevels) stop("check the data!")

   choice.unique= sort(unique(data[,choice]))
   if(length(choice.unique) != 2) stop("check the data!")
   anw = choice.unique[2] #The 2nd item of choice must yes OR TRUE

   ii = which(data[,choice]==anw)
   choice.tmp = data[ii,alt.var] # choice variable for wide format 

   jj = which2(names(data), s.var)
   data.wide = cbind(choice.tmp, data[ii,jj]) # individual specific variables for wide format
   colnames(data.wide) = c(choice, names(data[ii,jj]))
   
   #print(head(data.wide))

   for(i in 1:length(f.var)) {   
      tmp = data.frame(t(matrix(data[,f.var[i]], nlevels, n))) # alternative specific variables for wide format
      names(tmp) = as.factor(paste(f.var[i],alt.levels,sep="."))
      data.wide = cbind(data.wide, tmp)
   }

#print("dataformat.long.to.wide...ok")

  return(data.wide)
}



##########################################
#Transform long format into wide format
transf.long.to.wide <- function(data, ns) {

#print("transf.long.to.wide...ok")

   if(!is.data.frame(data)) data = data.frame(data)

   nc = ncol(data) 
   nr = nrow(data)
   nr2 = nr/ns
 
   ii = (1:nr2)*ns - (ns-1)
   tmp = data[ii,]
 
   for(i in 2:ns) {
      ii = (1:nr2)*ns - (ns-i)
     tmp = cbind(tmp, data[ii,]) 
   }

#print("transf.long.to.wide...ok")

  return(tmp)
}

#a = transf.long.to.wide(dat3, 4)
#data = a

##########################################
#Transform wide format into long format
transf.wide.to.long <- function(data, ns) {

#print("transf.wide.to.long...")

  #ns = size.clusters
  nc = ncol(data)/ns 
  nr2 = nrow(data)
  nr = nr2*ns

  tmp = data[,1:nc]
  for(i in 2:ns) {
    j = ((i-1)*nc+1):(i*nc)
    tmp2 = data[,j]
    tmp = rbind(tmp, tmp2) 
  }

  tmp2 = tmp
  j = (1:ns) 
  jj = ((1:ns)-1)*nr2 + 1
  tmp[j,] = tmp2[jj,]

  for(i in 2:nr2) {

    j = j + ns
    jj = ((1:ns)-1)*nr2 + i
    tmp[j,] = tmp2[jj,]

  }

#print(head(tmp))
#print("transf.wide.to.long...ok")

  return(tmp)
}

#aa = transf.wide.to.long(a, 4)
#aa <- mlogit.data(aa, choice="choice", shape = "long", alt.var="mode")




##########################################
#Note that factors are treated only via their internal integer codes; one proposal has been to use 
c.factor <- function(..., recursive=TRUE) unlist(list(...), recursive=recursive)
#if factor concatenation by c() should give a factor. 


###########################################################################
#Cluster ids
#1,2,3,.. => 11..1,22...2,33...3,...

#ii <- c(1,3,7,10)
#ii <- 1:10
#id.clusters(ii, 4)

id.clusters <- function(ids, size.clusters) {

   ids.clusters = c()
   for(i in 1:size.clusters){
     ids.clusters = c(ids.clusters, ids*size.clusters-(size.clusters-i))
   }
   ids.clusters = sort(ids.clusters)

  return(ids.clusters)

}



##########################################
#Convert decimal system into binary system
binary <- function(p_number) {
  bsum <- 0
  bexp <- 1
  while (p_number > 0) {
     digit <- p_number %% 2
     p_number <- floor(p_number / 2)
     bsum <- bsum + digit * bexp
     bexp <- bexp * 10
  }
  return(bsum)
}

###########################################################
#Improve which function: b is a vector rather than a scaler
which2 <- function(a, b) {
  
  ii = c()
  for(i in 1:length(b)) ii = c(ii, which(a==b[i]))

  ii = sort(ii)

  return(ii)
}


###########################################################



