#########################################################
# Split Rule Selection by Residual Analysis (RA) for DC
#########################################################
split.rule.ct <- function(formula, s.var.index, s.var.type, target, data, method.point, impurity.ft, MINDAT) {
    
    print("split.rule.ct...")
    #print(dim(data))
    
    
    ###Split variable selection
    split.var = 1
    if(length(s.var.index) > 1) {
        tmp = split.var.selection.ct(formula, s.var.index, s.var.type, target, data, impurity.ft, MINDAT)
        split.var = tmp$split.var #split variable selected 
    }
    print("Split variable is... "); print(split.var)
    
    
    ###Split point/set selection for the selected split variable
    X = data[,s.var.index] #data with split variables
    if(!is.data.frame(X)) X=data.frame(X)
    
    if(s.var.type[split.var]=="n")  {   #X:ordered
        
        if(method.point=="median") {   #shortcut
            tmp = split.shortcut.orderedX(formula=formula, target=target, 
                                          x=X[,split.var], data=data, MINDAT=MINDAT, impurity.ft=impurity.ft)
        } else {                       #ES
            tmp = split.es.orderedX(formula=formula, target=target, 
                                    x=X[,split.var], data=data, MINDAT=MINDAT, impurity.ft=impurity.ft)
        }
        
        
    } else {                           #X:unordered
        
        #if(method.set=="shortcut") {  #shortcut
        #   tmp = split.shortcut.unorderedX(formula=formula, target=target, 
        #                                   x=X[,split.var], data=data, MINDAT=MINDAT, impurity.ft=impurity.ft)
        #} else {                      #ES
        tmp = split.es.unorderedX(formula=formula, target=target, 
                                  x=X[,split.var], data=data, MINDAT=MINDAT, impurity.ft=impurity.ft)
        #}   
        
    }
    
    delta = tmp$max.delta
    split.set = tmp$pt #split point/set selected 
    
    split.set.elements = NA    
    if(s.var.type[split.var]=="c") split.set.elements = sort(unique(X[,split.var])) 
    
    
    print("Split point/set is... "); print(split.set); print(split.set.elements)
    
    print("split.rule.ra...ok")
    
    return(list(split.var=split.var, split.set=split.set, split.set.elements=split.set.elements, delta=delta, impurity.ft=impurity.ft))
}


###################################################
#Split variable selection by residual analysis (RA)
split.var.selection.ct <- function(formula, s.var.index, s.var.type, target, data, impurity.ft, MINDAT) {
    
    print("split.var.selection.ct...")
    
    #print(dim(data)); print(head(data))
    
    
    X = data[,s.var.index] #data with split variables
    if(!is.data.frame(X)) X=data.frame(X)
    selected = 'start'
    temp = 1
    lp = summary(lmp(formula, data = data, perm = "exact"))
    print(lp$coefficients)
    for(i in 1:length(s.var.index)){
        p.val = lp$coefficients[(i+1), 4]
        if(p.val < temp){
            temp = p.val
            selected = s.var.index[i]
        }
    }

    print("split.var.selection.ct...ok")
    return(list(split.var=selected))
}



####################################################
#Selecting a split point by median for ordered variable X
split.shortcut.orderedX <- function(formula, choice, n.classes, varying, x, data, MINDAT, impurity.ft) {
    
    #print("split.shortcut.orderedX...")
    
    
    pt = median(x) #median of x as the split point
    max.delta=NA
    
    if(nrow(data) >= MINDAT) { 
        
        impur = impurity.DC(formula, choice, n.classes, varying, data, impurity.ft)
        
        ii = which(x <= pt)
        
        pL=1; pR=1
        if(impurity.ft == "rsquare") {pL = length(ii)/length(x); pR=1-pL}
        if(impurity.ft == "misclass") {pL = length(ii)/length(x); pR=1-pL}
        
        if((nrow(data[ii,]) >= MINDAT) & (nrow(data[-ii,]) >= MINDAT)) { 
            impur.tL = impurity.DC(formula, choice, n.classes, varying, data[ii,],  impurity.ft)
            impur.tR = impurity.DC(formula, choice, n.classes, varying, data[-ii,], impurity.ft)
            max.delta = impur - (pL*impur.tL + pR*impur.tR)
        }  
    }
    
    if(is.na(max.delta)) pt = NA
    
    #print("split.shortcut.orderedX...ok")
    
    return(list(pt=pt, max.delta=max.delta))
    
}

