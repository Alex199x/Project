    library(statnet)

    ## Warning: package 'statnet' was built under R version 4.0.5

    ## Warning: package 'tergm' was built under R version 4.0.5

    ## Warning: package 'networkDynamic' was built under R version 4.0.5

    ## Warning: package 'ergm.count' was built under R version 4.0.5

    ## Warning: package 'sna' was built under R version 4.0.5

    ## Warning: package 'tsna' was built under R version 4.0.5

    library(network)
    library(sna)
    library(ergm)
    library(ergm.ego)

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    library(dplyr)
    library(tibble)
    library(tidyr)

    ## Warning: package 'tidyr' was built under R version 4.0.5

    library(egor)

    load("C:/Users/Alex/Downloads/Project/ICPSR_36975/DS0001/36975-0001-Data.rda")
    load("C:/Users/Alex/Downloads/Project/ICPSR_36975/DS0005/36975-0005-Data.rda")

    ucnet.egos<-da36975.0005
    egos.attribute<-names(ucnet.egos)%in%c("GENDER","YEAR_PRELOAD","PRIM_KEY","WEIGHT_W1_DEMO")
    ucnet.egos<-ucnet.egos[egos.attribute]

    #Calculate the age by born year (Notice that the Wave 1 was May 2015-January 2016)
    ucnet.egos$YEAR_PRELOAD<-2015-ucnet.egos$YEAR_PRELOAD


    #numeric Gender factor.Male=1, Female=2
    ucnet.egos$GENDER<-as.numeric(ucnet.egos$GENDER)

    age.na<-ucnet.egos%>%filter(is.na(YEAR_PRELOAD)==TRUE)
    ucnet.egos<-ucnet.egos%>%filter(is.na(YEAR_PRELOAD)==FALSE)

    ucnet.egos<-ucnet.egos%>%rename(Age = YEAR_PRELOAD)
    #4 of egos does not have age, so we only left with 1155 egos

    #Data prepare for alters


    #"C2F_NSAMEREL","C1B_NSAMESEX","C1C_NSAMEAGE","C1C_NOLDER","PRIM_KEY"
    #Since we only need above attribute, for the sake of the computation, I just keep those column.
    alters.attribute<-names(da36975.0001)%in%c("C1B_NSAMESEX","C1C_NSAMEAGE","C1C_NOLDER","PRIM_KEY","ID_W1_W2_W3","B7C","B8A")
    ucnet.alters<-da36975.0001[alters.attribute]

    #delete invalid data (Those 4 egos does not have attribute)
    ucnet.alters<-ucnet.alters[-which(ucnet.alters$PRIM_KEY==age.na$PRIM_KEY[1]),]
    ucnet.alters<-ucnet.alters[-which(ucnet.alters$PRIM_KEY==age.na$PRIM_KEY[2]),]
    ucnet.alters<-ucnet.alters[-which(ucnet.alters$PRIM_KEY==age.na$PRIM_KEY[3]),]
    ucnet.alters<-ucnet.alters[-which(ucnet.alters$PRIM_KEY==age.na$PRIM_KEY[4]),]

    #numeric Gender factor.Same as ego = 2 
    ucnet.alters$C1B_NSAMESEX<-as.numeric(ucnet.alters$C1B_NSAMESEX)

    #Assign Gender value to alters

    ucnet.alters<-ucnet.alters %>%replace_na(list(C1B_NSAMESEX=3))

    for (i in 1:nrow(ucnet.alters)){
      
      if (ucnet.alters$C1B_NSAMESEX[i]==2){
    #if the sex lab of alters is "yes", then assign this alter the same gender as ego.
          
      ucnet.alters$C1B_NSAMESEX[i]<-ucnet.egos$GENDER[which(ucnet.egos$PRIM_KEY==ucnet.alters$PRIM_KEY[i])]

      }
      
      else if (ucnet.alters$C1B_NSAMESEX[i]==1){
        #If the sex lab of alters is "no", then assign this alter an opposite gender as ego.
        #Notice, since the number 1 and 2 represent "Male","Female" successively, the "3-gender number" will give a opposite number.
        
      ucnet.alters$C1B_NSAMESEX[i]<-3-ucnet.egos$GENDER[which(ucnet.egos$PRIM_KEY==ucnet.alters$PRIM_KEY[i])]
      
      }
      else if (ucnet.alters$C1B_NSAMESEX[i]==3) {
        ucnet.alters$C1B_NSAMESEX[i]<-3
      }
        
        
    }
    #Now, unify all the missing data to 3
    ucnet.alters$C1B_NSAMESEX[which(ucnet.alters$C1B_NSAMESEX==4)]<- 3

    #If we want to force data as factor "Male"/"Female" later, run this
    #ucnet.alters[which(ucnet.alters$C1B_NSAMESEX==1)]<-"Male"
    #ucnet.alters[which(ucnet.alters$C1B_NSAMESEX==2)]<-"Female"

    #Assign age label to alters


    #We will record the age of alters in a new column
    #1=no, 2=yes
    ucnet.alters$C1C_NOLDER<-as.numeric(ucnet.alters$C1C_NOLDER)
    ucnet.alters$C1C_NSAMEAGE<-as.numeric(ucnet.alters$C1C_NSAMEAGE)


    #Since the "if" loop can not handle "NA" value, we will value "NA" as 3
    ucnet.alters <-ucnet.alters %>%replace_na(list(C1C_NOLDER = 3,C1C_NSAMEAGE=3))

    #we will assign the alters with label"older"," younger","same age range"
                      
    for (i in 1:nrow(ucnet.alters)){
      if (ucnet.alters$C1C_NOLDER[i]==2){
        #if the logic of C1C_NOLDER=yes
        ucnet.alters$Age[i]<-'older'
      }
      else {
        if (ucnet.alters$C1C_NSAMEAGE[i]==2){
          #if the logic of C1C_NOLDER=No or NA, and the logic of C1C_NSAMEAGE=yes
          ucnet.alters$Age[i]<-'same'
        }
        else if (ucnet.alters$C1C_NOLDER[i]==1) {
          ucnet.alters$Age[i]<-'younger'
        }
        else if (ucnet.alters$C1C_NSAMEAGE[i]==3){
          ucnet.alters$Age[i]<-3
        }
      }
    }

    #Rearrange the data frame before transfer to egor data frame
    ucnet.alters<-ucnet.alters%>%rename(GENDER=C1B_NSAMESEX)
    alters.id<-ucnet.alters["ID_W1_W2_W3"]
    ucnet.alters<-ucnet.alters[c("PRIM_KEY","Age","GENDER","B7C","B8A")]
    ucnet.alters<-ucnet.alters%>%rename(indegree=B7C,outdegree=B8A)


    #convert tie column to logical
    ucnet.alters$indegree<-as.logical(ucnet.alters$indegree)
    ucnet.alters$outdegree<-as.logical(ucnet.alters$outdegree)
    ucnet.alters$indegree[is.na(ucnet.alters$indegree)==TRUE]<-FALSE
    ucnet.alters$outdegree[is.na(ucnet.alters$outdegree)==TRUE]<-FALSE

    #delete the egos and alters that have connection but do not have attribute.
    delete<-ucnet.alters%>%filter(ucnet.alters$indegree==TRUE |ucnet.alters$outdegree==TRUE)

    #need to be delete(notice no row gender=3)
    delete.row<-delete%>%filter(delete$Age==3)
    delete.id<-distinct( as.tibble(delete.row$PRIM_KEY))

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.

    delete.egos<-NULL

    for (i in 1:nrow(delete.id)){
      add<-ucnet.egos%>%filter(ucnet.egos$PRIM_KEY==as.numeric(as.character(delete.id$value[i])) )
      delete.egos<-rbind(add,delete.egos)
      ucnet.egos<-ucnet.egos%>%filter(ucnet.egos$PRIM_KEY!=as.numeric(as.character(delete.id$value[i])) )
      
    }

    dd<-NULL
    for (j in 1:nrow(ucnet.egos)){
      tt<-ucnet.alters%>%filter(ucnet.alters$PRIM_KEY==ucnet.egos$PRIM_KEY[j])
      dd<-rbind(tt,dd )
    }

    ucnet.alters<-dd
    #NO.alters=10417
    #NO.egos=622

    #Create an egor object
    ucnet.egor<-egor(ucnet.alters,ucnet.egos,ID.vars = list(ego="PRIM_KEY"))


    #Construct network 
    ucnet.network<-template_network(ucnet.egor,N=nrow(ucnet.egos))
    ucnet.network %n% "directed" <- TRUE

\#Model Specification

\#First fit an ERGM to the complete network

    #statistic term
    in.row<-ucnet.alters%>%filter(ucnet.alters$indegree==TRUE)
    out.row<-ucnet.alters%>%filter(ucnet.alters$outdegree==TRUE)
    for (i in 1:nrow(in.row)) {
     in.row$ego.gender[i]<-ucnet.egos$GENDER[which(ucnet.egos$PRIM_KEY==in.row$PRIM_KEY[i])]
     in.row$ego.age[i]<-ucnet.egos$Age[which(ucnet.egos$PRIM_KEY==in.row$PRIM_KEY[i])]
    }


    for (j in 1:nrow(out.row)) {
     out.row$ego.gender[j]<-ucnet.egos$GENDER[which(ucnet.egos$PRIM_KEY==out.row$PRIM_KEY[j])]
     out.row$ego.age[j]<-ucnet.egos$Age[which(ucnet.egos$PRIM_KEY==out.row$PRIM_KEY[j])]
    }

    #edge
    uc.edges<-sum(ucnet.alters$indegree)+sum(ucnet.alters$outdegree)


    #nodefactor--Gender(both in and out)

    ##nodeifactor.male: the number of times a node with Gender-male appears as the terminal node of a directed tie. AKA, the number of nominated "male" alters who egos want to help + the number of 'male' egos who alters will help with.

    nodeifactor.male<-(nrow(in.row%>%filter(ego.gender==1)) + nrow(out.row%>%filter(GENDER==1)))/2
    nodeifactor.female<-(nrow(in.row%>%filter(ego.gender==2)) + nrow(out.row%>%filter(GENDER==2)))/2

    ##nodeofactor.male: the number of times a node with Gender-male appears as the origin node of a directed tie.

    nodeofactor.male<-(nrow(in.row%>%filter(GENDER==1))+nrow(out.row%>%filter(ego.gender==1)))/2
    nodeofactor.female<-(nrow(in.row%>%filter(GENDER==2))+nrow(out.row%>%filter(ego.gender==2)))/2


    #nodematch-Gender 
    in.row.male<-in.row%>%filter(in.row$GENDER==1)
    out.row.male<-out.row%>%filter(out.row$GENDER==1)

    homophily.in.male<-nrow(in.row.male%>%filter(in.row.male$GENDER==in.row.male$ego.gender))
    homophily.out.male<-nrow(out.row.male%>%filter(out.row.male$GENDER==out.row.male$ego.gender))
    nodematch.male<-(homophily.in.male+homophily.out.male)/2


    homophily.in<-nrow(in.row%>%filter(in.row$GENDER==in.row$ego.gender))
    homophily.out<-nrow(out.row%>%filter(out.row$GENDER==out.row$ego.gender))
    nodematch<-(homophily.in+homophily.out)/2

    nodematch.female<-nodematch-nodematch.male


    #Reciprocal
    Reciprocal<-nrow(in.row%>%filter(in.row$outdegree==TRUE))

    #F(~edges + nodeocov("age") + nodeicov("age"), ~diff("age")>6)
    ##F~edges,the number of edges(i,j) for which attr(i,age)>=attr(j,age)
    f.in.row.big<-in.row%>%filter(Age=="older")
    f.out.row.big<-out.row%>%filter(Age=="younger")
    f.edges.big<-(nrow(f.in.row.big)+nrow(f.out.row.big))/2

    ##F~nodeicov("age"), This term equal to the total value of attr(j,age) for all edges(i,j) in the network. However, we do not know the age of alters, We will only sum up the age attribute of egos.
    f.nodeicov.big<-sum(f.in.row.big$ego.age)
    ##F~nodeocov("age")
    f.nodeocov.big<-sum(f.out.row.big$ego.age)


    #F(~edges + nodeocov("age") + nodeicov("age"), ~diff("age")<-6)
    f.in.row.small<-in.row%>%filter(Age=="younger")
    f.out.row.small<-out.row%>%filter(Age=="older")
    f.edges.small<-(nrow(f.in.row.small)+nrow(f.out.row.small))/2
    f.nodeicov.small<-sum(f.in.row.small$ego.age)
    f.nodeocov.small<-sum(f.out.row.small$ego.age)


    #F(~edges + nodeocov("age") + nodeicov("age"), ~~absdiff("Age")<=6)

    f.in.row.same<-in.row%>%filter(Age=="same")
    f.out.row.same<-out.row%>%filter(Age=="same")
    f.edges.same<-(nrow(f.in.row.same)+nrow(f.out.row.same))/2

    f.nodeicov.same<-sum(f.in.row.same$ego.age)
    f.nodeocov.same<-sum(f.out.row.same$ego.age)

    fit.01<-ergm(ucnet.network~
                       edges+nodeifactor('GENDER')+nodeofactor('GENDER') 
                 
                      +offset(edges),
                     
                     offset.coef = c(-log(11039)),
                 
                     constraints = ~blocks(~Age%in%31:49, levels2=4),
                     control = control.ergm(parallel = 4, SAN.maxit = 20,seed = 1),
                     target.stats=c(uc.edges,nodeifactor.male,nodeofactor.male),
                     )

    ## Unable to match target stats. Using MCMLE estimation.

    ## Starting maximum pseudolikelihood estimation (MPLE):

    ## Evaluating the predictor and response matrix.

    ## Maximizing the pseudolikelihood.

    ## Finished MPLE.

    ## Starting Monte Carlo maximum likelihood estimation (MCMLE):

    ## Iteration 1 of at most 60:

    ## Optimizing with step length 1.0000.

    ## The log-likelihood improved by 0.0464.

    ## Convergence test p-value: 0.5342. Not converged with 99% confidence; increasing sample size.
    ## Iteration 2 of at most 60:
    ## Optimizing with step length 1.0000.
    ## The log-likelihood improved by 0.0141.
    ## Convergence test p-value: 0.2737. Not converged with 99% confidence; increasing sample size.
    ## Iteration 3 of at most 60:
    ## Optimizing with step length 1.0000.
    ## The log-likelihood improved by 0.0278.
    ## Convergence test p-value: 0.0376. Not converged with 99% confidence; increasing sample size.
    ## Iteration 4 of at most 60:
    ## Optimizing with step length 1.0000.
    ## The log-likelihood improved by 0.0086.
    ## Convergence test p-value: 0.0140. Not converged with 99% confidence; increasing sample size.
    ## Iteration 5 of at most 60:
    ## Optimizing with step length 1.0000.
    ## The log-likelihood improved by 0.0037.
    ## Convergence test p-value: 0.0058. Converged with 99% confidence.
    ## Finished MCMLE.
    ## Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
    ## Bridging between the dyad-independent submodel and the full model...
    ## Setting up bridge sampling...

    ## Warning in ergm.bridge.llr(form.aug, constraints = constraints, obs.constraints
    ## = obs.constraints, : Using target.stats for a model with offset terms may
    ## produce an inaccurate estimate of the log-likelihood and derived quantities
    ## (deviance, AIC, BIC, etc.), because some of the target stats must be imputed.

    ## Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
    ## Bridging finished.
    ## This model was fit using MCMC.  To examine model diagnostics and check
    ## for degeneracy, use the mcmc.diagnostics() function.

    fit.02<-ergm(ucnet.network~
                          
                     nodematch("GENDER",diff=TRUE,levels=-1)+nodeifactor('GENDER')+nodeofactor('GENDER') 
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")>6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")< -6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~absdiff("Age")<=6)
                     
                     + offset(edges),
                    
                     
                     offset.coef = c(-log(11039)),
                     

                     constraints = ~blocks(~Age%in%31:49, levels2=4),
                 
                     control = control.ergm(parallel = 4, SAN.maxit = 40, seed = 1),

                     target.stats=c(nodematch.female,nodeifactor.male,nodeofactor.male,
                                    f.edges.big,f.nodeocov.big,f.nodeicov.big,
                                    f.edges.small,f.nodeocov.small,f.nodeicov.small,
                                    f.edges.same,f.nodeocov.same,f.nodeicov.same)
                     )

    ## Unable to match target stats. Using MCMLE estimation.

    ## Starting maximum pseudolikelihood estimation (MPLE):

    ## Evaluating the predictor and response matrix.

    ## Maximizing the pseudolikelihood.

    ## Finished MPLE.

    ## Starting Monte Carlo maximum likelihood estimation (MCMLE):

    ## Iteration 1 of at most 60:

    ## Optimizing with step length 0.0112.

    ## The log-likelihood improved by 2.3588.

    ## Estimating equations are not within tolerance region.

    ## Iteration 2 of at most 60:

    ## Optimizing with step length 0.0107.

    ## The log-likelihood improved by 2.4903.

    ## Estimating equations are not within tolerance region.

    ## Iteration 3 of at most 60:

    ## Optimizing with step length 0.0107.

    ## The log-likelihood improved by 2.5196.

    ## Estimating equations are not within tolerance region.

    ## Iteration 4 of at most 60:

    ## Optimizing with step length 0.0092.

    ## The log-likelihood improved by 1.9000.

    ## Estimating equations are not within tolerance region.

    ## Iteration 5 of at most 60:

    ## Optimizing with step length 0.0091.

    ## The log-likelihood improved by 2.0059.

    ## Estimating equations are not within tolerance region.

    ## Iteration 6 of at most 60:

    ## Optimizing with step length 0.0091.

    ## The log-likelihood improved by 1.8676.

    ## Estimating equations are not within tolerance region.

    ## Iteration 7 of at most 60:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 1.4677.

    ## Estimating equations are not within tolerance region.

    ## Iteration 8 of at most 60:

    ## Optimizing with step length 0.0091.

    ## The log-likelihood improved by 2.2078.

    ## Estimating equations are not within tolerance region.

    ## Iteration 9 of at most 60:

    ## Optimizing with step length 0.0091.

    ## The log-likelihood improved by 2.4950.

    ## Estimating equations are not within tolerance region.

    ## Iteration 10 of at most 60:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 1.8801.

    ## Estimating equations are not within tolerance region.

    ## Iteration 11 of at most 60:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 1.9950.

    ## Estimating equations are not within tolerance region.

    ## Iteration 12 of at most 60:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 2.1866.

    ## Estimating equations are not within tolerance region.

    ## Iteration 13 of at most 60:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 2.1403.

    ## Estimating equations are not within tolerance region.

    ## Iteration 14 of at most 60:

    ## Optimizing with step length 0.0061.

    ## The log-likelihood improved by 1.5083.

    ## Estimating equations are not within tolerance region.

    ## Iteration 15 of at most 60:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 2.7859.

    ## Estimating equations are not within tolerance region.

    ## Iteration 16 of at most 60:

    ## Optimizing with step length 0.0061.

    ## The log-likelihood improved by 2.0792.

    ## Estimating equations are not within tolerance region.

    ## Iteration 17 of at most 60:

    ## Optimizing with step length 0.0061.

    ## The log-likelihood improved by 2.0838.

    ## Estimating equations are not within tolerance region.

    ## Iteration 18 of at most 60:

    ## Optimizing with step length 0.0053.

    ## The log-likelihood improved by 2.0378.

    ## Estimating equations are not within tolerance region.

    ## Iteration 19 of at most 60:

    ## Optimizing with step length 0.0061.

    ## The log-likelihood improved by 2.5074.

    ## Estimating equations are not within tolerance region.

    ## Iteration 20 of at most 60:

    ## Optimizing with step length 0.0046.

    ## The log-likelihood improved by 1.8629.

    ## Estimating equations are not within tolerance region.

    ## Iteration 21 of at most 60:

    ## Optimizing with step length 0.0038.

    ## The log-likelihood improved by 1.5316.

    ## Estimating equations are not within tolerance region.

    ## Iteration 22 of at most 60:

    ## Optimizing with step length 0.0053.

    ## The log-likelihood improved by 2.8453.

    ## Estimating equations are not within tolerance region.

    ## Iteration 23 of at most 60:

    ## Optimizing with step length 0.0046.

    ## The log-likelihood improved by 2.6020.

    ## Estimating equations are not within tolerance region.

    ## Iteration 24 of at most 60:

    ## Optimizing with step length 0.0038.

    ## The log-likelihood improved by 1.8927.

    ## Estimating equations are not within tolerance region.

    ## Iteration 25 of at most 60:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.5054.

    ## Estimating equations are not within tolerance region.

    ## Iteration 26 of at most 60:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.6557.

    ## Estimating equations are not within tolerance region.

    ## Iteration 27 of at most 60:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.7831.

    ## Estimating equations are not within tolerance region.

    ## Iteration 28 of at most 60:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.7894.

    ## Estimating equations are not within tolerance region.

    ## Iteration 29 of at most 60:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.8263.

    ## Estimating equations are not within tolerance region.

    ## Iteration 30 of at most 60:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 2.1025.

    ## Estimating equations are not within tolerance region.

    ## Iteration 31 of at most 60:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 1.8557.

    ## Estimating equations are not within tolerance region.

    ## Iteration 32 of at most 60:

    ## Optimizing with step length 0.0023.

    ## The log-likelihood improved by 1.5426.

    ## Estimating equations are not within tolerance region.

    ## Iteration 33 of at most 60:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 2.2521.

    ## Estimating equations are not within tolerance region.

    ## Iteration 34 of at most 60:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 2.5573.

    ## Estimating equations are not within tolerance region.

    ## Iteration 35 of at most 60:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.6317.

    ## Estimating equations are not within tolerance region.

    ## Iteration 36 of at most 60:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.5900.

    ## Estimating equations are not within tolerance region.

    ## Iteration 37 of at most 60:

    ## Optimizing with step length 0.0023.

    ## The log-likelihood improved by 2.4073.

    ## Estimating equations are not within tolerance region.

    ## Iteration 38 of at most 60:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.8536.

    ## Estimating equations are not within tolerance region.

    ## Iteration 39 of at most 60:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.7774.

    ## Estimating equations are not within tolerance region.

    ## Iteration 40 of at most 60:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 2.0413.

    ## Estimating equations are not within tolerance region.

    ## Iteration 41 of at most 60:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 2.3647.

    ## Estimating equations are not within tolerance region.

    ## Iteration 42 of at most 60:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.7488.

    ## Estimating equations are not within tolerance region.

    ## Iteration 43 of at most 60:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.9786.

    ## Estimating equations are not within tolerance region.

    ## Iteration 44 of at most 60:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.6349.

    ## Estimating equations are not within tolerance region.

    ## Iteration 45 of at most 60:

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 1.6031.

    ## Estimating equations are not within tolerance region.

    ## Iteration 46 of at most 60:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 2.0329.

    ## Estimating equations are not within tolerance region.

    ## Iteration 47 of at most 60:

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 1.8097.

    ## Estimating equations are not within tolerance region.

    ## Iteration 48 of at most 60:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 2.2409.

    ## Estimating equations are not within tolerance region.

    ## Iteration 49 of at most 60:

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 2.5243.

    ## Estimating equations are not within tolerance region.

    ## Iteration 50 of at most 60:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.5796.

    ## Estimating equations are not within tolerance region.

    ## Iteration 51 of at most 60:

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 1.9742.

    ## Estimating equations are not within tolerance region.

    ## Iteration 52 of at most 60:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.7790.

    ## Estimating equations are not within tolerance region.

    ## Iteration 53 of at most 60:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.4094.

    ## Estimating equations are not within tolerance region.

    ## Iteration 54 of at most 60:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.9841.

    ## Estimating equations are not within tolerance region.

    ## Iteration 55 of at most 60:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.6971.

    ## Estimating equations are not within tolerance region.

    ## Iteration 56 of at most 60:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.8173.

    ## Estimating equations are not within tolerance region.

    ## Iteration 57 of at most 60:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.8671.

    ## Estimating equations are not within tolerance region.

    ## Iteration 58 of at most 60:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.6351.

    ## Estimating equations are not within tolerance region.

    ## Iteration 59 of at most 60:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.6563.

    ## Estimating equations are not within tolerance region.

    ## Iteration 60 of at most 60:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.0072.

    ## Estimating equations are not within tolerance region.

    ## MCMLE estimation did not converge after 60 iterations. The estimated coefficients may not be accurate. Estimation may be resumed by passing the coefficients as initial values; see 'init' under ?control.ergm for details.

    ## Finished MCMLE.

    ## Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
    ## Bridging between the dyad-independent submodel and the full model...
    ## Setting up bridge sampling...

    ## Warning in ergm.bridge.llr(form.aug, constraints = constraints, obs.constraints
    ## = obs.constraints, : Using target.stats for a model with offset terms may
    ## produce an inaccurate estimate of the log-likelihood and derived quantities
    ## (deviance, AIC, BIC, etc.), because some of the target stats must be imputed.

    ## Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
    ## Bridging finished.
    ## This model was fit using MCMC.  To examine model diagnostics and check
    ## for degeneracy, use the mcmc.diagnostics() function.

    fit.03<-ergm(ucnet.network~
                          
                     nodematch("GENDER",diff=TRUE,levels=-1)+nodeifactor("GENDER")+nodeofactor("GENDER") +mutual
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")>6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")< -6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~absdiff("Age")<=6)
                     + offset(mutual)+ offset(edges),
                    
                     
                     offset.coef = c(log(11039),-log(11039)),

                     constraints = ~blocks(~Age%in%31:49, levels2=4),
                 
                     control = control.ergm(parallel = 4, SAN.maxit =100,seed = 1, MCMLE.maxit = 50),

                     target.stats=c(nodematch.female,nodeifactor.male,nodeofactor.male,Reciprocal/2,
                                    f.edges.big,f.nodeocov.big,f.nodeicov.big,
                                    f.edges.small,f.nodeocov.small,f.nodeicov.small,
                                    f.edges.same,f.nodeocov.same,f.nodeicov.same)
    )

    ## Unable to match target stats. Using MCMLE estimation.

    ## Starting maximum pseudolikelihood estimation (MPLE):

    ## Evaluating the predictor and response matrix.

    ## Maximizing the pseudolikelihood.

    ## Finished MPLE.

    ## Starting Monte Carlo maximum likelihood estimation (MCMLE):

    ## Iteration 1 of at most 50:

    ## Optimizing with step length 0.0112.

    ## The log-likelihood improved by 1.8591.

    ## Estimating equations are not within tolerance region.

    ## Iteration 2 of at most 50:

    ## Optimizing with step length 0.0122.

    ## The log-likelihood improved by 2.5709.

    ## Estimating equations are not within tolerance region.

    ## Iteration 3 of at most 50:

    ## Optimizing with step length 0.0107.

    ## The log-likelihood improved by 2.0855.

    ## Estimating equations are not within tolerance region.

    ## Iteration 4 of at most 50:

    ## Optimizing with step length 0.0107.

    ## The log-likelihood improved by 2.0522.

    ## Estimating equations are not within tolerance region.

    ## Iteration 5 of at most 50:

    ## Optimizing with step length 0.0107.

    ## The log-likelihood improved by 2.1177.

    ## Estimating equations are not within tolerance region.

    ## Iteration 6 of at most 50:

    ## Optimizing with step length 0.0092.

    ## The log-likelihood improved by 1.8133.

    ## Estimating equations are not within tolerance region.

    ## Iteration 7 of at most 50:

    ## Optimizing with step length 0.0091.

    ## The log-likelihood improved by 1.7584.

    ## Estimating equations are not within tolerance region.

    ## Iteration 8 of at most 50:

    ## Optimizing with step length 0.0091.

    ## The log-likelihood improved by 2.0018.

    ## Estimating equations are not within tolerance region.

    ## Iteration 9 of at most 50:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 1.4707.

    ## Estimating equations are not within tolerance region.

    ## Iteration 10 of at most 50:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 1.4778.

    ## Estimating equations are not within tolerance region.

    ## Iteration 11 of at most 50:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 1.9417.

    ## Estimating equations are not within tolerance region.

    ## Iteration 12 of at most 50:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 1.8816.

    ## Estimating equations are not within tolerance region.

    ## Iteration 13 of at most 50:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 2.1145.

    ## Estimating equations are not within tolerance region.

    ## Iteration 14 of at most 50:

    ## Optimizing with step length 0.0076.

    ## The log-likelihood improved by 2.1482.

    ## Estimating equations are not within tolerance region.

    ## Iteration 15 of at most 50:

    ## Optimizing with step length 0.0061.

    ## The log-likelihood improved by 1.8097.

    ## Estimating equations are not within tolerance region.

    ## Iteration 16 of at most 50:

    ## Optimizing with step length 0.0061.

    ## The log-likelihood improved by 2.0347.

    ## Estimating equations are not within tolerance region.

    ## Iteration 17 of at most 50:

    ## Optimizing with step length 0.0061.

    ## The log-likelihood improved by 1.8536.

    ## Estimating equations are not within tolerance region.

    ## Iteration 18 of at most 50:

    ## Optimizing with step length 0.0053.

    ## The log-likelihood improved by 1.8568.

    ## Estimating equations are not within tolerance region.

    ## Iteration 19 of at most 50:

    ## Optimizing with step length 0.0046.

    ## The log-likelihood improved by 2.0309.

    ## Estimating equations are not within tolerance region.

    ## Iteration 20 of at most 50:

    ## Optimizing with step length 0.0046.

    ## The log-likelihood improved by 1.9911.

    ## Estimating equations are not within tolerance region.

    ## Iteration 21 of at most 50:

    ## Optimizing with step length 0.0038.

    ## The log-likelihood improved by 1.7204.

    ## Estimating equations are not within tolerance region.

    ## Iteration 22 of at most 50:

    ## Optimizing with step length 0.0038.

    ## The log-likelihood improved by 2.0776.

    ## Estimating equations are not within tolerance region.

    ## Iteration 23 of at most 50:

    ## Optimizing with step length 0.0038.

    ## The log-likelihood improved by 2.0053.

    ## Estimating equations are not within tolerance region.

    ## Iteration 24 of at most 50:

    ## Optimizing with step length 0.0038.

    ## The log-likelihood improved by 2.0801.

    ## Estimating equations are not within tolerance region.

    ## Iteration 25 of at most 50:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.6541.

    ## Estimating equations are not within tolerance region.

    ## Iteration 26 of at most 50:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 1.3921.

    ## Estimating equations are not within tolerance region.

    ## Iteration 27 of at most 50:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.7285.

    ## Estimating equations are not within tolerance region.

    ## Iteration 28 of at most 50:

    ## Optimizing with step length 0.0031.

    ## The log-likelihood improved by 1.8451.

    ## Estimating equations are not within tolerance region.

    ## Iteration 29 of at most 50:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 1.9679.

    ## Estimating equations are not within tolerance region.

    ## Iteration 30 of at most 50:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 2.0874.

    ## Estimating equations are not within tolerance region.

    ## Iteration 31 of at most 50:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 2.4432.

    ## Estimating equations are not within tolerance region.

    ## Iteration 32 of at most 50:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.2131.

    ## Estimating equations are not within tolerance region.

    ## Iteration 33 of at most 50:

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 2.8385.

    ## Estimating equations are not within tolerance region.

    ## Iteration 34 of at most 50:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.7611.

    ## Estimating equations are not within tolerance region.

    ## Iteration 35 of at most 50:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.7662.

    ## Estimating equations are not within tolerance region.

    ## Iteration 36 of at most 50:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.9848.

    ## Estimating equations are not within tolerance region.

    ## Iteration 37 of at most 50:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 2.0845.

    ## Estimating equations are not within tolerance region.

    ## Iteration 38 of at most 50:

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 2.3065.

    ## Estimating equations are not within tolerance region.

    ## Iteration 39 of at most 50:

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 1.6446.

    ## Estimating equations are not within tolerance region.

    ## Iteration 40 of at most 50:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 2.3518.

    ## Estimating equations are not within tolerance region.

    ## Iteration 41 of at most 50:

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 1.8729.

    ## Estimating equations are not within tolerance region.

    ## Iteration 42 of at most 50:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 2.4342.

    ## Estimating equations are not within tolerance region.

    ## Iteration 43 of at most 50:

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 2.1981.

    ## Estimating equations are not within tolerance region.

    ## Iteration 44 of at most 50:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.8505.

    ## Estimating equations are not within tolerance region.

    ## Iteration 45 of at most 50:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.9543.

    ## Estimating equations are not within tolerance region.

    ## Iteration 46 of at most 50:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 2.2642.

    ## Estimating equations are not within tolerance region.

    ## Iteration 47 of at most 50:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 2.0211.

    ## Estimating equations are not within tolerance region.

    ## Iteration 48 of at most 50:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 2.2963.

    ## Estimating equations are not within tolerance region.

    ## Iteration 49 of at most 50:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.0041.

    ## Estimating equations are not within tolerance region.

    ## Iteration 50 of at most 50:

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 1.8618.

    ## Estimating equations are not within tolerance region.

    ## MCMLE estimation did not converge after 50 iterations. The estimated coefficients may not be accurate. Estimation may be resumed by passing the coefficients as initial values; see 'init' under ?control.ergm for details.

    ## Finished MCMLE.

    ## Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
    ## Bridging between the dyad-independent submodel and the full model...
    ## Setting up bridge sampling...

    ## Warning in ergm.bridge.llr(form.aug, constraints = constraints, obs.constraints
    ## = obs.constraints, : Using target.stats for a model with offset terms may
    ## produce an inaccurate estimate of the log-likelihood and derived quantities
    ## (deviance, AIC, BIC, etc.), because some of the target stats must be imputed.

    ## Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
    ## Bridging finished.
    ## This model was fit using MCMC.  To examine model diagnostics and check
    ## for degeneracy, use the mcmc.diagnostics() function.

    summary(fit.01)

    ## Call:
    ## ergm(formula = ucnet.network ~ edges + nodeifactor("GENDER") + 
    ##     nodeofactor("GENDER") + offset(edges), constraints = ~blocks(~Age %in% 
    ##     31:49, levels2 = 4), offset.coef = c(-log(11039)), target.stats = c(uc.edges, 
    ##     nodeifactor.male, nodeofactor.male), control = control.ergm(parallel = 4, 
    ##     SAN.maxit = 20, seed = 1))
    ## 
    ## Monte Carlo Maximum Likelihood Results:
    ## 
    ##                      Estimate Std. Error MCMC % z value Pr(>|z|)    
    ## edges                 6.69925    0.01964      0  341.12   <1e-04 ***
    ## nodeifactor.GENDER.2 -2.35586    0.04296      0  -54.84   <1e-04 ***
    ## nodeofactor.GENDER.2 -2.35677    0.04311      0  -54.66   <1e-04 ***
    ## offset(edges)        -9.30919    0.00000      0    -Inf   <1e-04 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##      Null Deviance:   535465  on 386256  degrees of freedom
    ##  Residual Deviance: -4545997  on 386252  degrees of freedom
    ##  
    ## AIC: -4545991  BIC: -4545959  (Smaller is better. MC Std. Err. = 174)
    ## 
    ##  The following terms are fixed by offset and are not estimated:
    ##   offset(edges)

    summary(fit.02)

    ## Call:
    ## ergm(formula = ucnet.network ~ nodematch("GENDER", diff = TRUE, 
    ##     levels = -1) + nodeifactor("GENDER") + nodeofactor("GENDER") + 
    ##     F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") > 
    ##         6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") < 
    ##     -6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~absdiff("Age") <= 
    ##     6) + offset(edges), constraints = ~blocks(~Age %in% 31:49, 
    ##     levels2 = 4), offset.coef = c(-log(11039)), target.stats = c(nodematch.female, 
    ##     nodeifactor.male, nodeofactor.male, f.edges.big, f.nodeocov.big, 
    ##     f.nodeicov.big, f.edges.small, f.nodeocov.small, f.nodeicov.small, 
    ##     f.edges.same, f.nodeocov.same, f.nodeicov.same), control = control.ergm(parallel = 4, 
    ##     SAN.maxit = 40, seed = 1))
    ## 
    ## Monte Carlo Maximum Likelihood Results:
    ## 
    ##                                   Estimate Std. Error MCMC % z value Pr(>|z|)
    ## nodematch.GENDER.2                 6.09794    0.11444      0   53.28   <1e-04
    ## nodeifactor.GENDER.2              -4.56815    0.09849      0  -46.38   <1e-04
    ## nodeofactor.GENDER.2              -4.02839    0.08160      0  -49.37   <1e-04
    ## F(diff("Age")>6)~edges            40.83931    1.82730      0   22.35   <1e-04
    ## F(diff("Age")>6)~nodeocov.Age     -4.64208    0.25977      0  -17.87   <1e-04
    ## F(diff("Age")>6)~nodeicov.Age      4.70265    0.25980      0   18.10   <1e-04
    ## F(diff("Age")<-6)~edges           25.89535    0.78651      0   32.92   <1e-04
    ## F(diff("Age")<-6)~nodeocov.Age     2.61364    0.10889      0   24.00   <1e-04
    ## F(diff("Age")<-6)~nodeicov.Age    -2.55204    0.10827      0  -23.57   <1e-04
    ## F(absdiff("Age")<=6)~edges        -3.23213    0.24528      0  -13.18   <1e-04
    ## F(absdiff("Age")<=6)~nodeocov.Age  2.31861    0.04479      0   51.77   <1e-04
    ## F(absdiff("Age")<=6)~nodeicov.Age -2.30929    0.04524      0  -51.05   <1e-04
    ## offset(edges)                     -9.30919    0.00000      0    -Inf   <1e-04
    ##                                      
    ## nodematch.GENDER.2                ***
    ## nodeifactor.GENDER.2              ***
    ## nodeofactor.GENDER.2              ***
    ## F(diff("Age")>6)~edges            ***
    ## F(diff("Age")>6)~nodeocov.Age     ***
    ## F(diff("Age")>6)~nodeicov.Age     ***
    ## F(diff("Age")<-6)~edges           ***
    ## F(diff("Age")<-6)~nodeocov.Age    ***
    ## F(diff("Age")<-6)~nodeicov.Age    ***
    ## F(absdiff("Age")<=6)~edges        ***
    ## F(absdiff("Age")<=6)~nodeocov.Age ***
    ## F(absdiff("Age")<=6)~nodeicov.Age ***
    ## offset(edges)                     ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##      Null Deviance:   535465  on 386256  degrees of freedom
    ##  Residual Deviance: -4307961  on 386243  degrees of freedom
    ##  
    ## AIC: -4307937  BIC: -4307806  (Smaller is better. MC Std. Err. = 90.36)
    ## 
    ##  The following terms are fixed by offset and are not estimated:
    ##   offset(edges)

    summary(fit.03)

    ## Call:
    ## ergm(formula = ucnet.network ~ nodematch("GENDER", diff = TRUE, 
    ##     levels = -1) + nodeifactor("GENDER") + nodeofactor("GENDER") + 
    ##     mutual + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") > 
    ##     6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") < 
    ##     -6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~absdiff("Age") <= 
    ##     6) + offset(mutual) + offset(edges), constraints = ~blocks(~Age %in% 
    ##     31:49, levels2 = 4), offset.coef = c(log(11039), -log(11039)), 
    ##     target.stats = c(nodematch.female, nodeifactor.male, nodeofactor.male, 
    ##         Reciprocal/2, f.edges.big, f.nodeocov.big, f.nodeicov.big, 
    ##         f.edges.small, f.nodeocov.small, f.nodeicov.small, f.edges.same, 
    ##         f.nodeocov.same, f.nodeicov.same), control = control.ergm(parallel = 4, 
    ##         SAN.maxit = 100, seed = 1, MCMLE.maxit = 50))
    ## 
    ## Monte Carlo Maximum Likelihood Results:
    ## 
    ##                                   Estimate Std. Error MCMC % z value Pr(>|z|)
    ## nodematch.GENDER.2                 5.02819    0.12583      0  39.959  < 1e-04
    ## nodeifactor.GENDER.2              -3.76802    0.08784      0 -42.896  < 1e-04
    ## nodeofactor.GENDER.2              -3.34812    0.09178      0 -36.478  < 1e-04
    ## mutual                            -6.78590    0.12295      0 -55.190  < 1e-04
    ## F(diff("Age")>6)~edges            46.45078    2.47265      0  18.786  < 1e-04
    ## F(diff("Age")>6)~nodeocov.Age     -5.61769    0.35145      0 -15.984  < 1e-04
    ## F(diff("Age")>6)~nodeicov.Age      5.68753    0.35160      0  16.176  < 1e-04
    ## F(diff("Age")<-6)~edges            9.67668    0.24000      0  40.320  < 1e-04
    ## F(diff("Age")<-6)~nodeocov.Age     0.41811    0.02937      0  14.234  < 1e-04
    ## F(diff("Age")<-6)~nodeicov.Age    -0.38384    0.02822      0 -13.603  < 1e-04
    ## F(absdiff("Age")<=6)~edges         0.63081    0.17943      0   3.516 0.000439
    ## F(absdiff("Age")<=6)~nodeocov.Age  1.57567    0.03398      0  46.374  < 1e-04
    ## F(absdiff("Age")<=6)~nodeicov.Age -1.57033    0.03423      0 -45.875  < 1e-04
    ## offset(mutual)                     9.30919    0.00000      0     Inf  < 1e-04
    ## offset(edges)                     -9.30919    0.00000      0    -Inf  < 1e-04
    ##                                      
    ## nodematch.GENDER.2                ***
    ## nodeifactor.GENDER.2              ***
    ## nodeofactor.GENDER.2              ***
    ## mutual                            ***
    ## F(diff("Age")>6)~edges            ***
    ## F(diff("Age")>6)~nodeocov.Age     ***
    ## F(diff("Age")>6)~nodeicov.Age     ***
    ## F(diff("Age")<-6)~edges           ***
    ## F(diff("Age")<-6)~nodeocov.Age    ***
    ## F(diff("Age")<-6)~nodeicov.Age    ***
    ## F(absdiff("Age")<=6)~edges        ***
    ## F(absdiff("Age")<=6)~nodeocov.Age ***
    ## F(absdiff("Age")<=6)~nodeicov.Age ***
    ## offset(mutual)                    ***
    ## offset(edges)                     ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##      Null Deviance: 535465  on 386256  degrees of freedom
    ##  Residual Deviance: 349211  on 386241  degrees of freedom
    ##  
    ## AIC: 349237  BIC: 349379  (Smaller is better. MC Std. Err. = 40.52)
    ## 
    ##  The following terms are fixed by offset and are not estimated:
    ##   offset(mutual) offset(edges)

    plot(gof(fit.01))

![](project_files/figure-markdown_strict/unnamed-chunk-15-1.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-2.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-3.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-4.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-5.png)

    plot(gof(fit.02))

![](project_files/figure-markdown_strict/unnamed-chunk-15-6.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-7.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-8.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-9.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-10.png)

    plot(gof(fit.03))

![](project_files/figure-markdown_strict/unnamed-chunk-15-11.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-12.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-13.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-14.png)![](project_files/figure-markdown_strict/unnamed-chunk-15-15.png)
\#<a href="https://eehh-stanford.github.io/SNA-workshop/ergm-intro.html" class="uri">https://eehh-stanford.github.io/SNA-workshop/ergm-intro.html</a>

    anova(fit.02,fit.03)

    ## Analysis of Variance Table
    ## 
    ## Model 1: TARGET_STATS ~ nodematch("GENDER", diff = TRUE, levels = -1) + 
    ##     nodeifactor("GENDER") + nodeofactor("GENDER") + F(~edges + 
    ##     nodeocov("Age") + nodeicov("Age"), ~diff("Age") > 6) + F(~edges + 
    ##     nodeocov("Age") + nodeicov("Age"), ~diff("Age") < -6) + F(~edges + 
    ##     nodeocov("Age") + nodeicov("Age"), ~absdiff("Age") <= 6) + 
    ##     offset(edges)
    ## Model 2: TARGET_STATS ~ nodematch("GENDER", diff = TRUE, levels = -1) + 
    ##     nodeifactor("GENDER") + nodeofactor("GENDER") + mutual + 
    ##     F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") > 
    ##         6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") < 
    ##     -6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~absdiff("Age") <= 
    ##     6) + offset(mutual) + offset(edges)
    ##      Df Deviance Resid. Df Resid. Dev Pr(>|Chisq|)    
    ## NULL                386256     535465                 
    ## 1    12  4843425    386244   -4307961    < 2.2e-16 ***
    ## 2     1 -4657172    386243     349211    < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
