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

    for (i in 1:nrow(ucnet.alters)){
      if (ucnet.alters$C1C_NSAMEAGE[i]==2){
        #C1C_NSAMEAGE=yes
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

    #delete the egos and alters that have connection but do not have attribute
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

    nodeifactor.male<-nrow(in.row%>%filter(ego.gender==1)) + nrow(out.row%>%filter(GENDER==1))

    ##nodeofactor.male: the number of times a node with Gender-male appears as the origin node of a directed tie.

    nodeofactor.male<-nrow(in.row%>%filter(GENDER==1))+nrow(out.row%>%filter(ego.gender==1))


    #nodematch-Gender (not using yet)
    #homophily.in<-nrow(in.row%>%filter(in.row$GENDER==in.row$ego.gender))
    #homophily.out<-nrow(out.row%>%filter(out.row$GENDER==out.row$ego.gender))
    #nodematch<-nrow(as.data.frame(homophily.in$.egoID))+homophily.out


    #Reciprocal
    Reciprocal<-nrow(in.row%>%filter(in.row$outdegree==TRUE))

    #F(~edges + nodeocov("age") + nodeicov("age"), ~diff("age")>6)
    ##F~edges,the number of edges(i,j) for which attr(i,age)>=attr(j,age)
    f.in.row<-in.row%>%filter(Age=="older")
    f.out.row<-out.row%>%filter(Age=="younger")
    f.edges<-nrow(f.in.row)+nrow(f.out.row)

    ##F~nodeicov("age"), This term equal to the total value of attr(j,age) for all edges(i,j) in the network. However, we do not know the age of alters, We will only sum up the age attribute of egos.
    f.nodeicov<-sum(f.in.row$ego.age)
    ##F~nodeocov("age")
    f.nodeocov<-sum(f.out.row$ego.age)


    #F(~edges + nodeocov("age") + nodeicov("age"), ~diff("age")<-6)
    f.in.row.small<-in.row%>%filter(Age=="younger")
    f.out.row.small<-out.row%>%filter(Age=="older")
    f.edges.small<-nrow(f.in.row.small)+nrow(f.out.row.small)
    f.nodeicov.small<-sum(f.in.row.small$ego.age)
    f.nodeocov.small<-sum(f.out.row.small$ego.age)


    #F(~edges + nodeocov("age") + nodeicov("age"), ~~absdiff("Age")<=6)

    f.in.row.same<-in.row%>%filter(Age=="same")
    f.out.row.same<-out.row%>%filter(Age=="same")
    f.edges.same<-nrow(f.in.row.same)+nrow(f.out.row.same)
    f.nodeicov.same<-sum(f.in.row.same$ego.age)
    f.nodeocov.same<-sum(f.out.row.same$ego.age)

    fit.01<-ergm(ucnet.network~
                       edges+nodeifactor('GENDER')+nodeofactor('GENDER') 
                 
                      +offset(edges),
                     
                     offset.coef = c(-log(11039)),
                 
                     constraints = ~blocks(~Age%in%37:57, levels2=3),
                     
                     target.stats=c(uc.edges,nodeifactor.male,nodeofactor.male),
                     )

    ## Starting maximum pseudolikelihood estimation (MPLE):

    ## Evaluating the predictor and response matrix.

    ## Maximizing the pseudolikelihood.

    ## Finished MPLE.

    ## Starting Monte Carlo maximum likelihood estimation (MCMLE):

    ## Iteration 1 of at most 60:

    ## Optimizing with step length 0.5407.

    ## The log-likelihood improved by 2.0079.

    ## Estimating equations are not within tolerance region.

    ## Iteration 2 of at most 60:

    ## Optimizing with step length 1.0000.

    ## The log-likelihood improved by 1.7991.

    ## Estimating equations are not within tolerance region.

    ## Iteration 3 of at most 60:

    ## Optimizing with step length 1.0000.

    ## The log-likelihood improved by 0.0528.

    ## Convergence test p-value: 0.5128. Not converged with 99% confidence; increasing sample size.
    ## Iteration 4 of at most 60:
    ## Optimizing with step length 1.0000.
    ## The log-likelihood improved by 0.0071.
    ## Convergence test p-value: 0.0051. Converged with 99% confidence.
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
                          
                       nodeifactor('GENDER')+nodeofactor('GENDER') 
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")>6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")< -6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~absdiff("Age")<=6),
                 

                     constraints = ~blocks(~Age%in%37:57, levels2=3),
                     
                     control = control.ergm(MCMLE.maxit = 30),

                     target.stats=c(nodeifactor.male,nodeofactor.male,
                                    f.edges,f.nodeocov,f.nodeicov,
                                    f.edges.small,f.nodeocov.small,f.nodeicov.small,
                                    f.edges.same,f.nodeocov.same,f.nodeicov.same)
                     )

    ## Unable to match target stats. Using MCMLE estimation.

    ## Starting maximum pseudolikelihood estimation (MPLE):

    ## Evaluating the predictor and response matrix.

    ## Warning in mple.existence(pl): The MPLE does not exist!

    ## Maximizing the pseudolikelihood.

    ## Warning in ergm.mple(nw, fd, m, MPLEtype = MPLEtype, init = init, control =
    ## control, : glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Finished MPLE.

    ## Starting Monte Carlo maximum likelihood estimation (MCMLE):

    ## Iteration 1 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0027.

    ## The log-likelihood improved by 2.2056.

    ## Estimating equations are not within tolerance region.

    ## Iteration 2 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0023.

    ## The log-likelihood improved by 1.9849.

    ## Estimating equations are not within tolerance region.

    ## Iteration 3 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.7958.

    ## Estimating equations are not within tolerance region.

    ## Iteration 4 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.6045.

    ## Estimating equations are not within tolerance region.

    ## Iteration 5 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 1.9247.

    ## Estimating equations are not within tolerance region.

    ## Iteration 6 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.5185.

    ## Estimating equations are not within tolerance region.

    ## Iteration 7 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.5243.

    ## Estimating equations are not within tolerance region.

    ## Iteration 8 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 2.4005.

    ## Estimating equations are not within tolerance region.

    ## Iteration 9 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 1.3936.

    ## Estimating equations are not within tolerance region.

    ## Iteration 10 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 1.6940.

    ## Estimating equations are not within tolerance region.

    ## Iteration 11 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 2.3687.

    ## Estimating equations are not within tolerance region.

    ## Iteration 12 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 1.7129.

    ## Estimating equations are not within tolerance region.

    ## Iteration 13 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.9212.

    ## Estimating equations are not within tolerance region.

    ## Iteration 14 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 2.5257.

    ## Estimating equations are not within tolerance region.

    ## Iteration 15 of at most 30:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.8185.

    ## Estimating equations are not within tolerance region.

    ## Iteration 16 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.6497.

    ## Estimating equations are not within tolerance region.

    ## Iteration 17 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.2800.

    ## Estimating equations are not within tolerance region.

    ## Iteration 18 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 2.4736.

    ## Estimating equations are not within tolerance region.

    ## Iteration 19 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.5847.

    ## Estimating equations are not within tolerance region.

    ## Iteration 20 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.8350.

    ## Estimating equations are not within tolerance region.

    ## Iteration 21 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.8044.

    ## Estimating equations are not within tolerance region.

    ## Iteration 22 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.5468.

    ## Estimating equations are not within tolerance region.

    ## Iteration 23 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.6196.

    ## Estimating equations are not within tolerance region.

    ## Iteration 24 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.0239.

    ## Estimating equations are not within tolerance region.

    ## Iteration 25 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 1.6753.

    ## Estimating equations are not within tolerance region.

    ## Iteration 26 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.3799.

    ## Estimating equations are not within tolerance region.

    ## Iteration 27 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0005.

    ## The log-likelihood improved by 2.0114.

    ## Estimating equations are not within tolerance region.

    ## Iteration 28 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0005.

    ## The log-likelihood improved by 1.8069.

    ## Estimating equations are not within tolerance region.

    ## Iteration 29 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0004.

    ## The log-likelihood improved by 1.4798.

    ## Estimating equations are not within tolerance region.

    ## Iteration 30 of at most 30:

    ## Warning: Model statistics 'F(absdiff("Age")<=6)~nodeicov.Age' are linear
    ## combinations of some set of preceding statistics at the current stage of the
    ## estimation. This may indicate that the model is nonidentifiable.

    ## Optimizing with step length 0.0005.

    ## The log-likelihood improved by 1.9738.

    ## Estimating equations are not within tolerance region.

    ## MCMLE estimation did not converge after 30 iterations. The estimated coefficients may not be accurate. Estimation may be resumed by passing the coefficients as initial values; see 'init' under ?control.ergm for details.

    ## Finished MCMLE.

    ## Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
    ## Bridging between the dyad-independent submodel and the full model...
    ## Setting up bridge sampling...
    ## Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
    ## Bridging finished.
    ## This model was fit using MCMC.  To examine model diagnostics and check
    ## for degeneracy, use the mcmc.diagnostics() function.

    fit.02.change<-ergm(ucnet.network~
                          
                       nodeifactor('GENDER')+nodeofactor('GENDER') 
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")>6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")< -6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~absdiff("Age")<=6)
                     +offset(F(~ edges, ~diff("Age")>6))
                     +offset(F(~ edges, ~diff("Age")< -6))
                     +offset(F(~ edges, ~absdiff("Age")<=6)) ,
                   
                     offset.coef = c(-log(11039),-log(11039),-log(11039)),
                     
                     constraints = ~blocks(~Age%in%37:57, levels2=3),
                     
                     control = control.ergm(MCMLE.maxit = 30),

                     target.stats=c(nodeifactor.male,nodeofactor.male,
                                    f.edges,f.nodeocov,f.nodeicov,
                                    f.edges.small,f.nodeocov.small,f.nodeicov.small,
                                    f.edges.same,f.nodeocov.same,f.nodeicov.same)
                     )

    ## Unable to match target stats. Using MCMLE estimation.

    ## Starting maximum pseudolikelihood estimation (MPLE):

    ## Evaluating the predictor and response matrix.

    ## Maximizing the pseudolikelihood.

    ## Finished MPLE.

    ## Starting Monte Carlo maximum likelihood estimation (MCMLE):

    ## Iteration 1 of at most 30:

    ## Optimizing with step length 0.0023.

    ## The log-likelihood improved by 2.2263.

    ## Estimating equations are not within tolerance region.

    ## Iteration 2 of at most 30:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.4075.

    ## Estimating equations are not within tolerance region.

    ## Iteration 3 of at most 30:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.5637.

    ## Estimating equations are not within tolerance region.

    ## Iteration 4 of at most 30:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.7083.

    ## Estimating equations are not within tolerance region.

    ## Iteration 5 of at most 30:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.9097.

    ## Estimating equations are not within tolerance region.

    ## Iteration 6 of at most 30:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.9490.

    ## Estimating equations are not within tolerance region.

    ## Iteration 7 of at most 30:

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 2.0132.

    ## Estimating equations are not within tolerance region.

    ## Iteration 8 of at most 30:

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 1.7191.

    ## Estimating equations are not within tolerance region.

    ## Iteration 9 of at most 30:

    ## Optimizing with step length 0.0014.

    ## The log-likelihood improved by 2.1985.

    ## Estimating equations are not within tolerance region.

    ## Iteration 10 of at most 30:

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 3.7810.

    ## Estimating equations are not within tolerance region.

    ## Iteration 11 of at most 30:

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.4652.

    ## Estimating equations are not within tolerance region.

    ## Iteration 12 of at most 30:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.2722.

    ## Estimating equations are not within tolerance region.

    ## Iteration 13 of at most 30:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.5264.

    ## Estimating equations are not within tolerance region.

    ## Iteration 14 of at most 30:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.6566.

    ## Estimating equations are not within tolerance region.

    ## Iteration 15 of at most 30:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.6572.

    ## Estimating equations are not within tolerance region.

    ## Iteration 16 of at most 30:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.6456.

    ## Estimating equations are not within tolerance region.

    ## Iteration 17 of at most 30:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.1017.

    ## Estimating equations are not within tolerance region.

    ## Iteration 18 of at most 30:

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 2.6401.

    ## Estimating equations are not within tolerance region.

    ## Iteration 19 of at most 30:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.8625.

    ## Estimating equations are not within tolerance region.

    ## Iteration 20 of at most 30:

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.1628.

    ## Estimating equations are not within tolerance region.

    ## Iteration 21 of at most 30:

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 2.0544.

    ## Estimating equations are not within tolerance region.

    ## Iteration 22 of at most 30:

    ## Optimizing with step length 0.0005.

    ## The log-likelihood improved by 1.6989.

    ## Estimating equations are not within tolerance region.

    ## Iteration 23 of at most 30:

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 2.0140.

    ## Estimating equations are not within tolerance region.

    ## Iteration 24 of at most 30:

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 2.0779.

    ## Estimating equations are not within tolerance region.

    ## Iteration 25 of at most 30:

    ## Optimizing with step length 0.0005.

    ## The log-likelihood improved by 2.1722.

    ## Estimating equations are not within tolerance region.

    ## Iteration 26 of at most 30:

    ## Optimizing with step length 0.0004.

    ## The log-likelihood improved by 1.4735.

    ## Estimating equations are not within tolerance region.

    ## Iteration 27 of at most 30:

    ## Optimizing with step length 0.0003.

    ## The log-likelihood improved by 1.9868.

    ## Estimating equations are not within tolerance region.

    ## Iteration 28 of at most 30:

    ## Optimizing with step length 0.0003.

    ## The log-likelihood improved by 1.5819.

    ## Estimating equations are not within tolerance region.

    ## Iteration 29 of at most 30:

    ## Optimizing with step length 0.0003.

    ## The log-likelihood improved by 1.7715.

    ## Estimating equations are not within tolerance region.

    ## Iteration 30 of at most 30:

    ## Optimizing with step length 0.0002.

    ## The log-likelihood improved by 1.2855.

    ## Estimating equations are not within tolerance region.

    ## MCMLE estimation did not converge after 30 iterations. The estimated coefficients may not be accurate. Estimation may be resumed by passing the coefficients as initial values; see 'init' under ?control.ergm for details.

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
                      nodeifactor('GENDER')+nodeofactor('GENDER') +mutual

                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")>6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~diff("Age")< -6)
                     
                     +F(~ edges+nodeocov("Age") + nodeicov("Age"), ~absdiff("Age")<=6)
                     
                     + offset(mutual),
                     
                     offset.coef = c(log(11039)),
                 
                     control = control.ergm(MCMLE.maxit = 30),
                 
                     constraints = ~blocks(~Age%in%37:57, levels2=3),

                     target.stats=c(nodeifactor.male,nodeofactor.male,Reciprocal,
                                    f.edges,f.nodeocov,f.nodeicov,
                                    f.edges.small,f.nodeocov.small,f.nodeicov.small,
                                    f.edges.same,f.nodeocov.same,f.nodeicov.same)
                     )

    ## Unable to match target stats. Using MCMLE estimation.

    ## Starting maximum pseudolikelihood estimation (MPLE):

    ## Evaluating the predictor and response matrix.

    ## Warning in mple.existence(pl): The MPLE does not exist!

    ## Maximizing the pseudolikelihood.

    ## Finished MPLE.

    ## Starting Monte Carlo maximum likelihood estimation (MCMLE):

    ## Iteration 1 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0023.

    ## The log-likelihood improved by 2.4418.

    ## Estimating equations are not within tolerance region.

    ## Iteration 2 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0023.

    ## The log-likelihood improved by 2.5299.

    ## Estimating equations are not within tolerance region.

    ## Iteration 3 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0019.

    ## The log-likelihood improved by 2.2211.

    ## Estimating equations are not within tolerance region.

    ## Iteration 4 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.9917.

    ## Estimating equations are not within tolerance region.

    ## Iteration 5 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 1.8928.

    ## Estimating equations are not within tolerance region.

    ## Iteration 6 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 2.1675.

    ## Estimating equations are not within tolerance region.

    ## Iteration 7 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0016.

    ## The log-likelihood improved by 2.3931.

    ## Estimating equations are not within tolerance region.

    ## Iteration 8 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 1.5204.

    ## Estimating equations are not within tolerance region.

    ## Iteration 9 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 1.6435.

    ## Estimating equations are not within tolerance region.

    ## Iteration 10 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 2.1738.

    ## Estimating equations are not within tolerance region.

    ## Iteration 11 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0012.

    ## The log-likelihood improved by 2.0489.

    ## Estimating equations are not within tolerance region.

    ## Iteration 12 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 2.2398.

    ## Estimating equations are not within tolerance region.

    ## Iteration 13 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 1.7366.

    ## Estimating equations are not within tolerance region.

    ## Iteration 14 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0010.

    ## The log-likelihood improved by 2.2365.

    ## Estimating equations are not within tolerance region.

    ## Iteration 15 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.6577.

    ## Estimating equations are not within tolerance region.

    ## Iteration 16 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 1.6754.

    ## Estimating equations are not within tolerance region.

    ## Iteration 17 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.6782.

    ## Estimating equations are not within tolerance region.

    ## Iteration 18 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 2.0386.

    ## Estimating equations are not within tolerance region.

    ## Iteration 19 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 1.6287.

    ## Estimating equations are not within tolerance region.

    ## Iteration 20 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0008.

    ## The log-likelihood improved by 3.0817.

    ## Estimating equations are not within tolerance region.

    ## Iteration 21 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.0573.

    ## Estimating equations are not within tolerance region.

    ## Iteration 22 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0007.

    ## The log-likelihood improved by 2.6433.

    ## Estimating equations are not within tolerance region.

    ## Iteration 23 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 1.9881.

    ## Estimating equations are not within tolerance region.

    ## Iteration 24 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 2.2025.

    ## Estimating equations are not within tolerance region.

    ## Iteration 25 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 2.7556.

    ## Estimating equations are not within tolerance region.

    ## Iteration 26 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0004.

    ## The log-likelihood improved by 1.5908.

    ## Estimating equations are not within tolerance region.

    ## Iteration 27 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0006.

    ## The log-likelihood improved by 2.9646.

    ## Estimating equations are not within tolerance region.

    ## Iteration 28 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0004.

    ## The log-likelihood improved by 1.9361.

    ## Estimating equations are not within tolerance region.

    ## Iteration 29 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0003.

    ## The log-likelihood improved by 1.4772.

    ## Estimating equations are not within tolerance region.

    ## Iteration 30 of at most 30:

    ## Model statistics 'F(absdiff("Age")<=6)~edges', 'F(absdiff("Age")<=6)~nodeocov.Age', and 'F(absdiff("Age")<=6)~nodeicov.Age' are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.

    ## Optimizing with step length 0.0003.

    ## The log-likelihood improved by 1.5238.

    ## Estimating equations are not within tolerance region.

    ## Estimating equations did not move closer to tolerance region more than 1 time(s) in 4 steps; increasing sample size.

    ## MCMLE estimation did not converge after 30 iterations. The estimated coefficients may not be accurate. Estimation may be resumed by passing the coefficients as initial values; see 'init' under ?control.ergm for details.

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
    ##     37:57, levels2 = 3), offset.coef = c(-log(11039)), target.stats = c(uc.edges, 
    ##     nodeifactor.male, nodeofactor.male))
    ## 
    ## Monte Carlo Maximum Likelihood Results:
    ## 
    ##                      Estimate Std. Error MCMC % z value Pr(>|z|)    
    ## edges                 6.62273    0.04358      0  151.97   <1e-04 ***
    ## nodeifactor.GENDER.2 -1.04955    0.05610      0  -18.71   <1e-04 ***
    ## nodeofactor.GENDER.2 -0.98669    0.05352      0  -18.43   <1e-04 ***
    ## offset(edges)        -9.30919    0.00000      0    -Inf   <1e-04 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##      Null Deviance:  107495  on 77541  degrees of freedom
    ##  Residual Deviance: -878647  on 77537  degrees of freedom
    ##  
    ## AIC: -878641  BIC: -878613  (Smaller is better. MC Std. Err. = 142.3)
    ## 
    ##  The following terms are fixed by offset and are not estimated:
    ##   offset(edges)

    summary(fit.02)

    ## Call:
    ## ergm(formula = ucnet.network ~ nodeifactor("GENDER") + nodeofactor("GENDER") + 
    ##     F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") > 
    ##         6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") < 
    ##     -6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~absdiff("Age") <= 
    ##     6), constraints = ~blocks(~Age %in% 37:57, levels2 = 3), 
    ##     target.stats = c(nodeifactor.male, nodeofactor.male, f.edges, 
    ##         f.nodeocov, f.nodeicov, f.edges.small, f.nodeocov.small, 
    ##         f.nodeicov.small, f.edges.same, f.nodeocov.same, f.nodeicov.same), 
    ##     control = control.ergm(MCMLE.maxit = 30))
    ## 
    ## Monte Carlo Maximum Likelihood Results:
    ## 
    ##                                     Estimate Std. Error MCMC % z value Pr(>|z|)
    ## nodeifactor.GENDER.2              -8.522e-01  7.809e-02      0 -10.913   <1e-04
    ## nodeofactor.GENDER.2              -1.076e+00  6.666e-02      0 -16.146   <1e-04
    ## F(diff("Age")>6)~edges             4.750e+00  1.838e-01      0  25.837   <1e-04
    ## F(diff("Age")>6)~nodeocov.Age     -2.340e-01  4.909e-03      0 -47.673   <1e-04
    ## F(diff("Age")>6)~nodeicov.Age      1.944e-01  4.835e-03      0  40.212   <1e-04
    ## F(diff("Age")<-6)~edges            3.120e+01  1.194e+00      0  26.132   <1e-04
    ## F(diff("Age")<-6)~nodeocov.Age     3.947e+00  1.578e-01      0  25.018   <1e-04
    ## F(diff("Age")<-6)~nodeicov.Age    -3.926e+00  1.572e-01      0 -24.975   <1e-04
    ## F(absdiff("Age")<=6)~edges        -1.371e+02  5.508e+03    100  -0.025    0.980
    ## F(absdiff("Age")<=6)~nodeocov.Age -8.674e+00  9.180e+02    100  -0.009    0.992
    ## F(absdiff("Age")<=6)~nodeicov.Age  9.836e+00  9.180e+02    100   0.011    0.991
    ##                                      
    ## nodeifactor.GENDER.2              ***
    ## nodeofactor.GENDER.2              ***
    ## F(diff("Age")>6)~edges            ***
    ## F(diff("Age")>6)~nodeocov.Age     ***
    ## F(diff("Age")>6)~nodeicov.Age     ***
    ## F(diff("Age")<-6)~edges           ***
    ## F(diff("Age")<-6)~nodeocov.Age    ***
    ## F(diff("Age")<-6)~nodeicov.Age    ***
    ## F(absdiff("Age")<=6)~edges           
    ## F(absdiff("Age")<=6)~nodeocov.Age    
    ## F(absdiff("Age")<=6)~nodeicov.Age    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##      Null Deviance:  107495  on 77541  degrees of freedom
    ##  Residual Deviance: -287839  on 77530  degrees of freedom
    ##  
    ## AIC: -287817  BIC: -287716  (Smaller is better. MC Std. Err. = 10.06)

    summary(fit.02.change)

    ## Call:
    ## ergm(formula = ucnet.network ~ nodeifactor("GENDER") + nodeofactor("GENDER") + 
    ##     F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") > 
    ##         6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") < 
    ##     -6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~absdiff("Age") <= 
    ##     6) + offset(F(~edges, ~diff("Age") > 6)) + offset(F(~edges, 
    ##     ~diff("Age") < -6)) + offset(F(~edges, ~absdiff("Age") <= 
    ##     6)), constraints = ~blocks(~Age %in% 37:57, levels2 = 3), 
    ##     offset.coef = c(-log(11039), -log(11039), -log(11039)), target.stats = c(nodeifactor.male, 
    ##         nodeofactor.male, f.edges, f.nodeocov, f.nodeicov, f.edges.small, 
    ##         f.nodeocov.small, f.nodeicov.small, f.edges.same, f.nodeocov.same, 
    ##         f.nodeicov.same), control = control.ergm(MCMLE.maxit = 30))
    ## 
    ## Monte Carlo Maximum Likelihood Results:
    ## 
    ##                                     Estimate Std. Error MCMC % z value Pr(>|z|)
    ## nodeifactor.GENDER.2               -1.190415   0.067026      1 -17.761   <1e-04
    ## nodeofactor.GENDER.2               -1.045211   0.067573      0 -15.468   <1e-04
    ## F(diff("Age")>6)~edges             13.933853   0.155122      2  89.825   <1e-04
    ## F(diff("Age")>6)~nodeocov.Age      -0.264061   0.006807      1 -38.792   <1e-04
    ## F(diff("Age")>6)~nodeicov.Age       0.227355   0.006898      0  32.960   <1e-04
    ## F(diff("Age")<-6)~edges            51.749342   2.256664      1  22.932   <1e-04
    ## F(diff("Age")<-6)~nodeocov.Age      4.915775   0.263825      1  18.633   <1e-04
    ## F(diff("Age")<-6)~nodeicov.Age     -4.991575   0.268974      1 -18.558   <1e-04
    ## F(absdiff("Age")<=6)~edges          6.925466   0.128098      0  54.064   <1e-04
    ## F(absdiff("Age")<=6)~nodeocov.Age   0.004164   0.014305      0   0.291    0.771
    ## F(absdiff("Age")<=6)~nodeicov.Age  -0.022601   0.014117      0  -1.601    0.109
    ## offset(F(diff("Age")>6)~edges)     -9.309190   0.000000      0    -Inf   <1e-04
    ## offset(F(diff("Age")<-6)~edges)    -9.309190   0.000000      0    -Inf   <1e-04
    ## offset(F(absdiff("Age")<=6)~edges) -9.309190   0.000000      0    -Inf   <1e-04
    ##                                       
    ## nodeifactor.GENDER.2               ***
    ## nodeofactor.GENDER.2               ***
    ## F(diff("Age")>6)~edges             ***
    ## F(diff("Age")>6)~nodeocov.Age      ***
    ## F(diff("Age")>6)~nodeicov.Age      ***
    ## F(diff("Age")<-6)~edges            ***
    ## F(diff("Age")<-6)~nodeocov.Age     ***
    ## F(diff("Age")<-6)~nodeicov.Age     ***
    ## F(absdiff("Age")<=6)~edges         ***
    ## F(absdiff("Age")<=6)~nodeocov.Age     
    ## F(absdiff("Age")<=6)~nodeicov.Age     
    ## offset(F(diff("Age")>6)~edges)     ***
    ## offset(F(diff("Age")<-6)~edges)    ***
    ## offset(F(absdiff("Age")<=6)~edges) ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##      Null Deviance:   107495  on 77541  degrees of freedom
    ##  Residual Deviance: -1170030  on 77527  degrees of freedom
    ##  
    ## AIC: -1170008  BIC: -1169906  (Smaller is better. MC Std. Err. = 81.08)
    ## 
    ##  The following terms are fixed by offset and are not estimated:
    ##   offset(F(diff("Age")>6)~edges) offset(F(diff("Age")<-6)~edges) offset(F(absdiff("Age")<=6)~edges)

    summary(fit.03)

    ## Call:
    ## ergm(formula = ucnet.network ~ nodeifactor("GENDER") + nodeofactor("GENDER") + 
    ##     mutual + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") > 
    ##     6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~diff("Age") < 
    ##     -6) + F(~edges + nodeocov("Age") + nodeicov("Age"), ~absdiff("Age") <= 
    ##     6) + offset(mutual), constraints = ~blocks(~Age %in% 37:57, 
    ##     levels2 = 3), offset.coef = c(log(11039)), target.stats = c(nodeifactor.male, 
    ##     nodeofactor.male, Reciprocal, f.edges, f.nodeocov, f.nodeicov, 
    ##     f.edges.small, f.nodeocov.small, f.nodeicov.small, f.edges.same, 
    ##     f.nodeocov.same, f.nodeicov.same), control = control.ergm(MCMLE.maxit = 30))
    ## 
    ## Monte Carlo Maximum Likelihood Results:
    ## 
    ##                                     Estimate Std. Error MCMC % z value Pr(>|z|)
    ## nodeifactor.GENDER.2               -1.262169   0.072098      0  -17.51   <1e-04
    ## nodeofactor.GENDER.2               -1.039350   0.073147      0  -14.21   <1e-04
    ## mutual                            -11.027608   0.142898      0  -77.17   <1e-04
    ## F(diff("Age")>6)~edges              6.512507   0.291584      0   22.34   <1e-04
    ## F(diff("Age")>6)~nodeocov.Age      -0.263328   0.008261      0  -31.88   <1e-04
    ## F(diff("Age")>6)~nodeicov.Age       0.202744   0.006572      0   30.85   <1e-04
    ## F(diff("Age")<-6)~edges            43.183496   1.930643      0   22.37   <1e-04
    ## F(diff("Age")<-6)~nodeocov.Age      4.848191   0.236767      0   20.48   <1e-04
    ## F(diff("Age")<-6)~nodeicov.Age     -4.929279   0.239061      0  -20.62   <1e-04
    ## F(absdiff("Age")<=6)~edges        -22.015283         NA     NA      NA       NA
    ## F(absdiff("Age")<=6)~nodeocov.Age   0.012282         NA     NA      NA       NA
    ## F(absdiff("Age")<=6)~nodeicov.Age   0.030096         NA     NA      NA       NA
    ## offset(mutual)                      9.309190   0.000000      0     Inf   <1e-04
    ##                                      
    ## nodeifactor.GENDER.2              ***
    ## nodeofactor.GENDER.2              ***
    ## mutual                            ***
    ## F(diff("Age")>6)~edges            ***
    ## F(diff("Age")>6)~nodeocov.Age     ***
    ## F(diff("Age")>6)~nodeicov.Age     ***
    ## F(diff("Age")<-6)~edges           ***
    ## F(diff("Age")<-6)~nodeocov.Age    ***
    ## F(diff("Age")<-6)~nodeicov.Age    ***
    ## F(absdiff("Age")<=6)~edges           
    ## F(absdiff("Age")<=6)~nodeocov.Age    
    ## F(absdiff("Age")<=6)~nodeicov.Age    
    ## offset(mutual)                    ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##      Null Deviance:  107495  on 77541  degrees of freedom
    ##  Residual Deviance: -363338  on 77528  degrees of freedom
    ##  
    ## AIC: -363314  BIC: -363202  (Smaller is better. MC Std. Err. = 26.91)
    ## 
    ##  The following terms are fixed by offset and are not estimated:
    ##   offset(mutual)

    plot(gof(fit.01))

![](project_files/figure-markdown_strict/unnamed-chunk-17-1.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-2.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-3.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-4.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-5.png)

    plot(gof(fit.02))

![](project_files/figure-markdown_strict/unnamed-chunk-17-6.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-7.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-8.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-9.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-10.png)

    plot(gof(fit.02.change))

![](project_files/figure-markdown_strict/unnamed-chunk-17-11.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-12.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-13.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-14.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-15.png)

    plot(gof(fit.03))

![](project_files/figure-markdown_strict/unnamed-chunk-17-16.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-17.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-18.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-19.png)![](project_files/figure-markdown_strict/unnamed-chunk-17-20.png)
\#<a href="https://eehh-stanford.github.io/SNA-workshop/ergm-intro.html" class="uri">https://eehh-stanford.github.io/SNA-workshop/ergm-intro.html</a>
