
library(ggplot2)
library(plotly)
library(plyr)

dic<-read.csv("C:/Users/dell/Desktop/st/S2/DM/prjt/dic.csv")

summer<-read.csv("C:/Users/dell/Desktop/st/S2/DM/prjt/s2.csv")
summary(summer)

medalcondition<-summer$Medal=="Gold" #basic filter
sm<-summer[medalcondition , ]


m2<-20
k2<-10  

##########################list of sports
sportlist<-count(sm,'Discipline')
#sportlisttt<-count(sm,'Sport')
sport<-as.character(sportlist$Discipline)
sport<-as.list(sport)        ##new
#sport


###############list of countries
countrylist<-count(sm,'Country') 

country<-as.array(countrylist$Country)

countryncol<-length(country)   ##


country<-country[2:countryncol] ##

country<-as.character(country)



countryncol<-length(country) 
n<-countryncol




#########################dataframes###############

globalncol<-length(sport)

global<- data.frame(matrix(nrow=m2,ncol=globalncol))  ## 1 evaluation
colnames(global)<-(sport)


country_eval<-data.frame(matrix(nrow=k2,ncol=countryncol))
#country<-as.array(countrylist$Country)
colnames(country_eval)<-(country) 

evaldf<-data.frame(matrix(nrow=countryncol,ncol=globalncol))

for(i in 1:countryncol){
  for (j in 1:globalncol){
    evaldf[i,j]<-0
  }
}

colnames(evaldf)<-(sport)

rownames(evaldf)<-(country)

v<-length(sport)


########################################data preparation######

for (i in 1:v){

cond<-sm$Sport==sport[i] 

sm2<-sm[cond ,]


best<-count(sm2,'Country')  ## count the best contries
#best2<-count(wn2,'Country')
colnames(best) <- c("country","nbr") ##makes it ready for plot
bestsort<-best[order(best$nbr,decreasing=TRUE),] ## order it 


topbest<-bestsort[1:m2,]

ggplot(topbest,aes(x=topbest$country ,y=topbest$nbr ))+ geom_bar(stat="identity")

ind<-as.array(topbest$country) ## get the names of countries
ind<-as.character(ind)
#score<-as.array(topbest$nbr)

#score
for (j in 1:n) {  ## iterate all countries
 
  
      for(m in 1:m2) {
         
         if (identical(ind[m],country[j])){#&& score[m]>=3) {## if that country(j) is good in a sport i
          
           evaldf[j,i]<-1
           write<-TRUE
          
           for(k in 1:k2){
             if (is.na(country_eval[k,j]) && write) {  ## find elpty spot
             write<-FALSE  
             country_eval[k,j]<-sport[i]
             
           }
           
         }
        }
    }
  
}  
global[,i]<-ind
}

###################cleaning data##################
vr<-c()

for(i in 1:countryncol){
  
    if(is.na(country_eval[1,i])){#=FALSE)){
      tmp<-as.character(i)
      vr<-c(vr,tmp)
    }
  }


vr<-as.double(vr)
country_eval2<-country_eval[,-vr]
country2<-country[-vr]
country2

new<-data.frame(matrix(nrow=280,ncol=2))
colnames(new)<-c("country","sport")

country

goodcountries<-length(country_eval2)
cond<-TRUE
i<-1

  for(j in 1:goodcountries){
  
    cond<-TRUE
    k<-1
    while(k<=10 && cond)  {
      new$sport[i]<-country_eval2[k,j]
      new$country[i]<-country2[j]
      i<-i+1
      k<-k+1 
      if(is.na(country_eval2[k,j])){
    
      cond<-FALSE } 
    
  }
}

######################################starting the work#############

library(arules)
library(arulesViz)
evalm<- split(x=new[,"sport"],f=new$country)  


evalm <- as(evalm,"transactions") ##transform it to  transactions

################# plot some itemFrequencies

itemFrequency(evalm)             ##get the frequency of each sport
win.graph(800,600,10)
itemFrequencyPlot(evalm,topN=10,type="absolute")    ##plot the frequency


###use apriori algo with rules of min sup=0.12  and min conf 0.5=

rules <- apriori(evalm, parameter = list(supp = 0.12, conf = 0.5))


options(digits=2)

summary(rules)

##########plot rules 

win.graph(800,600,10)    # plot the rules 
plot(rules) 

head(quality(rules))

win.graph(800,600,10)    ## if we want to see the rules with strong lift
plot(rules, measure = "support", shading = "lift")

plot(rules, measure=c("support", "lift"), shading="confidence")


########################matrix ########################################
subrules <- rules[quality(rules)$confidence > 0.7] ## filter the rules with confidence of 0.5
subrules ## shpw the rules 

################### 2D matrix with shading
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))



#######graph based rules        
subrules2 <- head(sort(rules, by="lift"), 20)  ## sort rules by lift and take the top 20 
plot(subrules2, method="graph")
win.graph(800,600,10)
plot(subrules2, method="graph", control=list(type="itemsets"))

#saveAsGraph(head(sort(rules, by="lift"),1000), file="rules.graphml")




###############################ecalt######################################
## for itemsets
ruleseclat <- eclat(evalm, parameter = list(support = 0.02))
plot(ruleseclat)

plot(ruleseclat, measure = "support")








win.graph(800,600,10)
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))
win.graph(800,600,10)
plot(itemsets, method="graph")


#subruleseclat <- head(sort(ruleseclat, by="lift"), 20)  ## sort rules by lift and take the top 20 






























