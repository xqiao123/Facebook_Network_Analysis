library(bootnet)
library(psych)
library(qgraph)
library(btergm)
library(linkprediction)
library(igraph) #--Basic creation and handling of "graph" objects--#
library(network) #--Basic creation and handling of "network" objects--#
library(intergraph) #--To switch back and forth between "graph" and "network" objects--#
library(statnet) #--For basic network plots like gplots--#
library(networkD3) #--To create interactive visuals--#
library(visNetwork) #--To create interactive visuals--#
library(networkDynamic) #--To analyse networks that evolve over time--#
library(tsna) #--Another package to analyse networks that evolve over time--#
library(Rcpp)
library(RColorBrewer)
library(multinet) #To handle multi-layer networks---#
library(janitor) #getting number of values in one category
library(blockmodeling)
library(FactoMineR)
library(xUCINET)

library(VIM) #for KNN imputation


fb=Facebook #igraph
summary(asNetwork(fb))


###Dataset
##Convert an igraph to an edgelist and a dataframe
fb.edges <- as.data.frame(get.edgelist(fb))
fb.edges

fb.df <- as_data_frame(fb, what='vertices')[,1:6]
fb.df$friend_count=as.integer(fb.df$friend_count)
fb.df$mutual_friend_count=as.integer(fb.df$mutual_friend_count)
fb.df$group[fb.df$group=='B']='Book Club'
fb.df$group[fb.df$group=='C']='College'
fb.df$group[fb.df$group=='F']='Family'
fb.df$group[fb.df$group=='G']='Graduate School'
fb.df$group[fb.df$group=='H']='High School'
fb.df$group[fb.df$group=='M']='Music'
fb.df$group[fb.df$group=='S']='Spiel'
fb.df$group[fb.df$group=='W']='Work'

#Use KNN to impute missing values
fb.df.2=data.frame(fb.df$name,fb.df$group, fb.df$sex, fb.df$relationship_status, fb.df$friend_count, fb.df$mutual_friend_count)
sum(is.na(fb.df.2)) #4 missing values in column 'fb.df.friend_count'

fb.df.3=kNN(fb.df.2, k=6, variable='fb.df.friend_count')
fb.df.4=fb.df.3[-7]
fb.df.4
fb.graph=graph.data.frame(fb.edges,directed = "FALSE",vertices = fb.df.4) 


###Descriptive Analysis
##Descriptive Plot
plot(fb.graph, labels=TRUE)

##Create a Network
fb.network=asNetwork(fb.graph)

##five summaries
#---The five number summary for the full network---#
network.size(fb.network)  #size: 93 nodes
gden(fb.network) #density: 0.076
components(fb.network) #components: 10
max(geodist(component.largest(fb.network,result = "graph"))$gdist) #diameter: 3, this diameter is the longest path in the biggest component
gtrans(fb.network,mode="graph") #clustering coefficient: 0.666

#degree distribution
fit_power_law(degree(fb.network,gmode = "graph"))
hist(degree(fb.network,gmode = "graph"),freq = F,main = "Degree distibution, Facebook (full)",xlab="Degree")
mean(degree(fb.network,gmode = "graph")) #6.95
#p-value: 1
#alpha: 6.26
#xmin: 16
#the power law assumption isn't met because the xmin is 16, there are few data after 16


##Polarization
for (i in seq(1,6)) {
  ra=assortativity.nominal(fb.graph, as.integer(as.factor(fb.df.4[,i])),directed = FALSE)
  print(i)
  print(ra)
}
#The feature 'group' has the biggest ra, which is 0.913

##Community Detection
#Use group first
modularity(fb.graph,as.integer(as.factor(V(fb.graph)$fb.df.group))) #overall modularity is 0.614

group.colrs <- c("red", "green",'black','pink','blue','orange','yellow','purple')
V(fb.graph)$fb.df.group.colour <- group.colrs[as.integer(as.factor(V(fb.graph)$fb.df.group))]
plot(fb.graph,vertex.color=V(fb.graph)$fb.df.group.colour, main='Friendship Network, Community Detection by Group')
legend(x = "bottomleft", legend = c("Book Club ", "College ", "Family ", "Graduate School ", "High School ", "Music ", "Spiel ", "Work "), col = group.colrs, pch = 19,pt.cex = 1.2, bty = "n")

#Automatic Methods
edge_betweenness(fb.graph, e=E(fb.graph),directed = FALSE)#--which edge got what score--#
which(edge_betweenness(fb.graph, e=E(fb.graph), directed = FALSE)==max(edge_betweenness(fb.graph,e=E(fb.graph),directed = FALSE))) #index: 303
E(fb.graph) [303] #SE--AC 

#--Okay, now, automatic, non-visual clustering--#
ceb=cluster_edge_betweenness(fb.graph,directed = FALSE) #--the method--#
membership(ceb) #--who falls where--#
modularity(ceb) #0.63
plot(ceb, fb.graph) #--the graph--#

#----Compare Multiple Community Methods---#
#--Okay, now, automatic, non-visual clustering--#
ceb=cluster_edge_betweenness(fb.graph,directed = FALSE) #--the method--#
membership(ceb) #--who falls where--#
modularity(ceb) #--the modularity score for this split--#
plot(ceb,fb.graph) #--the graph--#

cle=cluster_leading_eigen(fb.graph)
membership(cle)
modularity(cle)
plot(cle,fb.graph)

cfg=cluster_fast_greedy(fb.graph)
membership(cfg)
modularity(cfg)
plot(cfg,fb.graph)

cl=cluster_louvain(fb.graph)
membership(cl)
modularity(cl)
plot(cl,fb.graph)

ceb=cluster_edge_betweenness(fb.graph,directed = FALSE)
membership(ceb)
modularity(ceb)
plot(ceb,fb.graph)

cw=cluster_walktrap(fb.graph)
membership(cw)
modularity(cw)
plot(cw,fb.graph)

clp=cluster_label_prop(fb.graph)
membership(clp)
modularity(clp)
plot(clp,fb.graph)

cim=cluster_infomap(fb.graph)
membership(cim)
modularity(cim)
plot(cim,fb.graph)

co=cluster_optimal(fb.graph)
membership(co)
modularity(co)
plot(co,fb.graph)

#############
par(mfrow=c(3,3))
plot(ceb,fb.graph)
plot(cle,fb.graph)
plot(cfg,fb.graph)
plot(cl,fb.graph)
plot(cw,fb.graph)
plot(clp,fb.graph)
plot(cim,fb.graph)
plot(co,fb.graph)

########################
methods=list(ceb,cle,cfg,cl,cw,clp,cim,co)
m=matrix(0,length(methods),length(methods))
for(i in 1:length(methods))
{
  for(j in 1:length(methods))
  {
    m[i,j]=compare(methods[[i]],methods[[j]],method = "adjusted.rand")
  }
}
m #it seems each community method is close to each other, because the rand index is big
rownames(m)<-c("CEB","CLE","CFG","CL","CW","CLP","CIM","CO")
colnames(m)<-c("CEB","CLE","CFG","CL","CW","CLP","CIM","CO")

heatmap(m)

##Degree Centrality: Who is most popular
degree(fb.network, gmode='graph')
which(degree(fb.network, gmode='graph')==max(degree(fb.network, gmode='graph')))
max(degree(fb.network, gmode='graph'))
get.vertex.attribute(fb.network, "vertex.names")[1] #SE has 32 immediate friends

##Closeness Centrality: Who is nearest, on average, to the rest of the crowd.
xClosenessCentrality(as.matrix(get.adjacency(fb.graph)))
which(xClosenessCentrality(as.matrix(get.adjacency(fb.graph)))[,1]==min(xClosenessCentrality(as.matrix(get.adjacency(fb)))[,1]))
#The user named SE is nearest, on average to the rest of the crowd， because it has the lowest FreemanCloseness


##MCA
NodeName=vertex.attributes(fb.graph)$name
CorrespondenceMatrix=matrix(0,length(NodeName),6)
CorrespondenceMatrix[,1]=xDegreeCentrality(as.matrix(get.adjacency(fb.graph)))[,2]
CorrespondenceMatrix[,2]=xBetweennessCentrality(as.matrix(get.adjacency(fb.graph)))[,2]
CorrespondenceMatrix[,3]=xEigenvectorCentrality(as.matrix(get.adjacency(fb.graph)))[,2]
CorrespondenceMatrix[,4]=xClosenessCentrality(as.matrix(get.adjacency(fb.graph)))[,2]
CorrespondenceMatrix[,5]=xClosenessCentrality(as.matrix(get.adjacency(fb.graph)))[,4]
CorrespondenceMatrix[,6]=xClosenessCentrality(as.matrix(get.adjacency(fb.graph)))[,6]
rownames(CorrespondenceMatrix)=NodeName
colnames(CorrespondenceMatrix)=c("Degree","BetweenNess","EigenVector","FreemanCloseness","ReciprocalCloseness","ValenteCloseness")

xCorrespondenceAnalysis(CorrespondenceMatrix) 
#Dimension 1 explained about 68.9% of variation
#The user named SE can be found take a higher loading on dimension 1, overall he is the important node


###Link Prediction for the biggest component in the network
#--Common neighbors--#
strong_graph=decompose(fb.graph, mode = c("strong"), max.comps = NA, min.vertices = 0)[[1]]

#--So what's my "training" set?---#
#--#
summary(strong_graph) #47 nodes and 227 edges
kept.edges=sample(seq(1,length(E(strong_graph)),1),170,replace = FALSE) #75% of all edges in the biggest component
kept.edges

strong.edges=seq(1,length(E(strong_graph)),1)
strong.edges
other.edges=strong.edges[-kept.edges] #25% of all edges in the biggest component
other.edges #as test set

Test.graph=subgraph.edges(strong_graph,eids=other.edges,delete.vertices = FALSE)
Training.graph=subgraph.edges(strong_graph,eids=kept.edges,delete.vertices = FALSE)
summary(Training.graph)
summary(Test.graph) #47 vertices (from test edgelist), so there are supposed to be 1081 edges, but only 57 edges in my test set
#Positive (P): 57, Negative (N): 1081-57=1014

#--Let's implement CN on the training set--#
#--proxfun needs a connected graph. If yours become unconnected because of the training-testing split, just resample--#
cn.strong.scores=proxfun(strong_graph, method="cn", value="edgelist")
write.csv(cn.strong.scores, "/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/cn_strong_scores") #used to determine the threshold

cn.scores=proxfun(Training.graph, method="cn", value="edgelist")
write.csv(cn.scores, "/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/cn_scores")
#my remaining code for CN is in python


#--Next, preferential attachment on THE SAME TEST SET!!--#
pa.strong.scores=proxfun(strong_graph, method="pa", value="edgelist")
write.csv(pa.strong.scores, "/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/pa_strong_scores")

pa.scores=proxfun(Training.graph, method="pa", value="edgelist")
write.csv(pa.scores, "/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/pa_scores")
#Remaining codes are in Python


#--Next, Jaccard, on THE SAME TEST SET!!--#
jc.strong.scores=proxfun(strong_graph, method="jaccard", value="edgelist")
write.csv(jc.strong.scores, "/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/jc_strong_scores")

jc.scores=proxfun(Training.graph, method="jaccard", value="edgelist")
write.csv(jc.scores, "/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/jc_scores")
#Remaining codes are in Python
#Summary: Based on my ROC plot that has the percentile from 0 to 100, CM is the best


##ERGM model
#---A basic, null model: serves as a benchmark (when we do not even want to look into the properties of the nodes)--#
model0 = ergm(fb.network~edges)
summary(model0)
#AIC: 2292  BIC: 2298

model1=ergm(fb.network~edges+nodematch("fb.df.group", diff=TRUE)) 
summary(model1)
#AIC: 1080  BIC: 1137

model2=ergm(fb.network~edges+nodematch("fb.df.group", diff=TRUE)+nodefactor('fb.df.relationship_status')) 
summary(model2)
#AIC: 1076  BIC: 1153

model3=ergm(fb.network~edges+nodematch("fb.df.group", diff=TRUE)+nodefactor('fb.df.relationship_status')+nodecov('fb.df.mutual_friend_count')) 
summary(model3)
#AIC: 838.2  BIC: 920.8

model4=ergm(fb.network~edges+nodematch("fb.df.group", diff=TRUE)+nodefactor('fb.df.relationship_status')+nodecov('fb.df.mutual_friend_count')+absdiff('fb.df.friend_count')) 
summary(model4)
#AIC: 835.4  BIC: 924.5

model5=ergm(fb.network~edges+nodematch("fb.df.group", diff=TRUE)+nodecov('fb.df.mutual_friend_count')+absdiff('fb.df.friend_count')) 
summary(model5)
#AIC: 829.6  BIC: 899.6
#In summary, model 5 is my final model


###SIR simulation
sm=sir(fb.graph,beta = 5,gamma = 1)
plot(sm)

plot(
  sm,
  comp = c("NI"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = NULL,
  ylim = NULL,
  xlab = "Time")

sm[[1]] #--that's simulation 1--#
sm[[2]] #--that's simulation 2--#


#---Let's see whether the parameters make sense--#
par(mfrow=c(3,3))
sm51=sir(fb.graph,beta = 5,gamma = 1)

plot(
  sm51,
  comp = c("NS"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Susceptible (beta=5, gamma=1)")
plot(
  sm51,
  comp = c("NI"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Infected (beta=5, gamma=1)")
plot(
  sm51,
  comp = c("NR"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Recovered (beta=5, gamma=1)")

sm1005=sir(fb.graph,beta = 10,gamma = 0.5)

plot(
  sm1005,
  comp = c("NS"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Susceptible (beta=10, gamma=0.5)")
plot(
  sm1005,
  comp = c("NI"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Infected (beta=10, gamma=0.5)")
plot(
  sm1005,
  comp = c("NR"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Recovered (beta=10, gamma=0.5)")

sm20025=sir(fb.graph,beta = 20,gamma = 0.25)

plot(
  sm20025,
  comp = c("NS"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Susceptible (beta=20, gamma=0.25)")
plot(
  sm20025,
  comp = c("NI"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Infected (beta=20, gamma=0.25)")
plot(
  sm20025,
  comp = c("NR"),
  median = TRUE,
  quantiles = c(0.1, 0.9),
  color = NULL,
  median_color = c("red"),
  quantile_color = NULL,
  lwd.median = 2,
  lwd.quantile = 2,
  lty.quantile = 3,
  xlim = c(0,10),
  ylim = NULL,
  xlab = "Time", main="Recovered (beta=20, gamma=0.25)")


###Model Diagnose
gof.model0.detailed <- btergm::gof(model0, nsim = 50, statistics = c(edgebetweenness.modularity,
                                                                     walktrap.modularity,
                                                                     esp,
                                                                     nsp,
                                                                     deg,
                                                                     geodesic,
                                                                     triad.undirected,
                                                                     walktrap.roc,
                                                                     walktrap.pr,
                                                                     edgebetweenness.roc,
                                                                     edgebetweenness.pr,
                                                                     rocpr))
gof.model1.detailed <- btergm::gof(model1, nsim = 50, statistics = c(edgebetweenness.modularity,
                                                                     walktrap.modularity,
                                                                     esp,
                                                                     nsp,
                                                                     deg,
                                                                     geodesic,
                                                                     triad.undirected,
                                                                     walktrap.roc,
                                                                     walktrap.pr,
                                                                     edgebetweenness.roc,
                                                                     edgebetweenness.pr,
                                                                     rocpr))

gof.model2.detailed <- btergm::gof(model2, nsim = 50, statistics = c(edgebetweenness.modularity,
                                                                     walktrap.modularity,
                                                                     esp,
                                                                     nsp,
                                                                     deg,
                                                                     geodesic,
                                                                     triad.undirected,
                                                                     walktrap.roc,
                                                                     walktrap.pr,
                                                                     edgebetweenness.roc,
                                                                     edgebetweenness.pr,
                                                                     rocpr))

gof.model3.detailed <- btergm::gof(model3, nsim = 50, statistics = c(edgebetweenness.modularity,
                                                                     walktrap.modularity,
                                                                     esp,
                                                                     nsp,
                                                                     deg,
                                                                     geodesic,
                                                                     triad.undirected,
                                                                     walktrap.roc,
                                                                     walktrap.pr,
                                                                     edgebetweenness.roc,
                                                                     edgebetweenness.pr,
                                                                     rocpr))
gof.model4.detailed <- btergm::gof(model4, nsim = 50, statistics = c(edgebetweenness.modularity,
                                                                     walktrap.modularity,
                                                                     esp,
                                                                     nsp,
                                                                     deg,
                                                                     geodesic,
                                                                     triad.undirected,
                                                                     walktrap.roc,
                                                                     walktrap.pr,
                                                                     edgebetweenness.roc,
                                                                     edgebetweenness.pr,
                                                                     rocpr))
gof.model5.detailed <- btergm::gof(model5, nsim = 50, statistics = c(edgebetweenness.modularity,
                                                                     walktrap.modularity,
                                                                     esp,
                                                                     nsp,
                                                                     deg,
                                                                     geodesic,
                                                                     triad.undirected,
                                                                     walktrap.roc,
                                                                     walktrap.pr,
                                                                     edgebetweenness.roc,
                                                                     edgebetweenness.pr,
                                                                     rocpr))


plot(gof.model0.detailed)
plot(gof.model1.detailed)
plot(gof.model2.detailed)
plot(gof.model3.detailed)
plot(gof.model4.detailed)
plot(gof.model5.detailed)


###Questions Applied
##Q1 k=30
fit_power_law(degree(fb.network,gmode = "graph")) #alpha=6.26
prob=1/(30**6.26)
prob #5.665274e-10

##Q2
strong_graph #47 nodes, and 227 edges
strong_network=asNetwork(strong_graph)
plot(strong_graph, labels=TRUE)
degree(strong_network, gmode='graph')
which(degree(strong_network, gmode='graph')==max(degree(strong_network, gmode='graph')))
max(degree(strong_network, gmode='graph'))
get.vertex.attribute(strong_network, "vertex.names")[1] #SE has 32 immediate friends


get.vertex.attribute(strong_network, "vertex.names") #SE: index=1, KW: index 13
matrix1=get.adjacency(strong_graph) 
matrix1
matrix2=matrix1 %*% matrix1
matrix2[1,13] #1

##Q3
model0 = ergm(fb.network~edges)
summary(model0)
manual.prob=1/(1+exp(2.50508))
manual.prob #7.55%

##Q4
model4=ergm(fb.network~edges+nodematch("fb.df.group", diff=TRUE)+nodecov('fb.df.mutual_friend_count')+absdiff('fb.df.friend_count')) 
summary(model4)

manual.prob2=1/(1+exp((1.017e+01)-(8.336e+00)*1-(7.326e+00)*1-(2.087e-01)*5-(2.087e-01)*1+(8.810e-04)*2))
manual.prob2 #almost close to 100%
given_predictors=data.frame(nodematch.fb.df.group.Music=1, nodematch.fb.df.group.College=1, nodecov.fb.df.mutual_friend_count=26,nodecov.fb.df.mutual_friend_count=6,absdiff.fb.df.friend_count=200)
predict(model4, newdata=given_predictors) 


###Further Improvement:
#Use Gower Distance to fill missing values
#Find a bigger dataset that has multiple layers and have networks in different time points



