##### Abreviations ####
# npv => number of forest fragment
# lpv => forest proportion
# lpu => urban proportion
# iso => isolation of forest class
# qp => landscape quality (forest proportion + other vegetation types)
# qh => habitat quality (index in text)
# conect => functional conectivity (LSCorridors)

# Scale effect
# 5 => 500m
# 1 => 1km
# 2 => 2km


##### Standardize predict variables ############################
require(vegan)
decostand(npv1, method = "standardize")->npv1.std
decostand(npv5, method = "standardize")->npv5.std
decostand(npv2, method = "standardize")->npv2.std
decostand(lpv5, method = "standardize")->lpv5.std
decostand(lpv1, method = "standardize")->lpv1.std
decostand(lpv2, method = "standardize")->lpv2.std
decostand(lpu5, method = "standardize")->lpu5.std
decostand(lpu1, method = "standardize")->lpu1.std
decostand(lpu2, method = "standardize")->lpu2.std
decostand(qp5, method = "standardize")->qp5.std
decostand(qp1, method = "standardize")->qp1.std
decostand(qp2, method = "standardize")->qp2.std
decostand(iso2, method = "standardize")->iso2.std
decostand(iso5, method = "standardize")->iso5.std
decostand(qh, method = "standardize")->qh.std
decostand(conect, method = "standardize")->conect.std

#### richness interaction #####
sum(rede_RPPN>0)


##### Interaction Richness #####

ira_bar<-shan/log(size)# pielou evenness
ira_bot<-shan/log(size)
ira_cap<-shan/log(size)
ira_cic<-shan/log(size)
ira_mac<-shan/log(size)
ira_papa<-shan/log(size)
ira_rppn<-shan/log(size)

###### Model Selection AICc ==> WNODF <==   ####################
require(AICcmodavg)   

m1<-glm(wnodf~npv1.std,family = gaussian())
m2<-glm(wnodf~lpv1.std,family = gaussian())
m3<-glm(wnodf~lpu2.std,family = gaussian())
m4<-glm(wnodf~iso5.std,family = gaussian())
m5<-glm(wnodf~qp2.std,family = gaussian())
m6<-glm(wnodf~conect.std,family = gaussian())
m7<-glm(wnodf~qh.std,family = gaussian())
m8<-glm(wnodf~qh.std + npv1.std,family = gaussian())
m9<-glm(wnodf~npv1.std+lpv1.std,family = gaussian())
m10<-glm(wnodf~npv1.std+qp2.std,family = gaussian())
m11<-glm(wnodf~npv1.std+conect.std,family = gaussian())
m12<-glm(wnodf~lpv1.std+conect.std,family = gaussian())
m13<-glm(wnodf~lpv1.std+qh.std,family = gaussian())
m14<-glm(wnodf~iso5.std+qp2.std,family = gaussian())
m15<-glm(wnodf~iso5.std+qh.std,family = gaussian())
m16<-glm(wnodf~iso5.std+conect.std,family = gaussian())
m17<-glm(wnodf~qh.std+conect.std,family = gaussian())
m18<-glm(wnodf~qh.std+lpv1.std,family = gaussian())
m19<-glm(wnodf~lpu2.std+iso5.std,family = gaussian())
m20<-glm(wnodf~1)

Cand.models <- list( )
Cand.models[[1]]<-glm(wnodf~npv1.std,family = gaussian())
Cand.models[[2]]<-glm(wnodf~lpv1.std,family = gaussian())
Cand.models[[3]]<-glm(wnodf~lpu2.std,family = gaussian())
Cand.models[[4]]<-glm(wnodf~iso5.std,family = gaussian())
Cand.models[[5]]<-glm(wnodf~qp1.std,family = gaussian())
Cand.models[[6]]<-glm(wnodf~conect.std,family = gaussian())
Cand.models[[7]]<-glm(wnodf~qh.std,family = gaussian())
Cand.models[[8]]<-glm(wnodf~qh.std + npv1.std,family = gaussian())
Cand.models[[9]]<-glm(wnodf~npv1.std+lpv1.std,family = gaussian())
Cand.models[[10]]<-glm(wnodf~npv1.std+qp1.std,family = gaussian())
Cand.models[[11]]<-glm(wnodf~npv1.std+conect.std,family = gaussian())
Cand.models[[12]]<-glm(wnodf~lpv1.std+conect.std,family = gaussian())
Cand.models[[13]]<-glm(wnodf~lpv1.std+qh.std,family = gaussian())
Cand.models[[14]]<-glm(wnodf~iso5.std+qp1.std,family = gaussian())
Cand.models[[15]]<-glm(wnodf~iso5.std+qh.std,family = gaussian())
Cand.models[[16]]<-glm(wnodf~iso5.std+conect.std,family = gaussian())
Cand.models[[17]]<-glm(wnodf~qh.std+conect.std,family = gaussian())
Cand.models[[18]]<-glm(wnodf~qh.std+lpv1.std,family = gaussian())
Cand.models[[19]]<-glm(wnodf~lpu2.std+iso5.std,family = gaussian())
Cand.models[[20]]<-glm(wnodf~1)


Modnames<-paste("mod",1:length(Cand.models),sep=" ")
print(aictab(cand.set = Cand.models, modnames = Modnames,sort = TRUE), digits = 4, LL = TRUE)
####### Model Selection AICc ==> ZH2' <== ########

m1<-glm(h2_obs~npv1.std,family = gaussian())
m2<-glm(h2_obs~lpv2.std,family = gaussian())
m3<-glm(h2_obs~lpu5.std,family = gaussian())
m4<-glm(h2_obs~iso5.std,family = gaussian())
m5<-glm(h2_obs~qp2.std,family = gaussian())
m6<-glm(h2_obs~conect.std,family = gaussian())
m7<-glm(h2_obs~qh.std,family = gaussian())
m8<-glm(h2_obs~qh.std + npv1.std,family = gaussian())
m9<-glm(h2_obs~npv1.std+lpv2.std,family = gaussian())
m10<-glm(h2_obs~npv1.std+qp2.std,family = gaussian())
m11<-glm(h2_obs~npv1.std+conect.std,family = gaussian())
m12<-glm(h2_obs~lpv2.std+conect.std,family = gaussian())
m13<-glm(h2_obs~lpv2.std+qh.std,family = gaussian())
m14<-glm(h2_obs~iso5.std+qp2.std,family = gaussian())
m15<-glm(h2_obs~iso5.std+qh.std,family = gaussian())
m16<-glm(h2_obs~iso5.std+conect.std,family = gaussian())
m17<-glm(h2_obs~qh.std+conect.std,family = gaussian())
m18<-glm(h2_obs~qh.std+lpv2.std,family = gaussian())
m19<-glm(h2_obs~lpu5.std+iso5.std,family = gaussian())
m20<-glm(h2_obs~1)

Cand.models <- list( )
Cand.models[[1]]<-glm(h2_obs~npv1.std,family = gaussian())
Cand.models[[2]]<-glm(h2_obs~lpv2.std,family = gaussian())
Cand.models[[3]]<-glm(h2_obs~lpu5.std,family = gaussian())
Cand.models[[4]]<-glm(h2_obs~iso5.std,family = gaussian())
Cand.models[[5]]<-glm(h2_obs~qp2.std,family = gaussian())
Cand.models[[6]]<-glm(h2_obs~conect.std,family = gaussian())
Cand.models[[7]]<-glm(h2_obs~qh.std,family = gaussian())
Cand.models[[8]]<-glm(h2_obs~qh.std + npv1.std,family = gaussian())
Cand.models[[9]]<-glm(h2_obs~npv1.std+lpv2.std,family = gaussian())
Cand.models[[10]]<-glm(h2_obs~npv1.std+qp2.std,family = gaussian())
Cand.models[[11]]<-glm(h2_obs~npv1.std+conect.std,family = gaussian())
Cand.models[[12]]<-glm(h2_obs~lpv2.std+conect.std,family = gaussian())
Cand.models[[13]]<-glm(h2_obs~lpv2.std+qh.std,family = gaussian())
Cand.models[[14]]<-glm(h2_obs~iso5.std+qp2.std,family = gaussian())
Cand.models[[15]]<-glm(h2_obs~iso5.std+qh.std,family = gaussian())
Cand.models[[16]]<-glm(h2_obs~iso5.std+conect.std,family = gaussian())
Cand.models[[17]]<-glm(h2_obs~qh.std+conect.std,family = gaussian())
Cand.models[[18]]<-glm(h2_obs~qh.std+lpv2.std,family = gaussian())
Cand.models[[19]]<-glm(h2_obs~lpu5.std+iso5.std,family = gaussian())
Cand.models[[20]]<-glm(h2_obs~1)



Modnames<-paste("mod",1:length(Cand.models),sep=" ")
print(aictab(cand.set = Cand.models, modnames = Modnames,sort = TRUE), digits = 4, LL = TRUE)
######## Model Selection AICc ==> evenness <== ######### 

m1<-glm(alternative_interaction~npv5.std,family = gaussian())
m2<-glm(alternative_interaction~lpv5.std,family = gaussian())
m3<-glm(alternative_interaction~lpu5.std,family = gaussian())
m4<-glm(alternative_interaction~iso2.std,family = gaussian())
m5<-glm(alternative_interaction~qp1.std,family = gaussian())
m6<-glm(alternative_interaction~conect.std,family = gaussian())
m7<-glm(alternative_interaction~qh.std,family = gaussian())
m8<-glm(alternative_interaction~qh.std + npv5.std,family = gaussian())
m9<-glm(alternative_interaction~npv5.std+lpv5.std,family = gaussian())
m10<-glm(alternative_interaction~npv5.std+qp1.std,family = gaussian())
m11<-glm(alternative_interaction~npv5.std+conect.std,family = gaussian())
m12<-glm(alternative_interaction~lpv5.std+conect.std,family = gaussian())
m13<-glm(alternative_interaction~lpv5.std+qh.std,family = gaussian())
m14<-glm(alternative_interaction~iso2.std+qp1.std,family = gaussian())
m15<-glm(alternative_interaction~iso2.std+qh.std,family = gaussian())
m16<-glm(alternative_interaction~iso2.std+conect.std,family = gaussian())
m17<-glm(alternative_interaction~qh.std+conect.std,family = gaussian())
m18<-glm(alternative_interaction~qh.std+lpv5.std,family = gaussian())
m19<-glm(alternative_interaction~lpu5.std+iso2.std,family = gaussian())
m20<-glm(alternative_interaction~1)

Cand.models <- list( )
Cand.models[[1]]<-glm(alternative_interaction~npv5.std,family = gaussian())
Cand.models[[2]]<-glm(alternative_interaction~lpv5.std,family = gaussian())
Cand.models[[3]]<-glm(alternative_interaction~lpu5.std,family = gaussian())
Cand.models[[4]]<-glm(alternative_interaction~iso2.std,family = gaussian())
Cand.models[[5]]<-glm(alternative_interaction~qp1.std,family = gaussian())
Cand.models[[6]]<-glm(alternative_interaction~conect.std,family = gaussian())
Cand.models[[7]]<-glm(alternative_interaction~qh.std,family = gaussian())
Cand.models[[8]]<-glm(alternative_interaction~qh.std + npv5.std,family = gaussian())
Cand.models[[9]]<-glm(alternative_interaction~npv5.std+lpv5.std,family = gaussian())
Cand.models[[10]]<-glm(alternative_interaction~npv5.std+qp1.std,family = gaussian())
Cand.models[[11]]<-glm(alternative_interaction~npv5.std+conect.std,family = gaussian())
Cand.models[[12]]<-glm(alternative_interaction~lpv5.std+conect.std,family = gaussian())
Cand.models[[13]]<-glm(alternative_interaction~lpv5.std+qh.std,family = gaussian())
Cand.models[[14]]<-glm(alternative_interaction~iso2.std+qp1.std,family = gaussian())
Cand.models[[15]]<-glm(alternative_interaction~iso2.std+qh.std,family = gaussian())
Cand.models[[16]]<-glm(alternative_interaction~iso2.std+conect.std,family = gaussian())
Cand.models[[17]]<-glm(alternative_interaction~qh.std+conect.std,family = gaussian())
Cand.models[[18]]<-glm(alternative_interaction~qh.std+lpv5.std,family = gaussian())
Cand.models[[19]]<-glm(alternative_interaction~lpu5.std+iso2.std,family = gaussian())
Cand.models[[20]]<-glm(alternative_interaction~1)



Modnames<-paste("mod",1:length(Cand.models),sep=" ")
print(aictab(cand.set = Cand.models, modnames = Modnames,sort = TRUE), digits = 4, LL = TRUE)
######### Model Selection AICc ==> Size <== #################
m1<-glm(size~npv2.std,family = poisson(link = "log"))
m2<-glm(size~lpv1.std,family = poisson(link = "log"))
m3<-glm(size~lpu5.std,family = poisson (link = "log"))
m4<-glm(size~iso2.std,family = poisson (link = "log"))
m5<-glm(size~qp1.std,family = poisson (link = "log"))
m6<-glm(size~conect.std,family = poisson (link = "log"))
m7<-glm(size~qh.std,family = poisson (link = "log"))
m8<-glm(size~qh.std + npv2.std,family = poisson (link = "log"))
m9<-glm(size~npv2.std+lpv1.std,family = poisson (link = "log"))
m10<-glm(size~npv2.std+qp1.std,family = poisson (link = "log"))
m11<-glm(size~npv2.std+conect.std,family = poisson (link = "log"))
m12<-glm(size~lpv1.std+conect.std,family = poisson (link = "log"))
m13<-glm(size~lpv1.std+qh.std,family = poisson (link = "log"))
m14<-glm(size~iso2.std+qp1.std,family = poisson (link = "log"))
m15<-glm(size~iso2.std+qh.std,family = poisson (link = "log"))
m16<-glm(size~iso2.std+conect.std,family = poisson (link = "log"))
m17<-glm(size~qh.std+conect.std,family = poisson (link = "log"))
m18<-glm(size~qh.std+lpv1.std,family = poisson (link = "log"))
m19<-glm(size~lpu5.std+iso2.std,family = poisson (link = "log"))
m20<-glm(size~1,family = poisson (link = "log"))

Cand.models <- list( )
Cand.models[[1]]<-glm(size~npv2.std,family = poisson (link = "log"))
Cand.models[[2]]<-glm(size~lpv1.std,family = poisson (link = "log"))
Cand.models[[3]]<-glm(size~lpu5.std,family = poisson (link = "log"))
Cand.models[[4]]<-glm(size~iso2.std,family = poisson (link = "log"))
Cand.models[[5]]<-glm(size~qp1.std,family = poisson (link = "log"))
Cand.models[[6]]<-glm(size~conect.std,family = poisson (link = "log"))
Cand.models[[7]]<-glm(size~qh.std,family = poisson (link = "log"))
Cand.models[[8]]<-glm(size~qh.std + npv2.std,family = poisson (link = "log"))
Cand.models[[9]]<-glm(size~npv2.std+lpv1.std,family = poisson (link = "log"))
Cand.models[[10]]<-glm(size~npv2.std+qp1.std,family = poisson (link = "log"))
Cand.models[[11]]<-glm(size~npv2.std+conect.std,family = poisson (link = "log"))
Cand.models[[12]]<-glm(size~lpv1.std+conect.std,family = poisson (link = "log"))
Cand.models[[13]]<-glm(size~lpv1.std+qh.std,family = poisson (link = "log"))
Cand.models[[14]]<-glm(size~iso2.std+qp1.std,family = poisson (link = "log"))
Cand.models[[15]]<-glm(size~iso2.std+qh.std,family = poisson (link = "log"))
Cand.models[[16]]<-glm(size~iso2.std+conect.std,family = poisson (link = "log"))
Cand.models[[17]]<-glm(size~qh.std+conect.std,family = poisson (link = "log"))
Cand.models[[18]]<-glm(size~qh.std+lpv1.std,family = poisson (link = "log"))
Cand.models[[19]]<-glm(size~lpu5.std+iso2.std,family = poisson (link = "log"))
Cand.models[[20]]<-glm(size~1,family = poisson (link = "log"))



Modnames<-paste("mod",1:length(Cand.models),sep=" ")
print(aictab(cand.set = Cand.models, modnames = Modnames,sort = TRUE), digits = 4, LL = TRUE)
########## Model Selection AICc ==> Bird Richness <=== ############
m1<-glm(riq_a_geral~npv2.std,family = poisson(link = "log"))
m2<-glm(riq_a_geral~lpv5.std,family = poisson(link = "log"))
m3<-glm(riq_a_geral~lpu1.std,family = poisson(link = "log"))
m4<-glm(riq_a_geral~iso2.std,family = poisson(link = "log"))
m5<-glm(riq_a_geral~qp1.std,family = poisson(link = "log"))
m6<-glm(riq_a_geral~conect.std,family = poisson(link = "log"))
m7<-glm(riq_a_geral~qh.std,family = poisson(link = "log"))
m8<-glm(riq_a_geral~qh.std + npv2.std,family = poisson(link = "log"))
m9<-glm(riq_a_geral~npv2.std+lpv5.std,family = poisson(link = "log"))
m10<-glm(riq_a_geral~npv2.std+qp1.std,family = poisson(link = "log"))
m11<-glm(riq_a_geral~npv2.std+conect.std,family = poisson(link = "log"))
m12<-glm(riq_a_geral~lpv5.std+conect.std,family = poisson(link = "log"))
m13<-glm(riq_a_geral~lpv5.std+qh.std,family = poisson(link = "log"))
m14<-glm(riq_a_geral~iso2.std+qp1.std,family = poisson(link = "log"))
m15<-glm(riq_a_geral~iso2.std+qh.std,family = poisson(link = "log"))
m16<-glm(riq_a_geral~iso2.std+conect.std,family = poisson(link = "log"))
m17<-glm(riq_a_geral~qh.std+conect.std,family = poisson(link = "log"))
m18<-glm(riq_a_geral~qp1.std+lpv5.std,family = poisson(link = "log"))
m19<-glm(riq_a_geral~lpu1.std+iso2.std,family = poisson(link = "log"))
m20<-glm(riq_a_geral~lpv5.std+iso2.std,family = poisson(link = "log"))
m21<-glm(riq_a_geral~1,family = poisson(link = "log"))

Cand.models <- list( )
Cand.models[[1]]<-glm(riq_a_geral~npv2.std,family = poisson(link = "log"))
Cand.models[[2]]<-glm(riq_a_geral~lpv5.std,family = poisson(link = "log"))
Cand.models[[3]]<-glm(riq_a_geral~lpu1.std,family = poisson(link = "log"))
Cand.models[[4]]<-glm(riq_a_geral~iso2.std,family = poisson(link = "log"))
Cand.models[[5]]<-glm(riq_a_geral~qp1.std,family = poisson(link = "log"))
Cand.models[[6]]<-glm(riq_a_geral~conect.std,family = poisson(link = "log"))
Cand.models[[7]]<-glm(riq_a_geral~qh.std,family = poisson(link = "log"))
Cand.models[[8]]<-glm(riq_a_geral~qh.std + npv2.std,family = poisson(link = "log"))
Cand.models[[9]]<-glm(riq_a_geral~npv2.std+lpv5.std,family = poisson(link = "log"))
Cand.models[[10]]<-glm(riq_a_geral~npv2.std+qp1.std,family = poisson(link = "log"))
Cand.models[[11]]<-glm(riq_a_geral~npv2.std+conect.std,family = poisson(link = "log"))
Cand.models[[12]]<-glm(riq_a_geral~lpv5.std+conect.std,family = poisson(link = "log"))
Cand.models[[13]]<-glm(riq_a_geral~lpv5.std+qh.std,family = poisson(link = "log"))
Cand.models[[14]]<-glm(riq_a_geral~iso2.std+qp1.std,family = poisson(link = "log"))
Cand.models[[15]]<-glm(riq_a_geral~iso2.std+qh.std,family = poisson(link = "log"))
Cand.models[[16]]<-glm(riq_a_geral~iso2.std+conect.std,family = poisson(link = "log"))
Cand.models[[17]]<-glm(riq_a_geral~qh.std+conect.std,family = poisson(link = "log"))
Cand.models[[18]]<-glm(riq_a_geral~qp1.std+lpv5.std,family = poisson(link = "log"))
Cand.models[[19]]<-glm(riq_a_geral~lpu1.std+iso2.std,family = poisson(link = "log"))
Cand.models[[20]]<-glm(riq_a_geral~lpv5.std+iso2.std,family = poisson(link = "log"))
Cand.models[[21]]<-glm(riq_a_geral~1,family = poisson(link = "log"))



Modnames<-paste("mod",1:length(Cand.models),sep=" ")
print(aictab(cand.set = Cand.models, modnames = Modnames,sort = TRUE), digits = 4, LL = TRUE)
########### Model Selection AICc ==> Plant Richness <== ###########
m1<-glm(riq_p_geral~npv2.std,family = poisson(link = "log"))
m2<-glm(riq_p_geral~lpv5.std,family = poisson(link = "log"))
m3<-glm(riq_p_geral~lpu5.std,family = poisson(link = "log"))
m4<-glm(riq_p_geral~iso2.std,family = poisson(link = "log"))
m5<-glm(riq_p_geral~qp1.std,family = poisson(link = "log"))
m6<-glm(riq_p_geral~conect.std,family = poisson(link = "log"))
m7<-glm(riq_p_geral~qh.std,family = poisson(link = "log"))
m8<-glm(riq_p_geral~qh.std + npv2.std,family = poisson(link = "log"))
m9<-glm(riq_p_geral~npv2.std+lpv5.std,family = poisson(link = "log"))
m10<-glm(riq_p_geral~npv2.std+qp1.std,family = poisson(link = "log"))
m11<-glm(riq_p_geral~npv2.std+conect.std,family = poisson(link = "log"))
m12<-glm(riq_p_geral~lpv5.std+conect.std,family = poisson(link = "log"))
m13<-glm(riq_p_geral~lpv5.std+qh.std,family = poisson(link = "log"))
m14<-glm(riq_p_geral~iso2.std+qp1.std,family = poisson(link = "log"))
m15<-glm(riq_p_geral~iso2.std+qh.std,family = poisson(link = "log"))
m16<-glm(riq_p_geral~iso2.std+conect.std,family = poisson(link = "log"))
m17<-glm(riq_p_geral~qh.std+conect.std,family = poisson(link = "log"))
m18<-glm(riq_p_geral~qh.std+lpv5.std,family = poisson(link = "log"))
m19<-glm(riq_p_geral~lpu5.std+iso2.std,family = poisson(link = "log"))
m20<-glm(riq_p_geral~1,family = poisson(link = "log"))

Cand.models <- list( )
Cand.models[[1]]<-glm(riq_p_geral~npv2.std,family = poisson(link = "log"))
Cand.models[[2]]<-glm(riq_p_geral~lpv5.std,family = poisson(link = "log"))
Cand.models[[3]]<-glm(riq_p_geral~lpu5.std,family = poisson(link = "log"))
Cand.models[[4]]<-glm(riq_p_geral~iso2.std,family = poisson(link = "log"))
Cand.models[[5]]<-glm(riq_p_geral~qp1.std,family = poisson(link = "log"))
Cand.models[[6]]<-glm(riq_p_geral~conect.std,family = poisson(link = "log"))
Cand.models[[7]]<-glm(riq_p_geral~qh.std,family = poisson(link = "log"))
Cand.models[[8]]<-glm(riq_p_geral~qh.std + npv2.std,family = poisson(link = "log"))
Cand.models[[9]]<-glm(riq_p_geral~npv2.std+lpv5.std,family = poisson(link = "log"))
Cand.models[[10]]<-glm(riq_p_geral~npv2.std+qp1.std,family = poisson(link = "log"))
Cand.models[[11]]<-glm(riq_p_geral~npv2.std+conect.std,family = poisson(link = "log"))
Cand.models[[12]]<-glm(riq_p_geral~lpv5.std+conect.std,family = poisson(link = "log"))
Cand.models[[13]]<-glm(riq_p_geral~lpv5.std+qh.std,family = poisson(link = "log"))
Cand.models[[14]]<-glm(riq_p_geral~iso2.std+qp1.std,family = poisson(link = "log"))
Cand.models[[15]]<-glm(riq_p_geral~iso2.std+qh.std,family = poisson(link = "log"))
Cand.models[[16]]<-glm(riq_p_geral~iso2.std+conect.std,family = poisson(link = "log"))
Cand.models[[17]]<-glm(riq_p_geral~qh.std+conect.std,family = poisson(link = "log"))
Cand.models[[18]]<-glm(riq_p_geral~qh.std+lpv5.std,family = poisson(link = "log"))
Cand.models[[19]]<-glm(riq_p_geral~lpu5.std+iso2.std,family = poisson(link = "log"))
Cand.models[[20]]<-glm(riq_p_geral~1,family = poisson(link = "log"))



Modnames<-paste("mod",1:length(Cand.models),sep=" ")
print(aictab(cand.set = Cand.models, modnames = Modnames,sort = TRUE), digits = 4, LL = TRUE)

############ Sampling Effort ####
int.as.sp <- as.vector(as.matrix(rede_PAPA))
So <- sum(rede_PAPA>0)
Sp <- estimateR(int.as.sp, index="chao")[2]
Sperc <- 100*(So/Sp)
se.Sp <- estimateR(int.as.sp, index="chao")[3]
a <- rarefy(int.as.sp, 1:sum(int.as.sp), se=TRUE)
my.ylim <- c(0,Sp+se.Sp+0.05*(Sp+se.Sp))
if (is.nan(se.Sp))
  my.ylim <- c(0, Sp + 0.1*Sp)
png(filename="papa_effort.tiff", width = 3000, height = 1500,res = 400)
try(plot(1:sum(int.as.sp),a[1,], ylim=my.ylim, ann=F, type="n")) #rarefaction curve
se.top <- a[1,] + a[2,] # errors
se.bottom <- a[1,] - a[2,]
points(1:sum(int.as.sp), se.bottom, col="gray50", pch=16, cex=0.25)
points(1:sum(int.as.sp), se.top, col="gray50", pch=16, cex=0.25)
lines(1:sum(int.as.sp),a[1,], lwd=1.5)
abline(h=Sp, lty=2, lwd=1.5)
abline(h=c(Sp+se.Sp,Sp-se.Sp), lty=2, col="gray50", lwd=1.5)
title(xlab="Interactions recorded", ylab="Number of unique interactions",main="Pope Park", outer=F, font.lab=1, cex.lab=1)
dev.off()
############# Correlation #######
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- corralacao_nodf
chart.Correlation(my_data, histogram=TRUE, pch=19)

############## Plot #####
png(filename="riqAxNF_1.tiff", width = 3000, height = 1500,res = 400)
dia_plot <- ggplot(data_urban, aes(x = npv, y = riq_a_geral))+
  geom_point()
p<-dia_plot + geom_smooth(method="lm", se = T, size = 0.5, col = "black")
p + theme(panel.background = element_rect(fill = "white", colour = "black"))+
  xlab("Number of Fragment(1000m)")+
  ylab("Bird Richness")
dev.off()


############### Bipartite networks #####
png(filename="rppn.tiff", width = 3000, height = 1500,res = 400)
plotweb(rede_RPPN, method="normal", text.rot="90", labsize=1.1, 
        col.low="green",col.high="red", col.interaction="black")
dev.off()

