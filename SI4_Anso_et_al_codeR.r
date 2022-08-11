# author: Gasc Amandine and Anso Jeremy
# year: 2020
# Supplementary information of Anso et al. 2020

# Bibliotheque
library(vegan)
library(ggplot2)
library(ade4)
library(dplyr)
library(tibble)

#################################################################################
# Data preparation 

#◘ Load Environmental Data (env)
env<-read.csv("Enviro_data.csv",sep=";")

#◘ Preparation de la table des données environmentales (envTab) et la meme chose moyenné par site (envTab_persites)
envTab<-env[order(env$habitat),2:18]
rownames(envTab)<-env[order(env$habitat),1]
site1=substr(rownames(envTab),1,5)

envTab_2<-cbind(site1,envTab)
envTab_persites_av<-envTab_2 %>% group_by(site1) %>% summarise_all(funs(mean))
envTab_persites<-as.matrix(envTab_persites_av[,-1])
rownames(envTab_persites)<-envTab_persites_av$site1



#◘ Load Taxonomic Inventories (TableTaxoSansSP)
TableTaxo<-read.csv("CricketInventory_data.csv",sep=";")

#◘ Preparation Of cricket community tables

#Cricket community table (ComMatrix) 
ComMatrix<-as.data.frame.matrix(t(table(as.character(TableTaxo$genre_espece),as.character(TableTaxo$plot_ID))))

#Averaged per site (ComMatrix_persites)
site2=substr(rownames(ComMatrix),1,5)
ComMatrix_2<-cbind(site2,ComMatrix)
ComMatrix_persites_av<-ComMatrix_2 %>% group_by(site2) %>% summarise_all(funs(mean))
ComMatrix_persites<-as.matrix(ComMatrix_persites_av[,-1])
rownames(ComMatrix_persites)<-ComMatrix_persites_av$site2

#Presence/absence (ComMatrix_persites_PresenceAbs)
ComMatrix_persites_PresenceAbs<-ComMatrix_persites
ComMatrix_persites_PresenceAbs[ComMatrix_persites_PresenceAbs>0]<-1

#Averaged per site only with soniferous species (ComMatrix_persites_Acou)
Acoustic_species<-c("Agnotecous azurensis","Agnotecous clarus","Agnotecous meridionalis","Bullita fusca","Bullita mouirangensis","Calcirtus magnus","Koghiella flammea","Koghiella nigris","Matuanus affinis mirabilis","MOGOPLISTIDAE sp1","MOGOPLISTIDAE sp2","MOGOPLISTIDAE sp3","MOGOPLISTIDAE sp7","Notosciobia affinis paranola (ancien sp1)","Notosciobia minoris (sp2)","Notosciobia sp1","Pixibinthus sonicus","Protathra nana","Pseudotrigonidium caledonica")
ComMatrix_persites_Acou<-ComMatrix_persites[,colnames(ComMatrix_persites)%in%Acoustic_species]

#Presence/absence only with soniferous species (ComMatrix_persites_Acou_PresenceAbs)
ComMatrix_persites_Acou_PresenceAbs<-ComMatrix_persites_Acou
ComMatrix_persites_Acou_PresenceAbs[ComMatrix_persites_Acou_PresenceAbs>0]<-1



###################################################################################
###################################################################################
# Figure 1 of Anso et al. 2020
Name1<-as.character(TableTaxo$genre_espece)
OldNames<-unique(Name1)

#M
Name1[which(Name1=="Adenopterus crouensis (sp1)")]<-"aa-Adenopterus crouensis"  #Night
Name1[which(Name1=="Pixipterus punctulatus (sp3)")]<-"ba-Pixipterus punctulatus"  #Night   
Name1[which(Name1=="MOGOPLISTIDAE sp3")]<-"ca-MOGOPLISTIDAE sp3"  # Night/Day

#M/P
Name1[which(Name1=="MOGOPLISTIDAE sp1")]<-"da-MOGOPLISTIDAE sp1" #Night
Name1[which(Name1=="Pixibinthus sonicus")]<-"ea-Pixibinthus sonicus"   #Night/Day
Name1[which(Name1=="Koghiella flammea")]<-"fa-Koghiella flammea" #Night/Day
Name1[which(Name1=="MOGOPLISTIDAE sp2")]<-"ga-MOGOPLISTIDAE sp2" #Night/Day

#P
Name1[which(Name1=="Notosciobia affinis paranola (ancien sp1)")]<-"ha-Notosciobia affinis paranola"  #Night
Name1[which(Name1=="TRIGONIDIINAE sp")]<-"ia-TRIGONIDIINAE sp" #Night
Name1[which(Name1=="Bullita obscura")]<-"ja-Bullita obscura"  #Day/Night

#FO/P
Name1[which(Name1=="Caltathra balmessae")]<-"ka-Caltathra balmessae" #Night 
Name1[which(Name1=="Kanakinemobius sp")]<-"la-Kanakinemobius sp"  #Night
Name1[which(Name1=="Agnotecous azurensis")]<-"ma-Agnotecous azurensis" #Day/Night
Name1[which(Name1=="Agnotecous clarus")]<-"na-Agnotecous clarus" #Day/Night
Name1[which(Name1=="Bullita fusca")]<-"oa-Bullita fusca"  #Day/Night
Name1[which(Name1=="Caltathra meunieri")]<-"pa-Caltathra meunieri"  #Day/Night
Name1[which(Name1=="Pseudotrigonidium ana")]<-"qa-Pseudotrigonidium ana" #Day/Night     
Name1[which(Name1=="Koghiella nigris")]<-"ra-Koghiella nigris"  #Day/Night
Name1[which(Name1=="Notosciobia minoris (sp2)")]<-"sa-Notosciobia minoris" #Day/Night 
#FO
Name1[which(Name1=="Adenopterus meridionalis (sp2)")]<-"ta-Adenopterus meridionalis"  #Night     
Name1[which(Name1=="Agnotecous meridionalis")]<-"ua-Agnotecous meridionalis"   #Night
Name1[which(Name1=="Protathra nana")]<-"va-Protathra nana" #Night 
Name1[which(Name1=="Calcirtus magnus (sp3)")]<-"wa-Calcirtus magnus" #Night                                
Name1[which(Name1=="Calcirtus amoa (sp1)")]<-"xa-Calcirtus amoa" #Night   
Name1[which(Name1=="Bullita mouirangensis")]<-"ya-Bullita mouirangensis" #Day/Night
Name1[which(Name1=="MOGOPLISTIDAE sp7")]<-"za-MOGOPLISTIDAE sp7" #Day/Night 
Name1[which(Name1=="Matuanus affinis mirabilis")]<-"zb-Matuanus affinis mirabilis" #Day/Night  
Name1[which(Name1=="Notosciobia sp1")]<-"zc-Notosciobia sp1" #Day  
Name1[which(Name1=="Paniella bipunctatus")]<-"zd-Paniella bipunctatus" #Day                        
Name1[which(Name1=="Pseudotrigonidium caledonica")]<-"ze-Pseudotrigonidium caledonica" #Day                                   
                                 
Habitat_ordered<-as.character(TableTaxo$Habitat)
Habitat_ordered[which(Habitat_ordered=="shrubland")]<-"1"
Habitat_ordered[which(Habitat_ordered=="preforest")]<-"2"
Habitat_ordered[which(Habitat_ordered=="forest")]<-"3"

NameOrdered<-Name1                                  
TableTaxo2<-as.data.frame(cbind(TableTaxo,NameOrdered))
TableTaxo2$Habitat_ordered=factor(TableTaxo2$Habitat, levels=c("maquis","paraforestier","forest"))
TableTaxo2$jour_nuit_ordered=factor(TableTaxo2$jour_nuit, levels=c("nuit","jour"))


#Figure premier test (attention a ce que ce ne soir pas trop redondant avec le tableau. Peu etre retirer les effectif du tableau?)
ggplot(data=TableTaxo2,aes(NameOrdered))+
geom_bar()+ 
facet_wrap(~Habitat_ordered, dir = "v")+
theme_bw()+
theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave("E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/FigureInventaire.pdf")

#Figure deuxieme test avec info jour/nuit
ggplot(data=TableTaxo2,aes(NameOrdered, fill=jour_nuit_ordered))+
geom_bar()+ 
facet_wrap(~Habitat_ordered, dir = "v")+
theme_bw()+
theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+ 
scale_fill_manual(values = c("black", "grey"))
ggsave("E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/FigureInventaire_JourNuit.pdf")

}

########################################################################################################################
# Analyse 2: Conover for pairwise multiple comparisons of the ranked data between stages: vegetation caracteristics and richness differences between habitat
{
#...Vegetation 

#  average per site
env1<-cbind(site=substr(env$habitat,1,5),env)
Env_persite_Av<-env1[,-which(colnames(env1)=="habitat")] %>% 
group_by(site) %>%
summarise_all(funs(mean))

# Average and SD per habitat
env2<-cbind(habitat=substr(Env_persite$site,1,2),Env_persite)
Env_perhab_Sd<-env2[,-which(colnames(env2)=="site")] %>% 
group_by(habitat) %>%
summarise_all(funs(sd),)

Env_perhab_Av<-env2[,-which(colnames(env2)=="site")] %>% 
group_by(habitat) %>%
summarise_all(funs(mean),)

Env_perhab_Av
# A tibble: 3 x 18
  # habitat herba arbus  arbo  sol_nu veg_height nb_tiges dbh_moy Canopy_open rich_flore T_moy T_min T_max T_ecart H_moy H_min H_max H_ecart
  # <fct>   <dbl> <dbl> <dbl>   <dbl>      <dbl>    <dbl>   <dbl>       <dbl>      <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>
# 1 FO      0.125 0.328 0.353 0            23.8      15.9    57.5        7.23       18.1  18.6  16.6  21.2    4.65  88.1  68.8  96.4    27.6
# 2 MA      0.554 0.538 0.025 0.275         2.25      0.5    12.6       90.6        17.4  21.5  16.2  28.8   12.6   78.5  47.4  96.1    48.8
# 3 PA      0.238 0.312 0.372 0.00625      14.9      19      56.0        9.81       14.8  19.5  16.5  24.0    7.54  82.9  59.1  97.0    37.9

Env_perhab_Sd
# # A tibble: 3 x 18
  # habitat herba  arbus   arbo sol_nu veg_height nb_tiges dbh_moy Canopy_open rich_flore T_moy T_min T_max T_ecart H_moy H_min H_max H_ecart
  # <fct>   <dbl>  <dbl>  <dbl>  <dbl>      <dbl>    <dbl>   <dbl>       <dbl>      <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>
# 1 FO      0.134 0.121  0.0856 0           4.33     5.19     11.5       0.753       6.02 0.431 0.372 0.714   0.517  1.62  2.61  1.28    1.39
# 2 MA      0.241 0.138  0.0204 0.115       0.957    0.707    14.8       6.70        2.43 0.764 1.59  1.98    2.60   4.80  8.91  2.45    6.51
# 3 PA      0.165 0.0829 0.175  0.0125      2.25    11.6      11.6       3.15        2.33 0.533 0.970 0.611   1.26   1.50  1.54  1.35    1.34

# Kruskall test with post hoc multiple comparison conover test with bonferonni correction
library(PMCMR)
Hab=as.factor(substr(Env_persite_Av$site,1,2))
TableKrusk<-as.matrix(Env_persite_Av[,-1])
KrusK<-list()
PostHocKrusk<-list()
Var<-colnames(TableKrusk)
for (i in 1:ncol(TableKrusk))
{
KrusK[[i]]<-kruskal.test(TableKrusk[,i]~Hab)
PostHocKrusk[[i]]<-posthoc.kruskal.conover.test(TableKrusk[,i],Hab,"bonferroni")
}
names(KrusK)<-names(PostHocKrusk)<-Var


PostHocKrusk
# $herba

        # Pairwise comparisons using Conover's-test for multiple  
                         # comparisons of independent samples 
   # FO    MA   
# MA 0.032* -    
# PA 1.000 0.119

# P value adjustment method: bonferroni 

# $arbus
   # FO    MA   
# MA 0.106 -    
# PA 1.000 0.096

# $arbo
   # FO     MA    
# MA 0.0183* -     
# PA 1.0000 0.0088

# $sol_nu
   # FO      MA     
# MA 0.00021*** -      
# PA 0.94943 0.00075***

# $veg_height
   # FO      MA    
# MA 2.1e-05*** -     
# PA 0.0038**  0.0038**

# $nb_tiges
   # FO    MA   
# MA 0.012* -    
# PA 1.000 0.015*

# $dbh_moy
   # FO    MA   
# MA 0.012* -    
# PA 1.000 0.015*

# $Canopy_open
   # FO     MA    
# MA 0.0031** -     
# PA 0.6219 0.0237*

# $rich_flore
   # FO   MA  
# MA 1.00 -   
# PA 0.64 0.79

# $T_moy
   # FO     MA    
# MA 0.0002*** -     
# PA 0.0363* 0.0126*

# $T_min
   # FO MA
# MA 1  - 
# PA 1  1 

# $T_max
   # FO      MA    
# MA 3.2e-05*** -     
# PA 0.0053**  0.0053**

# $T_ecart
   # FO      MA    
# MA 3.2e-05*** -     
# PA 0.0053 ** 0.0053**

# $H_moy
   # FO     MA    
# MA 0.0031** -     
# PA 0.0237* 0.6219

# $H_min
   # FO      MA     
# MA 0.00068*** -      
# PA 0.01879* 0.12707

# $H_max
   # FO MA
# MA 1  - 
# PA 1  1 

# $H_ecart
   # FO      MA    
# MA 3.2e-05*** -     
# PA 0.0053**  0.0053**


#...Crickets
#  Average and SD Richness per Habitat
ComMatrix_persites_PA<-ComMatrix_persites
ComMatrix_persites_PA[ComMatrix_persites_PA>0]<-1
Richess_persite<-apply(ComMatrix_persites_PA,1,sum)

tapply(as.numeric(as.vector(Richess_persite)),substr(names(Richess_persite),1,2 ), mean) 
 # FO  MA  PA 
# 7.5 3.5 7.0
tapply(as.numeric(as.vector(Richess_persite)),substr(names(Richess_persite),1,2 ), sd) 
 # FO       MA       PA 
# 3.696846 1.290994 2.708013 

posthoc.kruskal.conover.test(as.vector(Richess_persite),as.factor(substr(names(Richess_persite),1,2 )),"bonferroni")
    # FO   MA  
# MA 0.14 -   
# PA 1.00 0.11


#  Overall abundance average and SD Richness per Habitat
Richess_persite_OA<-apply(ComMatrix_persites,1,sum)

tapply(as.numeric(as.vector(Richess_persite_OA)),substr(names(Richess_persite_OA),1,2 ), mean) 
    # FO     MA     PA 
# 57.125 16.500 48.375
tapply(as.numeric(as.vector(Richess_persite_OA)),substr(names(Richess_persite_OA),1,2 ), sd) 
       # FO        MA        PA 
# 12.847665  9.582971 15.189772 

posthoc.kruskal.conover.test(as.vector(Richess_persite_OA),as.factor(substr(names(Richess_persite_OA),1,2 )),"bonferroni")
   # FO    MA   
# MA 0.005 -    
# PA 1.000 0.022

}

########################################################################################################################
# Analyse 3: NMDS
{

#◘ NMDS sur données non moyennées par site puis fit des données environementale a partir de cette NMDS, données abondance
MDSres <- metaMDS(ComMatrix,try=1000)
# MDSres
# Call:
# metaMDS(comm = ComMatrix, try = 1000) 

# global Multidimensional Scaling using monoMDS

# Data:     wisconsin(ComMatrix) 
# Distance: bray 

# Dimensions: 2 
# Stress:     0.0920718 
# Stress type 1, weak ties
# Two convergent solutions found after 1000 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on ‘wisconsin(ComMatrix)’
save(MDSres,file="E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/MDSres.Rdata" )


fit <- envfit(MDSres, envTab, perm = 999)
               # NMDS1    NMDS2     r2 Pr(>r)    
# herba        0.99855 -0.05385 0.3689  0.005 ** 
# arbus        0.70145 -0.71272 0.3814  0.006 ** 
# arbo        -0.68558  0.72800 0.5311  0.002 ** 
# sol_nu       0.94087 -0.33878 0.6472  0.001 ***
# veg_height  -0.92617  0.37711 0.8586  0.001 ***
# nb_tiges    -0.99946  0.03278 0.3376  0.015 *  
# dbh_moy     -0.95244  0.30472 0.4801  0.001 ***
# Canopy_open  0.90031 -0.43526 0.8351  0.001 ***
# rich_flore  -0.86522 -0.50140 0.0089  0.920    
# T_moy        0.92470 -0.38070 0.8200  0.001 ***
# T_min       -0.34198 -0.93971 0.0301  0.706    
# T_max        0.99637 -0.08515 0.8018  0.001 ***
# T_ecart      0.99995  0.00974 0.7243  0.001 ***
# H_moy       -0.99559 -0.09383 0.5751  0.001 ***
# H_min       -0.99963  0.02731 0.6213  0.001 ***
# H_max        0.02572 -0.99967 0.0019  0.978    
# H_ecart      0.99893 -0.04633 0.7133  0.001 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999

save(fit,file="E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/fit.Rdata" )


#◘ sur données moyennée par sites puis fit des données environementale a partir de cette NMDS, données abondance   # CONSERVE POUR ARTICLE
MDSres2 <- metaMDS(ComMatrix_persites,try=1000) 
# Call:
# metaMDS(comm = ComMatrix_persites, try = 1000) 

# global Multidimensional Scaling using monoMDS

# Data:     wisconsin(ComMatrix_persites) 
# Distance: bray 

# Dimensions: 2 
# Stress:     0.0652302 
# Stress type 1, weak ties
# Two convergent solutions found after 1000 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on ‘wisconsin(ComMatrix_persites)’
save(MDSres2,file="E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/MDSres2.Rdata" )

fit2 <- envfit(MDSres2, envTab_persites, perm = 999) 
               # NMDS1    NMDS2     r2 Pr(>r)   
# herba        0.85235  0.52298 0.5243  0.029 * 
# arbus        0.82677  0.56255 0.4639  0.058 . 
# arbo        -0.96138 -0.27524 0.5165  0.041 * 
# sol_nu       0.99685 -0.07930 0.6669  0.002 **
# veg_height  -0.80847  0.58853 0.8270  0.003 **
# nb_tiges    -0.99510 -0.09889 0.5354  0.029 * 
# dbh_moy     -0.94911  0.31494 0.5867  0.020 * 
# Canopy_open  0.99994  0.01091 0.8164  0.002 **
# rich_flore   0.07830  0.99693 0.0854  0.663   
# T_moy        1.00000  0.00287 0.7234  0.004 **
# T_min       -0.19651  0.98050 0.0668  0.754   
# T_max        0.94387 -0.33032 0.7088  0.004 **
# T_ecart      0.89427 -0.44752 0.6598  0.008 **
# H_moy       -0.87703  0.48043 0.5046  0.046 * 
# H_min       -0.91769  0.39730 0.5575  0.024 * 
# H_max        0.41969  0.90767 0.0025  0.991   
# H_ecart      0.92399 -0.38242 0.6462  0.012 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999
save(fit2,file="E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/fit2.Rdata" )


#◘ sur données moyennée par sites ET espèces ACOUSTIQUE puis fit des données environementale a partir de cette NMDS, données abondance   # CONSERVE POUR ARTICLE
MDSres3 <- metaMDS(ComMatrix_persites_Acou,try=1000) 
# Call:
# metaMDS(comm = ComMatrix_persites_Acou, try = 1000) 
# global Multidimensional Scaling using monoMDS
# Data:     wisconsin(ComMatrix_persites_Acou) 
# Distance: bray 
# Dimensions: 2 
# Stress:     0.03416982 
# Stress type 1, weak ties
# Two convergent solutions found after 1000 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on ‘wisconsin(ComMatrix_persites_Acou)’
save(MDSres3,file="H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R//MDSres3.Rdata" )

load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/MDSres3.Rdata")
fit3 <- envfit(MDSres3, envTab_persites, perm = 999) 
               # NMDS1    NMDS2     r2 Pr(>r)    
# herba        0.99683 -0.07950 0.4143  0.089 .  
# arbus        0.81350 -0.58156 0.4970  0.054 .  
# arbo        -0.83649  0.54798 0.6012  0.024 *  
# sol_nu       0.95066 -0.31022 0.6764  0.005 ** 
# veg_height  -0.97358  0.22834 0.9081  0.001 ***
# nb_tiges    -0.99897 -0.04530 0.4197  0.088 .  
# dbh_moy     -0.90901  0.41677 0.7386  0.003 ** 
# Canopy_open  0.94043 -0.33999 0.8077  0.002 ** 
# rich_flore  -0.48482  0.87461 0.0803  0.699    
# T_moy        0.87659 -0.48124 0.7967  0.001 ***
# T_min       -0.30191 -0.95334 0.2648  0.253    
# T_max        0.98446 -0.17560 0.8051  0.002 ** 
# T_ecart      0.99941  0.03441 0.7642  0.004 ** 
# H_moy       -0.99282 -0.11964 0.5721  0.017 *  
# H_min       -0.99772 -0.06756 0.6324  0.009 ** 
# H_max        0.11464 -0.99341 0.0190  0.918    
# H_ecart      0.99957  0.02947 0.7302  0.004 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999
save(fit3,file="E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/fit3.Rdata" )


#◘ sur données moyennée par sites puis fit des données environementale a partir de cette NMDS, Presence / Absense Data	PA

MDSres4 <- metaMDS(ComMatrix_persites_PresenceAbs,try=1000) 
# Call:
# metaMDS(comm = ComMatrix_persites_PresenceAbs, try = 1000) 

# global Multidimensional Scaling using monoMDS

# Data:     ComMatrix_persites_PresenceAbs 
# Distance: bray 

# Dimensions: 2 
# Stress:     0.05622952 
# Stress type 1, weak ties
# Two convergent solutions found after 1000 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on ‘ComMatrix_persites_PresenceAbs’
save(MDSres4,file="H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/MDSres4.Rdata" )


fit4 <- envfit(MDSres4, envTab_persites, perm = 999) 
# ***VECTORS

               # NMDS1    NMDS2     r2 Pr(>r)    
# herba        0.92227  0.38655 0.4368  0.077 .  
# arbus        0.76082  0.64896 0.4998  0.050 *  
# arbo        -0.94527 -0.32630 0.5041  0.069 .  
# sol_nu       0.94027 -0.34042 0.6847  0.008 ** 
# veg_height  -0.99901  0.04439 0.8887  0.001 ***
# nb_tiges    -0.95985  0.28052 0.4520  0.088 .  
# dbh_moy     -0.91567 -0.40194 0.6756  0.011 *  
# Canopy_open  0.99701  0.07726 0.8288  0.001 ***
# rich_flore   0.06586  0.99783 0.0619  0.759    
# T_moy        0.84739  0.53096 0.8668  0.001 ***
# T_min       -0.09205  0.99575 0.1994  0.385    
# T_max        0.99749 -0.07075 0.7859  0.001 ***
# T_ecart      0.92967 -0.36838 0.7254  0.004 ** 
# H_moy       -0.99999 -0.00465 0.5064  0.040 *  
# H_min       -0.99768  0.06812 0.6056  0.015 *  
# H_max        0.19813 -0.98018 0.0109  0.964    
# H_ecart      0.99371 -0.11203 0.7083  0.006 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999
save(fit4,file="H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/fit4.Rdata" )


#◘ sur données moyennée par sites ET espèces ACOUSTIQUE puis fit des données environementale a partir de cette NMDS, données presence absence   # CONSERVE POUR ARTICLE
MDSres5 <- metaMDS(ComMatrix_persites_Acou_PresenceAbs,try=1000) 
# Call:
# metaMDS(comm = ComMatrix_persites_Acou_PresenceAbs, try = 1000) 

# global Multidimensional Scaling using monoMDS

# Data:     ComMatrix_persites_Acou_PresenceAbs 
# Distance: bray 

# Dimensions: 2 
# Stress:     0.02586118 
# Stress type 1, weak ties
# Two convergent solutions found after 1000 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on ‘ComMatrix_persites_Acou_PresenceAbs’ 
save(MDSres5,file="H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/MDSres5.Rdata" )

fit5 <- envfit(MDSres5, envTab_persites, perm = 999) 
# ***VECTORS

               # NMDS1    NMDS2     r2 Pr(>r)    
# herba        0.96564  0.25990 0.4479  0.074 .  
# arbus        0.70696  0.70725 0.6168  0.010 ** 
# arbo        -0.87231 -0.48896 0.5480  0.022 *  
# sol_nu       0.99604 -0.08893 0.6497  0.009 ** 
# veg_height  -0.90191  0.43192 0.7913  0.002 ** 
# nb_tiges    -0.91949 -0.39311 0.4811  0.042 *  
# dbh_moy     -0.99224  0.12436 0.5200  0.037 *  
# Canopy_open  0.99264  0.12107 0.8045  0.001 ***
# rich_flore  -0.05743  0.99835 0.2109  0.351    
# T_moy        0.99739  0.07215 0.6330  0.012 *  
# T_min       -0.44128  0.89737 0.1133  0.589    
# T_max        0.95683 -0.29064 0.7146  0.006 ** 
# T_ecart      0.92013 -0.39162 0.7192  0.003 ** 
# H_moy       -0.86455  0.50254 0.5020  0.050 *  
# H_min       -0.92396  0.38249 0.5701  0.023 *  
# H_max        0.59713  0.80215 0.0159  0.930    
# H_ecart      0.93572 -0.35275 0.6758  0.009 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999
save(fit5,file="H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/fit5.Rdata" )


#◘ ADONIS


# Test pour connaitre l'effet du facteur habitat sur la structuration des assemblages: on le fait avec les données brute mais en design block (pour prendre en compte les parcelles) 
adonis(ComMatrix~substr(rownames(ComMatrix),1,2),group=substr(rownames(ComMatrix),1,4))
# Call:
# adonis(formula = ComMatrix ~ substr(rownames(ComMatrix), 1, 2),      group = substr(rownames(ComMatrix), 1, 4)) 
# Permutation: free
# Number of permutations: 999
# Terms added sequentially (first to last)
                                  # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# substr(rownames(ComMatrix), 1, 2)  2    3.7167 1.85836  9.9656 0.48694  0.001 ***
# Residuals                         21    3.9160 0.18648         0.51306           
# Total                             23    7.6327                 1.00000
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Test pour connaitre l'effet du facteur habitat sur la structuration des assemblages: avec les données moyennées par sites (pas de block) # CONSERVE POUR ARTICLE
adonis2(ComMatrix_persites~substr(rownames(ComMatrix_persites),1,2)) 
# Permutation test for adonis under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999
# adonis2(formula = ComMatrix_persites ~ substr(rownames(ComMatrix_persites), 1, 2))
                                           # Df SumOfSqs      R2      F Pr(>F)   
# substr(rownames(ComMatrix_persites), 1, 2)  2   1.9523 0.52154 4.9052  0.004 **
# Residual                                    9   1.7910 0.47846                 
# Total                                      11   3.7434 1.00000                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# Test pour connaitre l'effet du facteur habitat sur la structuration des assemblages: avec les données moyennées par sites (pas de block) sur données presence absence 
adonis2(ComMatrix_persites_PresenceAbs~substr(rownames(ComMatrix_persites_PresenceAbs),1,2))
# Permutation test for adonis under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999

# adonis2(formula = ComMatrix_persites_PresenceAbs ~ substr(rownames(ComMatrix_persites_PresenceAbs), 1, 2))
                                                       # Df SumOfSqs      R2      F Pr(>F)    
# substr(rownames(ComMatrix_persites_PresenceAbs), 1, 2)  2   1.9338 0.55052 5.5116  0.001 ***
# Residual                                                9   1.5789 0.44948                  
# Total                                                  11   3.5127 1.00000                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





# Test pour connaitre l'effet du facteur habitat sur la structuration des assemblages: avec les données moyennées par sites (pas de block) et sur espèces SONORES # CONSERVE POUR ARTICLE
adonis2(ComMatrix_persites_Acou~substr(rownames(ComMatrix_persites_Acou),1,2)) 
# Permutation test for adonis under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999

# adonis2(formula = ComMatrix_persites_Acou ~ substr(rownames(ComMatrix_persites_Acou), 1, 2))
                                                # Df SumOfSqs      R2     F Pr(>F)   
# substr(rownames(ComMatrix_persites_Acou), 1, 2)  2   1.9553 0.52696 5.013  0.006 **
# Residual                                         9   1.7552 0.47304                
# Total                                           11   3.7105 1.00000                
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Test pour connaitre l'effet du facteur habitat sur la structuration des assemblages: avec les données moyennées par sites (pas de block) et sur espèces SONORES sur données presence absence 
adonis2(ComMatrix_persites_Acou_PresenceAbs~substr(rownames(ComMatrix_persites_Acou_PresenceAbs),1,2)) 
# Permutation test for adonis under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999

# adonis2(formula = ComMatrix_persites_Acou_PresenceAbs ~ substr(rownames(ComMatrix_persites_Acou_PresenceAbs), 1, 2))
                                                            # Df SumOfSqs      R2      F Pr(>F)    
# substr(rownames(ComMatrix_persites_Acou_PresenceAbs), 1, 2)  2   2.0056 0.60001 6.7503  0.001 ***
# Residual                                                     9   1.3370 0.39999                  
# Total                                                       11   3.3427 1.00000                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


########################################################################################################################
# Figure 2: NMDS graphics
load("E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/fit.Rdata" )
load("E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/MDSres.Rdata")

load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/fit2.Rdata" )
load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/MDSres2.Rdata")

load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/fit3.Rdata" )
load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/MDSres3.Rdata")

load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/fit4.Rdata" )
load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/MDSres4.Rdata")

load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/fit5.Rdata" )
load("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/MDSres5.Rdata")

CoordEnv<-fit$vectors$arrows
CoordSpecies<-MDSres$species
CoordSites<-MDSres$points
pdf("E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/NMDS.pdf")
plot(CoordSpecies)
plot(fit)
s.class(CoordSites,as.factor(substr(rownames(CoordSites),1,2)),add.plot=TRUE)
dev.off()

CoordEnv2<-fit2$vectors$arrows
CoordSpecies2<-MDSres2$species
CoordSites2<-MDSres2$points
pdf("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/NMDS_meanperSite.pdf")
plot(CoordSpecies2,ylim=c(-1.1,1.1),xlim=c(-1.8,1.8))
plot(fit2)
s.class(CoordSites2,as.factor(substr(rownames(CoordSites2),1,2)),add.plot=TRUE)
dev.off()

pdf("E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/NMDS_meanperSite_spnames.pdf")
plot(MDSres2,type="t")
dev.off()

CoordEnv3<-fit3$vectors$arrows
CoordSpecies3<-MDSres3$species
CoordSites3<-MDSres3$points
pdf("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/NMDS_meanperSite_Acou.pdf")
plot(CoordSpecies3,ylim=c(-1.1,1.1),xlim=c(-1.8,1.8))
plot(fit3)
s.class(CoordSites3,as.factor(substr(rownames(CoordSites3),1,2)),add.plot=TRUE)
dev.off()

pdf("E:/projets/NouvelleCaledonie_2014_projetAnso/code R/ArticleEcology/sortie R/NMDS_meanperSite_Acou_spnames.pdf")
plot(MDSres3,type="t")
dev.off()


CoordEnv4<-fit4$vectors$arrows
CoordSpecies4<-MDSres4$species
CoordSites4<-MDSres4$points
pdf("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/NMDS_meanperSite_PA.pdf")
plot(CoordSpecies4,ylim=c(-1.1,1.1),xlim=c(-1.8,1.8))
plot(fit4)
s.class(CoordSites4,as.factor(substr(rownames(CoordSites4),1,2)),add.plot=TRUE)
dev.off()

pdf("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/NMDS_meanperSite_spnames_PA.pdf")
plot(MDSres4,type="t")
dev.off()

CoordEnv5<-fit5$vectors$arrows
CoordSpecies5<-MDSres5$species
CoordSites5<-MDSres5$points
pdf("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/NMDS_meanperSite_Acou_PA.pdf")
plot(CoordSpecies5,ylim=c(-1.1,1.1),xlim=c(-1.8,1.8))
plot(fit5)
s.class(CoordSites5,as.factor(substr(rownames(CoordSites5),1,2)),add.plot=TRUE)
dev.off()

pdf("H:/projets/NouvelleCaledonie_2014_projetAnso/2-code R/Figure pour ArticleEcology/sortie R/NMDS_meanperSite_Acou_spnames_PA.pdf")
plot(MDSres5,type="t")
dev.off()



}



########################################################################################################################
# Analyse 5: turnover (βSIM) and nestedness (βNES)
{
library(betapart)
beta.multi (ComMatrix_persites_PresenceAbs)
# $beta.SIM
# [1] 0.8073394
# $beta.SNE
# [1] 0.06728742
# $beta.SOR
# [1] 0.8746269


ComMatrix_persites_PresenceAbs_habitat<-ComMatrix_persites_PresenceAbs
ComMatrix_persites_PresenceAbs_habitat<-rbind(
apply(ComMatrix_persites_PresenceAbs[1:4,],2, sum),
apply(ComMatrix_persites_PresenceAbs[5:8,],2, sum),
apply(ComMatrix_persites_PresenceAbs[9:12,],2, sum))
rownames(ComMatrix_persites_PresenceAbs_habitat)<-c("FO","MA","PA")
ComMatrix_persites_PresenceAbs_habitat[ComMatrix_persites_PresenceAbs_habitat>0]<-1

beta.multi (ComMatrix_persites_PresenceAbs_habitat)
# $beta.SIM
# [1] 0.5666667
# $beta.SNE
# [1] 0.1310078
# $beta.SOR
# [1] 0.6976744


}

########################################################################################################################
# Analyse 6: Species signular to habitat SIMPEr
{
SIMPER<-simper(ComMatrix_persites,group=as.factor(substr(rownames(ComMatrix_persites),1,2)))
summary(SIMPER)
# cumulative contributions of most influential species:
# $FO_MA
       # Bullita fusca Agnotecous azurensis    Koghiella flammea 
           # 0.2899719            0.5511518            0.7408589 
# $FO_PA
       # Bullita fusca Agnotecous azurensis      Bullita obscura     Koghiella nigris 
           # 0.2784923            0.4599933            0.6034177            0.7104244 
# $MA_PA
# Agnotecous azurensis    Koghiella flammea        Bullita fusca      Bullita obscura 
           # 0.2787241            0.4959808            0.6921351            0.8272713 
		   
SIMPER2<-simper(ComMatrix_persites_PresenceAbs,group=as.factor(substr(rownames(ComMatrix_persites_PresenceAbs),1,2)))
summary(SIMPER2)
# cumulative contributions of most influential species:

# $FO_MA
       # Agnotecous azurensis          Caltathra meunieri           Koghiella flammea           MOGOPLISTIDAE sp3               Bullita fusca           MOGOPLISTIDAE sp2         Pixibinthus sonicus       Pseudotrigonidium ana            Koghiella nigris       Bullita mouirangensis Adenopterus crouensis (sp1) 
                  # 0.1023384                   0.2046767                   0.3070151                   0.3790250                   0.4417056                   0.4870824                   0.5324591                   0.5777949                   0.6218285                   0.6614863                   0.6918148 
          # Agnotecous clarus 
                  # 0.7185035 
# $FO_PA
        # Agnotecous clarus        Caltathra meunieri     Pseudotrigonidium ana          Koghiella nigris         MOGOPLISTIDAE sp2             Bullita fusca     Bullita mouirangensis         Kanakinemobius sp       Caltathra balmessae Notosciobia minoris (sp2)       Pixibinthus sonicus          TRIGONIDIINAE sp 
               # 0.09014526                0.15756955                0.22024796                0.28026311                0.33526553                0.38752371                0.43023763                0.46771984                0.50424758                0.54077531                0.57598255                0.61118980 
     # Agnotecous azurensis           Bullita obscura         MOGOPLISTIDAE sp1 
               # 0.64340684                0.67562389                0.70784093 
# $MA_PA
    # Agnotecous clarus     Koghiella flammea  Agnotecous azurensis         Bullita fusca     MOGOPLISTIDAE sp3 Pseudotrigonidium ana     MOGOPLISTIDAE sp2   Pixibinthus sonicus    Caltathra meunieri      Koghiella nigris 
            # 0.1134112             0.2072800             0.2906210             0.3739620             0.4542919             0.5339748             0.5893580             0.6442030             0.6938157             0.7434284
			}