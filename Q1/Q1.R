# installation et chargement des librairies
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.15")
BiocManager::install(c("gRain", "gRbase","graph","RBGL","Rgraphviz"))

library("gRain", "gRbase", "Rgraphviz")

# Création du grapheNEL
dag_solvency = dag(
  c("R"),
  c("Ac", "R"),
  c("DvsR", "Ag"),
  c("H", "DvsR"),
  c("Ag"),
  c("Fi", "H", "Ag"),
  c("Rf", "Ac"),
  c("S", "Fi", "Rf", "DvsR"),
  result = "graphNEL"
)

# Affichage des caractéristiques du graphe
dag_solvency

# Affichage du graphe 
plot(dag_solvency)

# Création du réseau bayésien en spécifiant les probabilités associés à chaque noeud
val_F_M_E = c("Faible","Moyen", "Elevé") 
val_Age = c("-25","25-50", "50-65", "+65")
val_B_M = c("Mauvais", "Bon")   
val_F_NF = c("Non fiable", "Fiable")   
val_S_NS = c("Non Solvable", "solvable")

cp_R <- cptable(~R,values=c(1/3, 1/3, 1/3),levels=val_F_M_E)
cp_Ag <- cptable(~Ag,values=c(1/4, 1/4,1/4, 1/4),levels=val_Age)
cp_Ac <- cptable(~Ac|R,values=c(0.8,0.15,0.05, 0.15,0.8,0.05, 0.05,0.15,0.8),levels=val_F_M_E)
cp_Rf <- cptable(~Rf|Ac,values=c(0.7,0.2,0.1, 0.1,0.6,0.3, 0.1,0.2,0.7),levels=val_F_M_E)
cp_DvsR <- cptable(~DvsR|Ag,values=c(0.4,0.3,0.3, 0.5, 0.3, 0.2, 0.6,0.3,0.1, 0.7,0.2,0.1),levels=val_F_M_E)
cp_H <- cptable(~H|DvsR,values=c(0.1,0.9, 0.35,0.65, 0.5,0.5),levels=val_B_M)
cp_Fi <- cptable(~Fi|Ag+H,values=c(0.6,0.4, 0.3,0.7, 0.2,0.8, 0.1,0.9,  
                                   0.5,0.5, 0.2,0.8, 0.1,0.9, 0,1), levels=val_F_NF) 
cp_S <- cptable(~S|Fi+Rf+DvsR,values=c(0.7,0.3, 0.5,0.5,
                                       0.6,0.4, 0.4, 0.6,
                                       0.5,0.5, 0.3, 0.7,
                                       0.6,0.4, 0.4, 0.6,
                                       0.6,0.4, 0.3, 0.7,
                                       0.5,0.5, 0.2, 0.8,
                                       0.7,0.3, 0.4, 0.6,
                                       0.5,0.5, 0.2, 0.8,
                                       0.4,0.6, 0.1, 0.9), levels=val_S_NS) 

net_list = compileCPT(list(cp_R,cp_Ag, cp_Ac, cp_Rf, cp_DvsR, cp_H, cp_Fi, cp_S))

# Création d'un objet de type grain (GRAphical Independence Network)
grain_solvency = grain(net_list)
plot(grain_solvency$dag)

# 1.Un(e) client(e) avec un bon historique de paiement a tendance à être plus fiable : 
querygrain(grain_solvency, nodes=c("Fi","H"), type="conditional")

# 2. Plus un(e) client(e) est âgé(e), plus il/elle a de chance d’être fiable : 
querygrain(grain_solvency, nodes=c("Fi","Ag"), type="conditional")

# 3. Les clients plus âgés ont tendance à avoir un faible ratio dettes vs revenus :
querygrain(grain_solvency, nodes=c("DvsR","Ag"), type="conditional")

# 4. La probabilité d’avoir un bon historique de paiement augmente au fur et à mesure que le ratio de dette vs revenus diminue : 
querygrain(grain_solvency, nodes=c("H","DvsR"), type="conditional")

# 5. Plus les revenus d’une personne sont élevés, plus cette personne a de chance d’avoir des actifs élevés : 
querygrain(grain_solvency, nodes=c("Ac","R"), type="conditional")

# 6. Plus une personne a d’actifs, plus cette personne a de chance d’avoir un revenu élevé dans le futur :
querygrain(grain_solvency, nodes=c("Rf","Ac"), type="conditional")

# 7. Une personne fiable a tendance à être plus solvable qu’une personne non fiable :
querygrain(grain_solvency, nodes=c("S","Fi"), type="conditional")

# 8. Les personnes qui ont des revenus prometteurs ont + de chance d’être solvables que celles dont la perspective des revenus à venir est mauvaise :
querygrain(grain_solvency, nodes=c("S","Rf"), type="conditional")
