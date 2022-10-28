# import of data
load("./Q3/ApportCalorique.RData") # dans variable a

# Pour maximiser notre 1ere vraisemblance sur IMC
# Cherchons les parametres muIMC et sigma2IMC
# D'apres la partie théorique du DM nous avons deja les resultats estimees
# il suffit d'implementer ce resultat

muIMC = mean(a$IMC)
sigma2IMC = var(a$IMC)

# Il faut savoir que lorsqu'on applique le calcul de la variance et 
# la covariance numerique sur un echantillon disperse autour de la moyenne, 
# il faut prendre en consideration le biais introduit, on doit donc diviser 
# par n-1 au lieu de n où n est la taille de l'echantillon, comme nous avons
# beaucoup de valeurs ici, le resultat ne sera pas vraiment biaise


# Meme logique pour la 2e vraisemblance sur Phys
muPhys = mean(a$Phys)
sigma2Phys = var(a$Phys)


# 3e vraisemblance : B|IMC
betaB_IMC = cov(a$B, a$IMC)/sigma2IMC
muB_IMC = mean(a$B)
betaB_0 = muB_IMC - betaB_IMC*muIMC
sigma2B_IMC = var(a$B)-betaB_IMC*betaB_IMC*sigma2IMC


# 4e vraisemblance : H|Phys
betaH_Phys = cov(a$H, a$Phys)/sigma2Phys
muH_Phys = mean(a$H)
betaH_0 = muH_Phys - betaH_Phys*muPhys
sigma2H_Phys = var(a$H) - betaH_Phys*betaH_Phys*sigma2Phys


# 5e vraisemblance : C|IMC,Phys
cov_IMC_Phys = cov(a$IMC, a$Phys)
cov_C_Phys = cov(a$C, a$Phys)
cov_C_IMC = cov(a$C, a$IMC)
sigma2C = var(a$C)

betaC_bot = cov_IMC_Phys*cov_IMC_Phys - sigma2IMC*sigma2Phys
betaC_IMC = (cov_C_Phys*cov_IMC_Phys-cov_C_IMC*sigma2Phys)/betaC_bot
betaC_Phys = (cov_C_IMC*cov_IMC_Phys-cov_C_Phys*sigma2IMC)/betaC_bot
muC_IMC_Phys = mean(a$C)
betaC_0 = muC_IMC_Phys - betaC_IMC*muIMC - betaC_Phys*muPhys
sigma2C_IMC_Phys = (sigma2C
                    - betaC_IMC*betaC_IMC*sigma2IMC
                    - betaC_Phys*betaC_Phys*sigma2Phys
                    - 2*betaC_IMC*betaC_Phys*cov_IMC_Phys)


# 6e vraisemblance : Ap|H,Phys,B
cov_Ap_B = cov(a$Ap, a$B)
cov_Ap_H = cov(a$Ap, a$H)
cov_Ap_Phys = cov(a$Ap, a$Phys)
cov_H_B = cov(a$H, a$B)
cov_H_Phys = cov(a$H, a$Phys)
cov_Phys_B = cov(a$Phys, a$B)
sigma2B = var(a$B)
sigma2H = var(a$H)

betaAp_B_num = (cov_Ap_B*(cov_H_Phys*cov_H_Phys-sigma2H*sigma2Phys)
                +cov_Ap_H*(cov_H_B*sigma2Phys-cov_Phys_B*cov_H_Phys)
                -cov_Ap_Phys*(cov_H_B*cov_H_Phys-cov_Phys_B*sigma2H))
betaAp_B_dem = (cov_H_B*cov_H_B*sigma2Phys-2*cov_H_B*cov_Phys_B*cov_H_Phys
                +cov_Phys_B*cov_Phys_B*sigma2H
                +sigma2B*(cov_H_Phys*cov_H_Phys-sigma2H*sigma2Phys))
betaAp_B = betaAp_B_num/betaAp_B_dem
betaAp_Phys_top = (cov_Ap_H*cov_H_Phys-cov_Ap_Phys*sigma2H
                  -betaAp_B*(cov_H_B*cov_H_Phys-cov_Phys_B*sigma2H))
betaAp_Phys = betaAp_Phys_top/(cov_H_Phys*cov_H_Phys-sigma2Phys*sigma2H)
betaAp_H = (cov_Ap_H-betaAp_Phys*cov_H_Phys-betaAp_B*cov_H_B)/sigma2H
muAp_H_Phys_B = mean(a$Ap)
betaAp_0 = (muAp_H_Phys_B - betaAp_H*muH_Phys
           - betaAp_Phys*muPhys - betaAp_B*mean(a$B))
sigma2Ap_H_Phys_B = (var(a$Ap)-betaAp_H*betaAp_H*sigma2H
                     -betaAp_Phys*betaAp_Phys*sigma2Phys
                     -betaAp_B*betaAp_B*sigma2B
                     -2*betaAp_H*betaAp_Phys*cov_H_Phys
                     -2*betaAp_H*betaAp_B*cov_H_B
                     -2*betaAp_Phys*betaAp_B*cov_Phys_B)

# 7e vraisemblance : A|C,Phys,Ap
cov_A_Ap = cov(a$A, a$Ap)
cov_A_C = cov(a$A, a$C)
cov_A_Phys = cov(a$A, a$Phys)
cov_C_Ap = cov(a$C, a$Ap)
cov_C_Phys = cov(a$C, a$Phys)
cov_Phys_Ap = cov(a$Phys, a$Ap)
sigma2Ap = var(a$Ap)

betaA_Ap_num = (cov_A_Ap*(cov_C_Phys*cov_C_Phys-sigma2C*sigma2Phys)
                +cov_A_C*(cov_C_Ap*sigma2Phys-cov_Phys_Ap*cov_C_Phys)
                -cov_A_Phys*(cov_C_Ap*cov_C_Phys-cov_Phys_Ap*sigma2C))
betaA_Ap_dem = (cov_C_Ap*cov_C_Ap*sigma2Phys-2*cov_C_Ap*cov_Phys_Ap*cov_C_Phys
                +cov_Phys_Ap*cov_Phys_Ap*sigma2C
                +sigma2Ap*(cov_C_Phys*cov_C_Phys-sigma2C*sigma2Phys))
betaA_Ap = betaA_Ap_num/betaA_Ap_dem
betaA_Phys_top = (cov_A_C*cov_C_Phys-cov_A_Phys*sigma2C
                  -betaA_Ap*(cov_C_Ap*cov_C_Phys-cov_Phys_Ap*sigma2C))
betaA_Phys = betaA_Phys_top/(cov_C_Phys*cov_C_Phys-sigma2Phys*sigma2C)
betaA_C = (cov_A_C-betaA_Phys*cov_C_Phys-betaA_Ap*cov_C_Ap)/sigma2C
muA_C_Phys_Ap = mean(a$A)
betaA_0 = (muA_C_Phys_Ap - betaA_C*mean(a$C)
           - betaA_Phys*muPhys - betaA_Ap*mean(a$Ap))
sigma2A = var(a$A)
sigma2A_C_Phys_Ap = (sigma2A-betaA_C*betaA_C*sigma2C
                     -betaA_Phys*betaA_Phys*sigma2Phys
                     -betaA_Ap*betaA_Ap*sigma2Ap
                     -2*betaA_C*betaA_Phys*cov_C_Phys
                     -2*betaA_C*betaA_Ap*cov_C_Ap
                     -2*betaA_Phys*betaA_Ap*cov_Phys_Ap)

