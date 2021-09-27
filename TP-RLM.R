####### EXERCICE 2 ######## RLM

###2
setwd(dir = "C:/Users/JAGHDAM/Desktop")
data=read.table("TD-RLM.csv",sep=";",header=TRUE)
print(data)

cste=rep(1,14)
X=cbind(cste=cste,x1=data[,2],x2=data[,3],x3=data[,4])
X
U=cbind(cst=rep(1,14),data[,c(2,3,4)])
dim(U)
U
solve(t(U) %*% U)

###3
# La transposee de X
# t(X)
solve(t(X) %*% X)
###4
Y=data[,1]
Y
t(X)%*%Y
###5
# betaChap = (X^t X)^(-1) X^t y
BetaChap <- solve(t(X) %*% X) %*% (t(X) %*% Y)
BetaChap
yChap <- X %*%BetaChap
###6
residus <- Y - yChap
residus
sum(residus)
SCR=sum(residus^2)

varErrChap=SCR/(14-3-1)
sigmaChap=sqrt(varErrChap)
sigmaChap
###7  la matrice de variance covariance 
varcov= varErrChap * solve(t(X) %*% X)
varcov
# Les erreurs-types sont les racines des elements diagonaux
SigmaBetas = sqrt(diag(varcov))
SigmaBetas

###8
SCT= sum((Y-mean(Y))^2)
R2 =1- SCR / SCT
R2
#On a calculé le coefficient de détermination, calculons à présent le coefficient de corrélation ajusté, 
#qui vient apporter une pénalité au R²
#afin de prendre en compte le nombre de variables explicatives incluses dans le modèle. Il est défini comme
#suit (non abordé en cours):
m = 3
# Nombre de variables explicatives
n=14 # taille de l'éch
R2a = 1 - ((n-1) / (n-m-1)) * (1-R2)
R2a
###9
tObs = BetaChap / SigmaBetas
tObs
# On doit comparer les t_obs a la valeur tabulee d'une
# Student a (n-m-1) d.d.l. pour un risque de premiere espece
# alpha = 5%
n=14
T_tab= qt(p = 1-0.05/2, df = n-m-1)
T_tab
ifelse(abs(tObs) > T_tab, "Rejet H_0", "Non rejet H_0")
#La p-value (probabilité d'obtenir une valeur au moins aussi grande de la statistique observée, 
#si l'hypothèse nulle est vraie) associée à chaque test est la suivante :
2*pt(q = abs(tObs), df = n-m-1, lower.tail=FALSE)
###10
# La valeur calculee
F_obs = (R2 / m) / ((1-R2) / (n-m-1))
F_obs
# La valeur tabulee est :
F_tab = qf(p = 1-0.05, df1 = m, df2 = n-m-1)
F_tab
# p-value
1 - pf(q = F_obs, df1 = m, df2 = n-m-1)
# donc on rejette H_0 et le modèle est globalement significatif
###11
X_0=c(1,7,20,150)
X_0
y_0Chap=BetaChap[1,]+BetaChap[2,]*7+BetaChap[3,]*20+BetaChap[4,]*150
y_0Chap

binf=y_0Chap-qt(1-0.05/2,df=10)*sigmaChap*sqrt(1+t(X_0)%*%solve(t(X)%*%X)%*%(X_0))
bsup=y_0Chap+qt(1-0.05/2,df=10)*sigmaChap*sqrt(1+t(X_0)%*%solve(t(X)%*%X)%*%(X_0))
c(binf,bsup)
###12
hist(residus,probability = TRUE)
# estimer la densité que représente ces différentes valeurs
lines(density(residus), col = "red",lwd=1) # Superposer une ligne de densité à l'histogramme
###13
modele <- lm(y~ x1+x2+x3,data=data)
summary(modele)
summary(modele)$coefficients
BetaChap
SigmaBetas
anova(modele)
plot(predict(modele),residus)
abline(h=c(-3,0,3),lty=c(2,1,2),col=c(1,2,1))
predict(modele,data.frame(x1=7,x2=20,x3=150),interval='prediction')


