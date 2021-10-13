#' ---
#' title: "Mini-projet"
#' output: html_document
#' author: Eric Chen
#' ---

#' **Exercice 1 : ** 
#' 
#'  **1)**
#'  On utilise l'estimateur de maximum de vraisemblance sur une loi géométrique. On pose X = $(X_1,...,X_n)$
#'  où les $X_i$ sont indépendantes et identiquement distribuées.
#'  On obtient donc:
#'  
#'  $log(p(x,t)) = nlog(t) \times \sum_{i=1}^{n} x_i log(1-t)$
#'  
#'  On veut : 
#'  $\frac{\partial log(p(x,t))}{\partial t} = 0$
#'  
#'  Donc on obtient:
#'  
#'  $\hat\theta = \frac{1}{1+\frac{\sum_{i=1}^{n} x_i}{n}}$
#'  
theta = 1/(1+(sum(discoveries))/length(discoveries)) 
print(theta)
#' 
#' **2)**
#' On utilise l'estimateur de maximum de vraisemblance sur un modèle de Poisson de paramètre $\lambda$
#' 
#' On obtient donc $log(p(x,t)) = log(t)\sum_{i=1}^{n} x_i - nt -\sum_{i=1}^{n} log(x_i!)$ 
#' 
#' On veut $\frac{\partial log(p(x,t))}{\partial t} = 0$
#' 
#' Donc on obtient : $\hat\lambda = \frac{\sum_{i=1}^{n} x_i}{n}$
#' 
lambda = sum(discoveries)/length(discoveries)
print(lambda)
#' 
#' **3)**
#' On veut comparer les moyennes et variances empiriques du jeu de données avec les espérances et variances théoriques dans les deux modèles considérés.
#' 
#' On obtient pour le jeu de données:
#' 
#' m = $\frac{\sum_{i=1}^{n} x_i}{n}$
#' 
#' $V_e = \frac{1}{n} \sum_{i=1}^{n} (x_i - m)^2$
m = sum(discoveries)/length(discoveries)
Ve = (1/length(discoveries))*sum((discoveries-m)^2)
print(m)
print(Ve)
#' 
#' Pour le modèle géométrique, on a :
#' 
#' $E_g(x) = \frac{1}{\theta}$
#' 
#' $V_g(x) = \frac{1-\theta}{\theta^2}$
Eg = 1/theta
Vg = (1-theta)/(theta^2)
print(Eg)
print(Vg)
#' 
#' Pour le modèle de Poisson, on a :
#' 
#' $E_P(x) = \lambda$
#' 
#' $V_P(x) = \lambda$
Ep = lambda
Vp = lambda
print(Ep)
print(Vp)
#' 
#' En comparant les différentes espérances et variances des modèles, on constate que c'est le modèle de Poisson qui est le plus proche des résultats obtenus avec le jeu de données. Le modèle le plus approprié à première vue est donc le modèle de Poisson.
#' 
#' **4)**
#' 
x_ = seq(0, 12)
dg = dgeom(x = x_, prob = theta)
dp = dpois(x = x_,lambda = lambda)
hist(discoveries, breaks = 12, probability = TRUE, main ="Histogramme des données")
lines(x = x_, y = dg, col='red')
lines(x = x_, y = dp, col='blue')
legend("topright",
       legend =c("densité de la loi géométrique","densité de la loi de Poisson"),  
       lty=1, ## pour avoir des traits pleins
       col=c("red","blue"))
#' 
#' On constate que la première impression n'est pas confirmée. En effet, en observant certaines parties de l'histogramme, un modèle peut être plus "adapté" pour suivre les données qu'un autre.
#' 
#' **5)**
#' 
xx = seq(0, length(discoveries), length.out = length(discoveries))
yy = xx/101
qqplot(discoveries,qgeom(yy,theta), main ="QQ plot discoveries et loi géométrique")
qqplot(discoveries,qpois(yy,lambda), main="QQ plot discoveries et loi de Poisson")
#' On constate que les points des qq-plots associés aux lois sont majoritairement proches de la diagonale. On ne peut pas vraiment tirer de conclusion sur la loi des $X_i$.
#' 
#' **6)**
#' Le degré de liberté de chacun des modèles est obtenu par la relation k - 1 - p où p est la dimension du modèle paramétrique. On obtient pour chacun des modèles 4 degrés de liberté.
#' 
#' On calcule ensuite la valeur de la statistique S qui suit une loi du $\chi^2$ donnée dans l'énoncé : S = $\sum_{j=1}^{k} \frac{(n_j - np_j)^2}{np_j}$
#' 
#' Pour chaque modèle, la p-valeur est déterminée par $\mathbb{P}(S ≥ s)$ où S suit une loi du $\chi^2$ à 4 degrés de liberté et s la valeur calculée grâce à la formule ci-dessus.
#' 
#' On crée un vecteur n contenant les $n_j$ pour discoveries.
n = vector("numeric",6)
for (i in 1:5){
  n[i] = sum(discoveries == i-1)
}
n[6] = sum(discoveries >= 5)
#' 
#' On détermine $s_{geom}$ et la p-value pour le modèle géométrique :
p_geom = vector("numeric",6)
for (i in 1:5){
  p_geom[i] = theta*(1-theta)^(i-1)
}
p_geom[6] = 1 - sum(p_geom)

s_geom = 0
for (i in 1:6){
  s_geom = s_geom + ((n[i] - length(discoveries)*p_geom[i])^2)/(length(discoveries)*p_geom[i])
}
pvaleur_geom = 1 - pchisq(s_geom,4)
print(s_geom)
print(pvaleur_geom)
#' 
#' On détermine $s_{pois}$ et la p-valeur pour le modèle de Poisson :
p_pois = vector("numeric",6)
for (i in 1:5){
  p_pois[i] = lambda^(i-1)*exp(-lambda)/factorial(i-1)
}
p_pois[6] = 1 - sum(p_pois)

s_pois = 0
for (i in 1:6){
  s_pois = s_pois + ((n[i] - length(discoveries)*p_pois[i])^2)/(length(discoveries)*p_pois[i])
}
pvaleur_pois = 1 - pchisq(s_pois,4)
print(s_pois)
print(pvaleur_pois)
#' On constate que la p-valeur est inférieure à 0.05 pour le modèle géométrique mais supérieure à 0.05 pour le modèle de Poisson.
#' 
#' Ainsi, par rapport à la définition de la p-valeur, le modèle que l'on peut accepter avec un niveau de confiance 5% est le modèle de Poisson.
#' En effet, une faible valeur pour la p-valeur suppose une forte présomption contre l'hypothèse nulle.
#' 
#' **Exercice 2 **
#' 
#' **1)**
#' Soit s $\geq$ 1. On veut montrer la croissance de la fonction $\lambda$ $\longmapsto$ $\mathbb{P}_\lambda$($\sum_{i=1}^{n} X_i$ $\geq$ s) 
#' 
#' On sait que les $X_i$ sont indépendantes et identiquement distribuées, suivent une loi de poisson de paramètre $\lambda$. Ainsi, Y = $\sum_{i=1}^{n} X_i$ suit une loi de Poisson de paramètre n$\lambda$.
#' 
#' $\mathbb{P}_\lambda$(Y $\geq$ s) = 1 - $\mathbb{P}_\lambda$(Y < s)
#' 
#' $\frac{\partial \mathbb{P}_\lambda(Y \geq s)}{\partial \lambda} = n  exp(-n\lambda)(\sum_{0 \leq i < s} \frac{(n\lambda)^i}{i!} - \sum_{0 \leq i < s-1} \frac{(n\lambda)^i}{i!})$
#' 
#' $\frac{\partial \mathbb{P}_\lambda(Y \geq s)}{\partial \lambda} = n  exp(-n\lambda)\frac{(n\lambda)^M}{M!}$  où  M$\in\mathbb{N}*$
#' 
#' Donc $\frac{\partial \mathbb{P}_\lambda(Y \geq s)}{\partial \lambda} \geq 0$. La fonction est bien croissante.
#' 
#' **2)**
#' On pose T = $\sum_{i=1}^{n} X_i$ qui suit une loi de Poisson de paramètre n$\lambda$.
#' 
#' En calculant le rapport de vraisemblance, on obtient Z = exp(n($\lambda-\lambda'$)+Tlog($\frac{\lambda'}{\lambda}$)).  
#' Il s'agit d'une fonction croissante de T lorsque $\lambda' > \lambda$. 
#'
#' Le test de niveau $\alpha$ de l'hypothèse $H_0$ contre $H_1$ est donné par:
#' 
#' $\delta(T) = \mathbb{1} (T > s)$ avec $\mathbb{P}$$_{\lambda_0}$(T > s) = $\alpha$    où $\lambda_0 \in$ {$\lambda \leq$ 3}
#' 
#' On veut $\alpha$ $\leq$ 0.05 : 
#' 
#' $\mathbb{P}$$_{\lambda_0}$(T $\leq$ s) = 1 - $\alpha$ $\geq$ 0.95
#' 
#' Donc s = $F^{-1}(1 - \alpha)$ où $F^{-1}$ est la fonction quantile de la loi de Poisson. Cette fonction est bien définie car la fonction de répartition est bijective.
#' 
s = qpois(0.95,length(discoveries)*3)
print(s)
#' 
#' **3)**
#' D'après la question précédente, on obtient un test au niveau inférieur ou égal à 5%.
#'  
#' On a $\chi_0$ = {t $\in$ $\chi$, $\delta$(t) = $\mathbb{1}$(t > s) = 0} où s est déterminé à la question précedente.
#' 
#' $\chi_1$ = {t $\in$ $\chi$, $\delta$(t) = $\mathbb{1}$(t > s) = 1}
#' 
#' $\chi$ = $\chi_0$ U $\chi_1$
#' 
#' De plus, on a $\mathbb{P}$$_{\lambda_0}$(T $\in$ $\chi_1$) $\leq$ 0.05  
#' 
#' Ainsi, on peut accepter l'hypothèse nulle $H_0$.
#' 
#' **4)**
#' On veut déterminer le nombre minimum $n_0$ de données nécessaires pour rejeter $H_0$.
#' 
#' Ainsi, on considère $\mathbb{1}$(T>s) = 1
#' 
#' T > s
#' 
#' $\frac{1}{n}$ $\sum_{i=0}^{n} X_i$ > $\frac{s}{n}$
#' 
#' m > $\frac{s}{n}$
#' 
#' $n_0$m > s
#' 
m = sum(discoveries)/length(discoveries)
n_0 = 1
while (n_0*m <= qpois(0.95, n_0*3)){
  n_0 = n_0 + 1
}
print(n_0)
#' 
#' **5)**
x_lambda = seq(3, 4, length.out = 100)
beta = vector("numeric",100)
for (i in 1:100){
  beta[i] = 1 - ppois(s, 100*x_lambda[i])
}
plot(x_lambda, beta  , type = "l", main ="Fonction puissance en fonction de lambda")
#' On veut trouver le nombre de données minimum pour que $\beta$($\lambda$ = 3.5) $\geq$ 0.9
#' 
#' $\beta$($\lambda$ = 3.5) = 1 - $\mathbb{P}_{\lambda = 3.5}(T ≤ s)$
#' 
#' Donc $\mathbb{P}_{\lambda = 3.5}(T ≤ s)$ ≤ 0.1
#' 
#' Par conséquent, on cherche $n_1$ tel que la condition ci-dessous est vérifiée.
n_1 = 1
p_repartition = ppois(s,n_1*3.5)
while(p_repartition > 0.1){
  n_1 = n_1 + 1
  p_repartition = ppois(s, n_1 *3.5)
}
print(n_1)
#' 
#' 
#' 
#' 
#' **Exercice 3 **
#' 
#' **1)**
#' Dans le modèle de Poisson, on choisit le prior conjugué pour $\lambda$.
#' On choisit donc la loi Gamma($\alpha$,$\beta$) pour $\lambda$.
#' Elle a pour densité : $\frac{\beta^\alpha}{Γ(\alpha)} x^{\alpha -1} exp(-\beta x)$
#' 
#' On sait que pour une loi Gamma :
#' 
#' E(X) = $\frac{\alpha}{\beta}$
#' 
#' V(X) = $\frac{\alpha}{\beta^2}$
#' 
#' On veut E(X) = 5 et V(X) = 100 donc on obtient, en résolvant le système d'équation à 2 inconnues:
#' 
#' $\alpha$ = 0.25
#' 
#' $\beta$ = 0.05
#' 
#' **2)**
#' $\pi$($\lambda$|$x_{1:n}$) = $\frac{p_\lambda(x_{1:n})\pi(\lambda)}{p(x_{1:n})}$
#' 
#' On sait que les $X_i|\lambda$ sont iid et suivent une loi de Poisson de paramètre $\lambda$ donc on obtient:
#' 
#' $p_\lambda(x_{1:n})\pi(\lambda) = \frac{\beta^{\alpha}\lambda^{\alpha + \sum_{i=1}^{n} x_i -1} exp(-(\beta + n)\lambda)}{Γ(\alpha) \prod x_i!} \mathbb{1}(\lambda > 0)$
#' 
#' et 
#' $p(x_{1:n}) = \int_{0}^{+\infty} \frac{\beta^{\alpha}t^{\alpha + \sum_{i=1}^{n} x_i -1} exp(-(\beta + n) t)}{Γ(\alpha) \prod x_i!} dt$
#' 
#' En posant le changement de variable u = ($\beta + n)t$,
#' 
#' $p(x_{1:n}) = \frac{\beta^{\alpha}}{(\beta + n)^{\alpha + \sum_{i=1}^{n} x_i}} \int{0}^{+\infty} \frac{u^{\alpha + \sum_{i=1}^{n} x_i -1} exp(-u)}{Γ(\alpha) \prod x_i!} du$
#' 
#' Donc, on obtient:
#' 
#' $\pi$($\lambda$|$x_{1:n}$) = $\frac{(\beta + n)^{\alpha + \sum_{i=1}^{n} x_i} \lambda^{\alpha + \sum_{i=1}^{n} x_i -1} exp(-(\beta + n)\lambda)}{Γ(\alpha + \sum_{i=1}^{n} x_i)} \mathbb{1}(\lambda > 0)$
#' 
#' $\lambda$|$x_{1:n}$ suit donc une loi Gamma de paramètres $\alpha + \sum x_i$ et $\beta + n$.
#' 
#' L'expression de l'estimateur de l'espérance est donc $\hat\lambda(x_{1:n}) = \frac{\alpha + \sum_{i=1}^{n} x_i}{\beta + n}$ d'après l'expression de l'espérance de la loi Gamma.
#' 
#' Ainsi, pour l'estimateur du maximum de vraisemblance, on a : $\lambda_{vraisemblance} = \frac{1}{n}$ $\sum_{i=1}^{n} X_i$
#' 
#' Comme $\alpha$ et $\beta$ sont faibles ici, on peut dire que l'estimateur de l'espérance et l'estimateur de maximum de vraisemblance ont des valeurs très proches.
#' 
#' **3)**
#' Pour discoveries, on obtient
alpha = 0.25 + sum(discoveries)
print(alpha)
#'
beta = 0.05 + length(discoveries)
print(beta)
#' 
E_posteriori = alpha/beta
print(E_posteriori)
#' 
#' **4)**
#' On veut $\mathbb{P}(\lambda \in I | x_{1:n}) ≥ \alpha$
#' 
#' Or $\mathbb{P}(\lambda \in I | x_{1:n}) = \mathbb{P}(\lambda ≤ b | x_{1:n}) - \mathbb{P}(\lambda < a | x_{1:n})$    si I = [a,b]
#' 
#' On sait que $\lambda$ suit une loi Gamma. Comme la densité est positive, la fonction de répartition est croissante et à valeurs dans [0,1] donc elle est bijective.
#' 
#' Ainsi, on peut définir son inverse $F^{-1}$.
#' 
#' On définit $q_{\frac{1 - \alpha}{2}} = F^{-1}(\frac{1 - \alpha}{2})$  et  $q_{\frac{1 + \alpha}{2}} = F^{-1}(\frac{1 + \alpha}{2})$
#' 
#' Donc I = [$q_{\frac{1 - \alpha}{2}}, q_{\frac{1 + \alpha}{2}}$]
#' 
#' On note ici a = $q_{\frac{1 - \alpha}{2}}$ et b = $q_{\frac{1 + \alpha}{2}}$
a = qgamma(p=(1-0.95)/2,shape=alpha ,scale = 1/beta)
print(a)
#'
b = qgamma(p=(1+0.95)/2,shape=alpha ,scale = 1/beta)
print(b)









