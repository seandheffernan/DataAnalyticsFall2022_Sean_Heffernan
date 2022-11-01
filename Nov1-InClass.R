data("USArrests")

states= row.names(USArrests)
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pr.out = prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale

pr.out$rotation

biplot(pr.out, scale=0)
pr.out$sdev

pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve


data("iris")
irisdata1 <- iris[,1:4]
irisdata1

principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
plot(principal_components)
plot(principal_components, type="l")

biplot(principal_components)



