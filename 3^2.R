bl1 = c(10,15,11,19,25,30,22,25,22)
bl2 = c(7,14,14,16,18,22,12,27,34)
bl3 = c(8,16,12,15,22,24,14,20,26)
bl4 = c(11,15,3,18,29,36,24,27,28)

data = t(data.frame(bl1,bl2,bl3,bl4))
bltotal <- apply(data,1,sum)


r = 4

tb = bl1+bl2+bl3+bl4
d = matrix(tb,3,3)
I = c((d[1,1]+d[2,2]+d[3,3]),(d[2,1]+d[3,2]+d[1,3]),(d[3,1]+d[1,2]+d[2,3]))
J = c((d[1,1]+d[3,2]+d[2,3]),(d[2,1]+d[1,2]+d[3,3]),(d[3,1]+d[2,2]+d[1,3]))
A = colSums(d)
B = rowSums(d)

ct = sum(tb)^2/(9*r)
sstot = sum(bl4^2+bl1^2+bl2^2+bl3^2)-ct
blss = (sum(bltotal^2)/9)-ct
ssA = (sum(A^2)/(3*r))-ct
ssB = (sum(B^2)/(3*r))-ct
ssAB = (sum(J^2)/(3*r))-ct
ssAB2 = (sum(I^2)/(3*r))-ct
Tretss = ssA+ssB+ssAB+ssAB2
Ess = sstot-Tretss-blss

rbind(Total_SS = sstot, Treatment_SS = Tretss, Block_SS = blss, SSm = ssA, SSv = ssB, SSmv = ssAB, SSmv2 = ssAB2, Ess = Ess)