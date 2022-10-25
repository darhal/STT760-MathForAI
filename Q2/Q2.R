# import libs and data
library("bnlearn")
library("Rgraphviz")

data(marks)
dagNotes = empty.graph(names(marks))
arcs(dagNotes) = matrix(c("VECT", "MECH",
                        "ALG", "MECH",
                        "ALG", "VECT",
                        "ANL", "ALG",
                        "STAT", "ALG",
                        "STAT", "ANL"),
                        ncol = 2, byrow = TRUE, 
                        dimnames = list(c(), c("from", "to")))

vStructs = list(arcs = vstructs(dagNotes, arcs = TRUE))
graphviz.plot(dagNotes, highlight = vStructs, main = "Interdépendances des sujets")

# 1)
notesReussite = as.data.frame((marks >= 45) * 1)
notesReussite[notesReussite == 1] = "R"
notesReussite[notesReussite == 0] = "E"

#to set a column as factor
toFactor <- function(x)
{
  as.factor(x)
}

#set factors
notesReussite[] = lapply(notesReussite, toFactor) 

# 2)
f1 = function(x, p1)
{
  a = 0
  
  if((x == 0) || (x == 1))
  {
    a = p1^x * (1 - p1)^(1 - x)
  }
  
  return(a)
}

f2 = function(x, y, p2, p3)
{
  a = 0
  
  if(((x == 0) || (x == 1)) && ((y == 0) || (y == 1)))
  {
    a = f1(x, p2)^y * f1(x, p3)^(1 - y)
  }
  
  return(a)
}

f3 = function(x, y, z, p4, p5, p6, p7)
{
  a = 0
  
  if(((x == 0) || (x == 1)) && ((y == 0) || (y == 1)) && ((z == 0) || (z == 1)))
  {
    tmp1 = f1(x, p4)^(y*Z) * f1(x, p5)^((1 - y)*z)
    tmp2 = f1(x, p6)^((1 - z)*y) * f1(x, p7)^((1 - y)*(1 - z))
    a = tmp1 * tmp2
  }
  
  return(a)
}

f4 = function(x, y, p8, p9)
{
  return(f2(x, y, p8, p9))
}

f5 = function(x, y, z, p10, p11, p12, p13)
{
  return(f3(x, y, z, p10, p11, p12, p13))
}

# Xi = (x1, x2, ..., x5)
# P = (p1, p2, ..., p13)
L = function(Xi, P)
{
  F1 = f1(Xi[5], P[1])
  F2 = f2(Xi[4], Xi[5], P[2], P[3])
  F3 = f3(Xi[3], Xi[4], Xi[5], P[4], P[5], P[6], P[7])
  F4 = f4(Xi[2], Xi[3], P[8], P[9])
  F5 = f5(Xi[1], Xi[2], Xi[3], P[10], P[11], P[12], P[13])
  
  return(F1 * F2 * F3 * F4 * F5)
}

# 3)







# 4)
bn.fit(dagNotes, data = notesReussite)




