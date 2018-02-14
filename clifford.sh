#!/usr/bin/python

import sys

R = "\u211D"
C = "\u2102"
H = "\u210D"


def otimes(a,b):
	if   a[0] == R:
		return [b[0],a[1]*b[1],a[2]*b[2]]
	elif a[0] == C:
		if   b[0] == R:
			return [C,a[1]*b[1],a[2]*b[2]]
		elif b[0] == C:
			return [C,a[1]*b[1],2*a[2]*b[2]]
		elif b[0] == H:
			return [C,2*a[1]*b[1],a[2]*b[2]]
		else:
			return [0,0,0]
	elif a[0] == H:
		if   b[0] == R:
			return [H,a[1]*b[1],a[2]*b[2]]
		elif b[0] == C:
			return [C,2*a[1]*b[1],a[2]*b[2]]
		elif b[0] == H:
			return [R,4*a[1]*b[1],a[2]*b[2]]
		else:
			return [0,0,0]
	else:
		return [0,0,0]


def clifford(s,t):
	if (s<0)or(t<0):
		return [0,0,0]
	elif (s==0)and(t==0):
		return [R,1,1]
	elif (s==0)and(t==1):
		return [R,1,2]
	elif (s==1)and(t==0):
		return [C,1,1]
	elif (s>0)and(t>0):
		return otimes([R,2,1],clifford(s-1,t-1))
	elif (s==0)and(t>1):
		return otimes([R,2,1],clifford(t-2,s))
	elif (s>1)and(t==0):
		return otimes([H,1,1],clifford(t,s-2))


cl = clifford(int(sys.argv[1]),int(sys.argv[2]))
result = k = cl[0]+"(%d)"%cl[1]
for i in range(cl[2]-1):
	result = result+"\u2295"+k
print(result)
