import numpy
import numpy as np
t = np.zeros(1000)
for i in range(0,1000):
    t[i]=i

d_t_0 = (-1,False)
d_t = d_t_0
q_t = 0
R_i = 1
d_t_next = d_t_0
B = 128
q_max = 1024
temp = 0
for j in range (0,500):
    L = j
    d_t_0 = (-1,False)
    d_t = d_t_0
    q_t = 0
    d_t_next = d_t_0
    if temp==1:
        break
    for i in range(4000):
        d_t = d_t_next
        if(d_t[0]>=0 and d_t[1]==False):
            R_o = 2
        else:
            R_o = 0

        q_t = q_t + (R_i-R_o)

        if (d_t[0]==-1 and d_t[1]==False and q_t>=B):
            d_t_next = (L,True)
        elif (d_t[0]!=0 and d_t[1]==True):
            d_t_next = (d_t[0]-1,True)
        elif (d_t[0]==0 and d_t[1]==True):
            d_t_next = (B,False)
        elif (d_t[0]>=0 and d_t[1]==False):
            d_t_next = (d_t[0]-1,False)
        
        #print(d_t)

        if(q_t<0):
            print("\nUnderflow")
        elif(q_t>1024):
            print("Overflow")
            print(i)
            print(j)
            temp = 1
            break
    
    