import numpy as np  
import time   
N = 6000  
M = 10000  
  
#k_list = [64, 80, 96, 104, 112, 120, 128, 144, 160, 176, 192, 200, 208, 224, 240, 256, 384]  
k_list = [64]
  
def get_gflops(M, N, K):  
    return M*N*(2.0*K-1.0) / 1000**3  
  
for K in k_list:  
    a = np.array(np.random.random((M, N)), dtype=np.double, order='C', copy=False)  
    b = np.array(np.random.random((N, K)), dtype=np.double, order='C', copy=False)  
    A = np.matrix(a, dtype=np.double, copy=False)  
    B = np.matrix(b, dtype=np.double, copy=False)  
  
    C = A*B  
  
    start = time.time()  
  
    C = A*B  
    C = A*B  
    C = A*B  
    C = A*B  
    C = A*B  
  
    end = time.time()  
  
    tm = (end-start) / 5.0  
  
    print ('{0:4}, {1:9.7}, {2:9.7}'.format(K, tm, get_gflops(M, N, K) / tm)) 
