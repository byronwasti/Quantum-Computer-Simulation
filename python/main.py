import quantum as Q

base = Q.Register(2)
base.hadamard(1)
base.cnot(1, 0)
print(base)
