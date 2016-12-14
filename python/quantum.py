import numpy as np


class Register:
    def __init__(self, size):
        self.size = size
        self.qubits = [ 0+0j for x in range(2**size) ] 
        self.qubits[0] = 1+0j

    def __str__(self):
        return str( [ str(x)+" |"+str(i)+">" for i, x in enumerate(self.qubits) if x != 0] )

    def bit_is_set(self, num, bit):
        if (num & (1<<bit)) > 0:
            return True
        else:
            return False

    def updateNicely(self, reg, value, idx):
        if reg[idx] == 0:
            reg[idx] = value
        else:
            reg[idx] = reg[idx] + value

        return reg

    def X(self, bit):
        newReg = [0 for x in range(2**self.size)]
        for i, val in enumerate(self.qubits):
            newIdx = i ^ (1 << bit)
            newReg[newIdx] = val

        self.qubits = newReg

    def Y(self, bit):
        newReg = [0 for x in range(2**self.size)]
        for i, val in enumerate(self.qubits):
            newIdx = i ^ (1 << bit)
            if self.bit_is_set(i, bit):
                newReg[newIdx] = val*(0-1j)
            else:
                newReg[newIdx] = val*(0+1j)

        self.qubits = newReg

    def Z(self, bit):
        newReg = [0 for x in range(2**self.size)]
        for i, val in enumerate(self.qubits):
            if self.bit_is_set(i, bit):
                newReg[i] = val*(-1)
            else:
                newReg[i] = val*(1)

        self.qubits = newReg

    def hadamard(self, bit):
        newReg = [0 for x in range(2**self.size)]
        sqrt2 = 1/(2**.5)
        for i, val  in enumerate(self.qubits):
            newIdx = i ^ (1 << bit)
            if self.bit_is_set(i, bit):
                self.updateNicely(newReg, val*sqrt2, newIdx)
                self.updateNicely(newReg, -val*sqrt2, i)
            else:
                self.updateNicely(newReg, val*sqrt2, newIdx)
                self.updateNicely(newReg, val*sqrt2, i)

        self.qubits = newReg

    #def cnot(self, setBit, changeBit):
        #newReg = [x for x in self.qubits]
        #for i, val in enumerate(self.qubits):
            #if self.bit_is_set(i, setBit)

if __name__ == "__main__":
    print("Hello!")
    base = Register(6)
    print(base, "orig")
    #base.sauliX(0)
    print(base, "X")
    #base.sauliY(1)
    print(base, "Y")
    #base.sauliZ(1)
    print(base, "Z")
    base.hadamard(0)
    print(base, "Hadamard")
