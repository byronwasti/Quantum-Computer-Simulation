# Quantum Computer Simulation

*~~Note: In order to use the code in this repo, you will need to have Haskell-Stack installed. You can install it using these instructions [https://docs.haskellstack.org/en/stable/install_and_upgrade/](https://docs.haskellstack.org/en/stable/install_and_upgrade/).~~ You need Python3 installed.*

*GAMEPLAN: Simple simulator for Quantum Computer (X, Y, Z gates and Hadamard) as well as explanation of how to link things Then have a translation for how online quantum computer simulators function*

This repository holds files necessary to run simulation of quantum computation using 5 fundamental quantum logic gates (X, Y, Z, Hadamard and CNOT). There are *almost* two implementations of basic quantum computation, one in Python and one in Haskell. Currently the Haskell version is having difficulties and is not functional.

One of the main reasons for implementing quantum computation simulation in Python/Haskell is to more deeply understand how a quantum computer operates. By implementing a simulation for quantum computation, one can better understand where a quantum computer can be better than a classical computer. 

## How a Quantum Computer Works

A quantum computer is similar to a classical computer in many ways. For instance, in classical computing you have the bit (a `1` or a `0`) which is used to represent all data. Quantum computers use a similar concept, known as *quantum bit*, or *qubit* for short. Classical computers are built using a series of logic gates, and that is still true for quantum computers, although the types of logic gates are slightly different.

Lastly, both classical computers and quantum computers solve computational problems. A quantum computer can simulate a classical computer, and vis versa (which we do here). There are many applications for quantum computing, including solving problems that classical computer would take many thousands of years to do. However, there are many use-cases that a classical computer is more suited for than a quantum computer such as databases, where saving and reading memory is a common operation. 


----

## Implementation Details

The following sections go through the different aspects of basic quantum computation, as well as how they are represented in the simulation code in this repository.


### The Quantum bit

Instead of being a `1` or a `0`, a qubit is the linear combination of the `|0>` and `|1>` basis vectors (`|0>` is Ket notation for vectors, which is the usual notation when talking about quantum computers). We will not concern ourselves with the physical implementation of a qubit (there are many different methods).

For instance, a typical qubit can be represented as `a|0> + b|1>` where `|a^2| + |b^2| = 1`. There is also the notion of global phase, which adds in another layer of complexity to the qubit, but for this short intro to basic quantum computation we will not go there. However, since the Y, Z and Hadamard gates change the global phase, it is worth mentioning.

#### Qubit in the Code

Representing the qubit in a quantum computation simulator is one of the trickiest aspects of designing a simulator. There are two ways that I considered modeling the qubit. The first was to represent each qubit as a vector of length 2, with values as the coefficients of the basis vectors `|0>` and `|1>`. This means it is trivially easy to operate on a single qubit, because you can just apply a matrix multiplication on it.

For example, in Python, it can be represented as a 1D matrix with numpy:
```python
import numpy as np
class Qubit:
    __init__(self):
        qubit = np.matrix('1 0')
```

In Haskell, it can be represented as a vector using HMatrix:
```haskell
import Numerical.LinearAlgebra
type Qubit = Vector (Complex Double)

createQubit :: Qubit
createQubit = fromList [ 1.0 :+ 0, 0 :+ 0]
```

This method of representation allows for very easy single-qubit operations. Since every quantum gate can be represented as a unitary matrix, you basically have the quantum gate operations as mere matrix multiplications. Easy.

The downside is that two-qubit operations are now impossible, or at least non-trivial to implement. Since the CNOT gate causes for qubits to become entangled, you must represent this entanglement in some way. For example, in order to create the Bell State (a two-qubit system of `|00> + |11>` all over `sqrt(2)` to keep magnitudes correct) one needs to allow the qubits to become entangled. This turns out to be non-trivial if you implement the qubits as discrete objects because there is no good way to show entanglement.

Thus, as many online quantum simulators do it, the other method is to simply keep track of all states the qubits can be in. For a system of `n` qubits, this means keeping track of `2^n` qubits. This is the way the qubits are represented in the simulators in this repository.

For example, in Python, they are represented as an array:

```python
class Register:
    def __init__(self, size):
        self.size = size
        self.qubits = [ 0+0j for x in range(2**size) ] 
        self.qubits[0] = 1+0j
```

*Note that they are now called a "Register."*

In Haskell, I couldn't quite figure out a clean way of doing this, but I gave an attempt.
```haskell
data Register = Register { nStates :: Int, qubits :: [Qubit] }

newRegister :: Int -> Register
newRegister n = Register { nStates = n, qubits = [blankQubit (x-1) | x <- [1..2^n] ] }
                where blankQubit 0 = Qubit { probability = 1.0, qubit = 0 }
                      blankQubit y = Qubit { probability = 0.0, qubit = y }
```

This new way of representing the qubit states comes with its own drawbacks. It is now trivial to represent the entanglement of two qubits, but it becomes more difficult to apply single qubit operations. One can no longer just do simple matrix multiplication and know that everything will work.


----

### Quantum Logic Gate

TODO: How is a quantum gate represented in the code?

A quantum logic gate is similar to a classical logic gate. It is the method for changing the states of qubits. There are a few subtle differences that end up having some dramatic consequences.

One of the first differences is that a quantum gate is always reversible. Thus, a 2-input quantum gate has 2-outputs. Since every quantum gate can be represented as a matrix, every quantum gate is a special type of matrix called a unitary matrix.

The second difference is that a quantum gate always preserves the magnitude of the qubit(s) it operates on. Technically this is also true for a classical gate, as they can only switch a bit from `0` to `1`, which if we consider them orthogonal vectors would always have a magnitude of 1. However, a quantum gate can change a qubit into a superposition of the `|0>` and `|1>` states. One of the main rules (in order to follow the laws of quantum physics) is that any superposition of states must always have a magnitude of 1. Thus, `a|0> + b|1>` would have `a^2 + b^2 = 1`.


There are quite a few quantum gates. In theory, there are actually an infinite amount of them. For the purposes of our simulators, we will only discuss 5 of them.

The first four gates are single-qubit gates, which means they operate on only 1 qubit.

The X Gate (or Sauli-X gate, or Pauli-X gate) is the basic "not" gate for a qubit. It is represented by the matrix `[ 0 1 ; 1 0 ]` and will swap `|0>` for a `|1>`, and `|1>` for a `|0>`.

The Y Gate (Sauli-Y/Pauli-Y gate) is a slight variation on the X gate. It is represented by the matrix `[ 0 -i ; i 0 ]`. This gate is essentially the X gate with a change of phase.

The Z Gate (Sauli-Z/Pauli-Z gate) is simply a phase-shift gate and is represented by the matrix `[ 1 0 ; 0 -1 ]`. It takes the `|1>` state and phase-shifts it by pi radians. Since we did not go into what the phase has to do with quantum computation, we do not need to go into how this gate is useful.

The Hadamard gate is a fun gate. It is represented by the matrix `1/sqrt(2) * [ 1 1 ; 1 -1 ]`. What this gate does is transform `|0>` into `(|0> + |1>) / sqrt(2)` and `|1>` into `(|0> - |1>) / sqrt(2)`. This gate basically takes a qubit that is not doing quantum-fun and makes it into the superposition of two states.

The last gate we will discuss and implement is the CNOT gate, or controlled-Not gate. This is the only 2-qubit gate, which takes in two qubits and outputs two qubits. It is quite easily described, as it applies the X-Gate on the second qubit IF the first qubit is a `|1>`. 

----

*Phew* that was a lot. 


#### Quantum Logic Gates in the Code

### Linking It All Together

TODO: How to do a quantum computation?


## FAQ

- Why do this in ~~Haskell~~ Python?
- What are the use cases of this repository?
- Where to go from here?


# Resources

- Quantum Computing: A Gentle Introduction by By Eleanor G. Rieffel and Wolfgang H. Polak
- Haskell Simulator of Quantum Computer, Jan Skibinski [http://web.archive.org/web/20010803034527/http://www.numeric-quest.com/haskell/QuantumComputer.html](http://web.archive.org/web/20010803034527/http://www.numeric-quest.com/haskell/QuantumComputer.html)
- Michael Nielson's Videos on Quantum Computation [youtube](https://www.youtube.com/watch?v=X2q1PuI2RFI&list=PL1826E60FD05B44E4)

