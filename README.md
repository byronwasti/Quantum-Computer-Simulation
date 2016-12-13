# Quantum Computer Simulation

*~~Note: In order to use the code in this repo, you will need to have Haskell-Stack installed. You can install it using these instructions [https://docs.haskellstack.org/en/stable/install_and_upgrade/](https://docs.haskellstack.org/en/stable/install_and_upgrade/).~~ You need Python3 installed.*

*GAMEPLAN: Simple simulator for Quantum Computer (X, Y, Z gates and Hadamard) as well as explanation of how to link things Then have a translation for how online quantum computer simulators function*

This repository holds files necessary to run simulation of quantum computation using 5 fundamental quantum logic gates (X, Y, Z, Hadamard and CNOT). There are *almost* two implementations of basic quantum computation, one in Python and one in Haskell. Currently the Haskell version is having difficulties and is not functional.

One of the main reasons for implementing quantum computation simulation in Python/Haskell is to more deeply understand how a quantum computer operates. By implementing a simulation for quantum computation, one can better understand where a quantum computer can be better than a classical computer. 

## How a Quantum Computer Works

A quantum computer is similar to a classical computer in many ways. For instance, in classical computing you have the bit (a `1` or a `0`) which is used to represent all data. Quantum computers use a similar concept, known as *quantum bit*, or *qubit* for short. Classical computers are built using a series of logic gates, and that is still true for quantum computers, although the types of logic gates are slightly different.

Lastly, both classical computers and quantum computers solve computational problems. A quantum computer can simulate a classical computer, and vis versa (which we do here). There are many applications for quantum computing, including solving problems that classical computer would take many thousands of years to do. However, there are many use-cases that a classical computer is more suited for than a quantum computer such as databases, where saving and reading memory is a common operation. 

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


### Quantum Logic Gate

TODO: How is a quantum gate represented in the code?

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

