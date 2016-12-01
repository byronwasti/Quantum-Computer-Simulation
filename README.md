# Quantum Computer Simulation

*Note: In order to use the code in this repo, you will need to have Haskell-Stack installed. You can install it using these instructions [https://docs.haskellstack.org/en/stable/install_and_upgrade/](https://docs.haskellstack.org/en/stable/install_and_upgrade/).*

TODO: What did I do? What is this repository?

TODO: Why try to simulate a quantum computer?

## Implementation Details

TODO: How is this implemented?

## How a Quantum Computer Works

A quantum computer is similar to a classical computer in many ways. For instance, in classical computing you have the bit (a `1` or a `0`) which is used to represent all data. Quantum computers use a similar concept, known as the *quantum bit*, or *qubit* for short. Classical computers are built using a series of logic gates, and that is still true for quantum computers, although the types of logic gates are slightly different.

Lastly, both classical computers and quantum computers solve computational problems. A quantum computer can simulate a classical computer, and vis versa (which we do here). There are many applications for quantum computing, including solving problems that classical computer would take many thousands of years to do. However, there are many use-cases that a classical computer is more suited for than a quantum computer. 

### The Quantum bit
Instead of being a `1` or a `0`, it is the linear combination of the `|0>` and `|1>` vectors (`|0>` is Ket notation for vectors, which is the usual notation when talking about quantum computers).

For instance, a typical qubit can be represented as `a|0> + b|1>` where `|a^2| + |b^2| = 1`. There is also the notion of global phase, which adds in another layer of complexity to the qubit, but for this short into to quantum computers we will not go into that.

TODO: How is the quantum bit represented in the code?


### Quantum Logic Gate

TODO: How is a quantum gate represented in the code?

### Quantum Computing

TODO: How to do a quantum computation?

## FAQ

- Why do this in Haskell?
- What are the use cases of this repository?
- Where to go from here?
