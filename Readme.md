# Distributed Prime Number Calculator

A distributed prime number calculator implemented in Erlang, allowing multiple nodes to communicate and calculate prime numbers in a distributed system.

## Overview

This project demonstrates the implementation of a distributed system in Erlang. It consists of multiple nodes that can communicate with each other to calculate prime numbers. The system supports connecting nodes, maintaining routing tables, and calculating the Nth prime number.

## Modules

The project consists of four main modules:

node.erl:
The main module that implements the functionality of individual nodes in the system.

prime.erl:
A module that provides functions for calculating prime numbers.

routing.erl:
A module responsible for managing routing tables and updating routes between nodes.

demo.erl:
A demonstration script that sets up a sample network of nodes and shows the functionality of the project.

## Usage

To show off the functionality of the project, you can run the demo.erl script.

### Steps

1. Start the erlang shell:

```
$ erl
```

2. Compile the modules:

```
c(node).
c(prime).
c(routing).
c(demo).
```

3. Run the demo:run/0 function:

```
demo:run().
```

The output will show the creation of nodes, connections between them, routing tables, and the calculation of prime numbers.

If you prefer not to use the demo.erl module to demonstrate the functionality of the Distributed Prime Number Calculator project, you can follow the steps below to set up and run the project manually:

1. Start the Erlang shell.

```
$ erl
```

2. Compile all the Erlang modules: node, routing, prime, and demo (if not already compiled).

```
c(node).
c(prime).
c(routing).
c(demo).
```

3. Create and launch nodes using the node:launchNode/1 function.

```
Node1 = node:launchNode(node1).
Node2 = node:launchNode(node2).
...
NodeN = node:launchNode(nodeN).
```

4. Connect nodes together using the node:connectNode/4 function. Replace NodeA, NodeB, PidA, and PidB with the appropriate node names and process IDs.

```
node:connectNode(NodeAName, Node1, NodeBName, Node2).

```

5. Print the routing tables for each node using the node:printTable/1 function. Replace Pid with the appropriate process ID.

```
node:printTable(Pid).
```

6.Request the computation of the Nth prime number from one node to another using the node:retrieveNthPrime/3 function. Replace N, NodeA, and NodeB with the appropriate values.

```
node:retrieveNthPrime(N, NodeAName, NodeBName).
```

6. Observe the output in the Erlang shell to see the results and messages exchanged between nodes.

7. Exit the Erlang shell.

```
q().
```

## License

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

## Author

Ciaran Maye
# Distributed-Prime-Number-Calculator
