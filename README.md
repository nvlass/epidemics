# epidemics

Simulate epidemic spreading in a graph.

## Description

This is a simple simulator for the time evolution of an epidemic on a
[Watts–Strogatz](https://en.wikipedia.org/wiki/Watts%E2%80%93Strogatz_model)
graph.

It has been hacked together to give a visual representation of the
dynamics of an epidemic and a playground for understanding how
dangeous these dynamics can be.

[See epidemics in action](https://nvlass.com/epidemics)

## Model overview

The graph represents a population of *N* (>= 100) individuals, such
that every day, each individual may transmit a disease to its
neighbors. There are several parameters controlling the graph model
and the simulation:

* *N* is the number of individuals
* *Degree* is the average number of neighbors
* *beta* controlls the shape of the graph. Experiment with different
  values and check Wikipedia for more information on its effects
* *Disease Transmission Probability* (`p_t`) The probability that the
  disease is transmitted to a neighbor each day that an individual is
  infectious
* *Days an individual is infectious* The number of days an individual
  is infectious
  
Node colors in the graph represent the state of each individual:
* Green: subsceptible / not yet infected
* Red: Infected
* Black: Removed (immune or deceased)

### Probability and Days

The keen-eyed will probably notice that the `days` parameter could be
"absorbed" into `p_t`. Doing so, however, will probably not make
explicit the fact that "the more days you are infectious, the more
dangerous it is".


Built with [Clojurescript](https://clojurescript.org/),
[Cytoscape](https://js.cytoscape.org/) and [Milligram
CSS](https://milligram.io/)

## Bugs

Probably several. Hopefully not that many that the model is broken.

## License

Copyright © 2020 Nikolaos Vlassopoulos

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
