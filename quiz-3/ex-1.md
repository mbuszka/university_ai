# ant colony algorithms
https://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms
Used to find some path in graph, by simulating ants.
The intuition is, that ants initially wander randomly, leaving peromone trail.
Then when they reach goal they return. If ant steps onto existing trail it has higher probability
of following it. The pheromones evaporate with time, so only the better the path, the higher is
probability of it persisting.

# edge selection
attractivness - based on some heuristic
trail level - based on pheromone level

# trail update
lowered by some multiplier and increased by each ant travelled

# travelling salesman
1. It must visit each city exactly once;
2. A distant city has less chance of being chosen (the visibility);
3. The more intense the pheromone trail laid out on an edge between two cities, the greater the probability that that edge will be chosen;
4. Having completed its journey, the ant deposits more pheromones on all edges it traversed, if the journey is short;
5. After each iteration, trails of pheromones evaporate.
