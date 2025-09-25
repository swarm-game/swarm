# Synchronized capture

Two approaches to implement the goal-checking of this scenario were considered.
In both, the goal is reached when the "pig" is surrounded on all four sides by barriers.

The difference is is when the barriers are not synchronized:
1. In the first approach, a goal condition is defined that detects if any barrier is adjacent to the pig without being surrounded on all sides. The primary goal condition treats this as a negated prerequisite, so that the scenario is instantly lost.
2. In the second (adopted) approach, this secondary goal was preserved, but only as an optional achievement. Instead, the pig as a system robot implements the detection of an un-synchronized approach. The pig retreats a short distance, providing another opportunity for the player to attempt a synchronized approach (though players may simply choose to restart the scenario).

In implementing this second approach, it is essential that the pig react to barrier placement within a single tick. We do not want to allow the player to place one barrier later than the others and simply be too slow to detect this un-synchronized state, counting it as a win. This single-tick reaction is accomplished with the `instant` command.

## Retreat behavior
If a barrier comes in contact with the pig without surrounding on all sides, the pig "runs away" in a straight line opposite from the "base" robot, irrespective of any entities or impassible terrain, until no barriers are adjacent. This may entail traversing multiple cells, which happens instantaneously, due to the single-tick reaction requirement described above.

This escape strategy is simplified by finite availability of `unwalkable`, `portable` entities in this scenario.

## Solution

There at least two ways to solve this challenge, both of which require building child robots.
1. Use the `watch` command to synchronize two pairs of robots. Use symmetry and placed them equidistantly with respect to the watched location and their monolith.
2. Launch each child robot from the same location. Write custom code for each built robot to account for the delay incurred by building each. Adjust the timing by trial and error.

## Alternative game mechanic applications

Other physical phenomena could rationalize this "squeezing" game mechanic.

* The synchronized "squeeze" from all sides is evocative of the ["implosion method" of a fission weapon](https://en.wikipedia.org/wiki/Nuclear_weapon#/media/File:Fission_bomb_assembly_methods.svg). Synchronization is required to avoid a [fizzle](https://en.wikipedia.org/wiki/Fizzle_(nuclear_explosion)).  A [PNE](https://en.wikipedia.org/wiki/Peaceful_nuclear_explosion) has applications in:
    * [nuclear synthesis](https://en.wikipedia.org/wiki/Synthesis_of_precious_metals)
    * [space travel](https://en.wikipedia.org/wiki/Nuclear_pulse_propulsion)
    * [asteroid redirection](https://en.wikipedia.org/wiki/Asteroid_impact_avoidance)
* Instead of an explosion, the barriers could serve as [neutron reflectors](https://en.wikipedia.org/wiki/Neutron_reflector) for a fission reaction. See [demon core](https://en.wikipedia.org/wiki/Demon_core) for consequences of uncontrolled (un-synchronized) reflectors
* A high-pressure squeeze could create [artificial diamonds](https://en.wikipedia.org/wiki/Synthetic_diamond) (HPHT method).

