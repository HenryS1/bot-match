# Plan

## Design principles

* Fairness to bots
* Simplicity of interface
* Low resource utilisation while bots are busy
* Low communication overhead

## Runtime system

* Launch bot1, launch bot2
* Suspend processes
* Wake up bot1 send command
* Start timer after flushing stdin
* Can send back any time within timer window and get 50 milliseconds to send after starting sending
* Suspend bot1
* Repeat process for bot2

### Questions

* How do we sandbox bots
  * Potential answer: put it in a fire jail
* How do we ensure that GC doesn't cause timing blips?
  * Manage memory carefully during bot turns and suggest free after. Profile to see whether it works.

## Game

* Basic game to start with (for testing runtime system):
  * Take turns guessing the opponents number
* Game we would like at some point
  * Footsoldier wars
    * Create different types of soldiers
    * They run across and attack enemy soldier or enemy base
    * Soldier behaviour is deterministic
    * Win by destroying the opponents base
    * Bigger units take more time
      (or units cost an amount and fixed resources allocated per turn)
    * Spawn in various locations and that determines the path to the enemy base
    * Bases are one block in the middle vertically of the players back column

### Principles

* Use discrete maths and simple maths
* Avoid geometric edge cases

### Questions

* How do units attack each other?
* What units are there?
* How do units attack the enemy base?
* How many blocks do units move per turn?
* How do we prevent stalemates and generate interesting play?
