# `elo-zs`

This project includes an Elo rating engine and a race result simulator, which
support analysis tools for analysis of races and racer performances in the
[ZakStunts Stunts competition](http://zak.stunts.hu). While there is a fair
amount of ZakStunts-specific code in the package, the cores of the Elo engine
and simulator are largely competition-agnostic, and could conceivably be
factored out into separate libraries.

The code here is still very much a work in progress. For the moment, the
most effective way to use it is interactively by loading the `Zak` module in
GHCi. For ling simulation runs, though, compiling with optimisations is highly
recommended; the `Main` module illustrates how to put the simulator to work.
`cabal build`, `cabal repl` and `cabal run` should work with no complications
for Cabal 3+. A Cabal freeze file (assuming GHC 8.8.3 for now) is provided to
cover for the package organisation details that still have to be ironed out.

Possible entry points for browsing the code:

- `Engine`: The Elo engine.

- `Analysis.Simulation`: The simulation engine.

- `Analysis.PerfModel.Orbital`: A simple gamma distribution-based performance
    model the simulation engine depends upon.

- `Analysis`: The key parts of the result analysis pipelines. (In contrast,
    the `Zak` module is mostly about presentation and providing a
    REPL-friendly interface.)

A number of decisions in the implementation of the Elo engine were informed by
Glickman, Mark E., [*A Comprehensive Guide to Chess
Ratings*](http://www.glicko.net/research/acjpaper.pdf) (1995), a very readable
introduction to the problem space. It should be noted that using Elo ratings
to rank racers is a very simple approach with clear limitations, chiefly the
absence of explicit modeling of rating uncertainties and the fact that the Elo
system is designed for one-versus-one matches rather than free-for-all races
(for prior art on such matters, see for instance Glickman's other works on the
topic, including the Glicko system, as well as Microsoft's TrueSkill). In this
code base, such limitations are, to an extent, addressed by various local
measures, some of them principled, and some of a more ad hoc nature.

Thanks to the worldwide Stunts community, whose élan runs through this
codebase.
