---
title: The Folyami Rating System
Author: Daniel Mlot
published: 2023-01-14T00:00:00-03:00
license: CC-BY-SA
---

This document presents the design choices that shape the Folyami rating
system. Though based on the well-known [Elo rating system](
https://en.wikipedia.org/wiki/Elo_rating_system), Folyami incorporates
several modifications which aim at making it suitable for racing
competitions and, in particular, [ZakStunts](https://zak.stunts.hu).

An implementation of Folyami can be found at
<https://github.com/duplode/elo-zs>. While it currently lacks a polished
interface, it is otherwise fully functional. Note that the description
here correspons to the default configuration of the elo-zs program. The
implementation allows the various extra features of the Folyami system
to be tuned, or even turned off to obtain a regular Elo system.

## Elo system recap

To understand why Folyami is an Elo-like rating system, and how it
differs from regular Elo, a brief review of how Elo ratings work is in
order. 

In the Elo system, ratings are updated according to the results of
matches, which always involve two players. The difference between the
ratings of the players in a match reflects how likely a victory by the
highest rated player is regarded to be. After a match, the players
exchange rating points, with the loser transferring points to the
winner. The formula for the change in points incorporates the difference
of ratings before the match, so that the more surprising the result, the
larger the change. The key formula which expresses this update procedure
is:

$$R'_{A} = R_{A} + K(S_{A} - E_{A})$$

Where:

- $R'_{A}$ is the updated rating for player A (we will call their
  opponent B).
- $R_{A}$ is the player A rating before the match.
- $K$, the *modulation* factor, sets how fast ratings vary with incoming
  results. It amounts to the maximum possible rating change in a match.
- $S_{A}$ is the player A score in the match: 1 for a win, 0 for a loss,
  and 0.5 for a draw.
- $E_{A}$ is the *expected* score for player A before the match, which,
  ignoring draws, corresponds to their probability of victory, and is
  calculated with a second formula:

$$E_{A} = {{1}\over{1 + e^{-\alpha D}}}$$

Where:

- $D = R_{A} - R_{B}$ is the difference between the ratings of the
  players before the match.
- $\alpha$ is a constant typically set to ${ln 10}\over{400}$, so that
  a 400-point rating difference in favour of player A translates to
  a ${{10}\over{11}} \approx 91\%$ winning probability.

For instance, consider a Elo system as described above with $K = 24$ (a
common value for chess tournaments). A rating difference of 191 points
in favour of player A corresponds to a winning probability of about 75%.
If A confirms their favouritism and win the match, they will get (and
B will lose) only 6 points (25% of $K$). However, if B pulls an upset,
they will get (and A will lose) 18 points (75% of $K$).

Folyami can be described as an Elo-like system because its rating
updates are done according to the first formula above. It differs from
Elo in having a $K$ modulation factor which varies not just with player
status (something that is not uncommon in chess Elo systems) but also
with race results, and in using a different formula for the expected
score/winning probability.
