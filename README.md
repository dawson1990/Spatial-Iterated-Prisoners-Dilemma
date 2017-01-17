# Spatial-Iterated-Prisoners-Dilemma

The Game:
The prisoner's dilemma is a standard example of a game analyzed in game theory that shows why
two completely "rational" individuals might not cooperate, even if it appears that it is in their best
interests to do so. It was originally framed by Merrill Flood and Melvin Dresher working at RAND
in 1950. Albert W. Tucker formalized the game with prison sentence rewards and named it,
"prisoner's dilemma" (Poundstone, 1992), presenting it as follows:
Two members of a criminal gang are arrested and imprisoned. Each prisoner is in solitary
confinement with no means of communicating with the other. The prosecutors lack sufficient
evidence to convict the pair on the principal charge. They hope to get both sentenced to a year
in prison on a lesser charge. Simultaneously, the prosecutors offer each prisoner a bargain.
Each prisoner is given the opportunity either to: betray the other by testifying that the other
committed the crime, or to cooperate with the other by remaining silent. The offer is:

* If A and B each betray the other, each of them serves 2 years in prison
* If A betrays B but B remains silent, A will be set free and B will serve 3 years in prison (and vice versa)
* If A and B both remain silent, both of them will only serve 1 year in prison (on the lesser charge)

It is implied that the prisoners will have no opportunity to reward or punish their partner other than
the prison sentences they get, and that their decision will not affect their reputation in the future.
Because betraying a partner offers a greater reward than cooperating with him, all purely rational
self-interested prisoners would betray the other, and so the only possible outcome for two purely
rational prisoners is for them to betray each other.[1] The interesting part of this result is that
pursuing individual reward logically leads both of the prisoners to betray, when they would get a
better reward if they both kept silent. In reality, humans display a systemic bias towards cooperative
behavior in this and similar games, much more so than predicted by simple models of "rational"
self-interested action. [2] [3] [4] [5] A model based on a different kind of rationality, where people
forecast how the game would be played if they formed coalitions and then they maximize their
forecasts, has been shown to make better predictions of the rate of cooperation in this and similar
games given only the payoffs of the game.[6]
An extended "iterated" version of the game also exists, where the classic game is played repeatedly
between the same prisoners, and consequently, both prisoners continuously have an opportunity to
penalize the other for previous decisions. If the number of times the game will be played is known
to the players, then (by backward induction) two classically rational players will betray each other
repeatedly, for the same reasons as the single-shot variant. In an infinite or unknown length game
there is no fixed optimum strategy, and Prisoner's Dilemma tournaments have been held to compete
and test algorithms.[7]
The prisoner's dilemma game can be used as a model for many real world situations involving
cooperative behaviour. In casual usage, the label "prisoner's dilemma" may be applied to situations
not strictly matching the formal criteria of the classic or iterative games: for instance, those in
which two entities could gain important benefits from cooperating or suffer from the failure to do
so, but find it merely difficult or expensive, not necessarily impossible, to coordinate their activities
to achieve cooperation.


To create warden:

```erlang
W=warden:start({[],[]}).
```
To create prisoners:
```erlang
A=prisoner:create("prisonerOne",random,[]).
B=prisoner:create("prisonerTwo",titForTatRandom,[]).
C=prisoner:create("prisonerThree",titForTat,[]).
D=prisoner:create("prisonerFour",suspiciousTitForTat,[]).
E=prisoner:create("prisonerFive",alwaysCoop,[]).
F=prisoner:create("prisonerOne",alwaysDefect,[]).
```

warden APIs:
```erlang
warden:add(W,A).
.
.
.
warden:add(W,F).

warden:run(W,100).
warden:stats(W).
```

Prisoner APIs:
```erlang
prisoner:getName(A). 
prisoner:getStartegy(A).
prisoner:getState(A).
```

To run tests on warden:
```erlang 
warden:test().
```
For speed test it had to be put into a private function as EUnit crashed when iterations above 800 were entered.  To run a speed test:
```erlang 
warden:timerun(N) %% where N = number of iterations
```

To run tests on prisoner:
``` erlang
prisoner:test().
```
