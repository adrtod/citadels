citadels
=============
Play random games of [Citadels](http://en.wikipedia.org/wiki/Citadels_(card_game))

[![Build Status](https://travis-ci.org/adrtod/citadels.png?branch=master)](https://travis-ci.org/adrtod/citadels)

Getting started
=======
Install the latest version of the R package from github
```
require(devtools)
devtools::install_github("adrtod/citadels")
```

Play a random game with 3 players
```
library(citadels)

game(list_players(3), quiet = FALSE)
```

Author
==================
Copyright (C) 2015 Adrien Todeschini <adrien.todeschini@gmail.com>

License: GPL-2

Roadmap
========
- [ ] object oriented structure
- [ ] allow human players
- [ ] improve robustness
- [ ] graphical output
- [ ] intelligent bots
- [ ] translations

Release notes
==================
Version 0.1 (01-2015)
---------------------
Initial version. Can play random games with random players.

Todo
==================
- [ ] possibility to build zero quarters
- [ ] possibility to destroy zero quarters
- [ ] a player can NOT have 2 quarters with the same name in his city
- [ ] the king, the bishop, the merchant and the condottiere can collect coins
  at ANY moment in their turn
- [ ] implement special purple cards
- [ ] implement new roles
