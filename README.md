citadels
=============
Play random games of [Citadels](http://en.wikipedia.org/wiki/Citadels_(card_game))

Author
==================
Copyright 2015 Adrien Todeschini <adrien.todeschini@gmail.com>

Released under the GPL-2 licensed

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
Version 0.1 (01-2015) :
-----------------------
- [x] the thief can NOT steal the murdered role
- [x] each player receives 4 quarters and 2 coins at the beginning
- [x] draw the crown player at the beginning
- [x] adjust probabilities of `choose_random_action`
- [x] the magician can, at ANY moment in his turn
  - exchange ALL his hand against another player's hand
  - OR discard a number of cards and draw the same number IN THE DECK
- [x] the condottiere can NOT attack a city already over (with 8 quarters)

Todo
==================
- [ ] possibility to build zero quarters
- [ ] possibility to destroy zero quarters
- [ ] a player can NOT have 2 quarters with the same name in his city
- [ ] the king, the bishop, the merchant and the condottiere can collect coins
  at ANY moment in their turn
- [ ] implement special purple cards
- [ ] implement new roles
