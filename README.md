DivisionOfLabor
===============

## Abstract / Design Goals

The goal in all of this is to create a board game which utilizes the following

#### Modular character rules

- Each player can assume the role of a different character and take on their
  corresponding responsibilities and abilities
- The game has well defined rules for many subsets of available characters and
  remains balanced
- The number of characters that must be played is kept at a minimum, ideally
  one or two.
- Each pair of characters should be capable of rich interaction.
    - Maybe not?

#### Emphasis on trading

- Trading is allowed at all times between all players
- Absolutely anything can be traded, this includes, but is not limited to
    - Resources
    - Money
    - Victory Points
    - Decisions
    - Actions
        A player can effective rent out their actions to other players, so that
        player A can pay player B to take one of player B's available actions as
        if player A had that action available to them.
    - Futures (resources/actions/decisions etc)
- Trading can be between two players, or can be a more complex agreement
  between more parties
- All trades must be public unless a characters ability says otherwise

#### Encouragement of cooperation
- The richness in the character modularity should come through in the way that
  players work together to achieve both common and individual goals
- A game cannot be won unless every player meets a threshold of individual goals
  and/or all players together meet a threshold of common goals
- Players should have to balance helping each other out with holding onto a
  competitive advantage

#### Minimal chance
- Some chance in initial conditions but completely deterministic from then on
  out

#### Minimal dependency on an external/artificial resource market

## Character sketches

### The Farmer

The farmer's role is to gather, farm, mine, and otherwise collect raw materials
for use by all players. The farmer must decide which materials to produce and at
what time and for how long. Some resources that are important early in the game
may become less important as things progress, and the farmer needs to make sure
that these resources aren't being produced at the neglect of others.

The farmer leads a simple life and is largely dependent on the other players to
do anything interesting. On the other hand, the farmer forms the cornerstone of
the game and is necessary for the game to make any sense.

The types of materials that the farmer can produce include (with a potential to
specialize in each category):

- Wood
- Ore
- Livestock
- Grain
- Clay
- Oil

The farmer's goals revolve around producing large numbers of different resources
and with increasing efficiency.

### The Trader

The trader makes his living off of speculation. He begins the game with more
money than the other players and can use this to his advantage by controlling
resources and selling them for high prices or trading them as investments. The
trader will find himself making loans to the other players and getting involved
in their business.

The trader is allowed to make deals in secret, though the parties involved must
still be made public. When the trader makes a secret deal, the secret becomes
shared "property" of those involved in the deal and can be sold, traded, or
freely revealed only if all of it's owners are compliant.

The trader's goals revolve around making correct predictions of future values of
commodities.

### The Manufacturer

The manufacturer purchases raw materials from the farmer (perhaps with the help
of the trader) and creates finished or intermediary products. Products can be
used by the other players to increase efficiency of certain actions or provide
actions in and of themselves.

> In general, products should be general enough to be used by any player, though
> some will (must?) be geared toward specific players.

For example, the manufacturer may create an ax which allows the farmer to
receive extra wood whenever he takes an action that yields wood. A basket may
allow players to store more resources, a firepit may allow players to substitute
 wood for oil in any action.

The manufacturers goals revolve around around producing high end goods which
have many intermediary steps.

### The Electrician

The electrician controls the energy of the game. Players can exchange goods for
energy, but excess is given to electrician who can store energy in batteries.
This excess energy can be sold or traded.

### The Explorer

The explorer can uncover new parts of the world which can give other players
more room to store products, new spaces to recover natural resources, as well as
one time bonuses such as artifacts or natural resource deposits. The explorer is
able to send out scouts to get a sense of what terrain lies ahead before
actually claiming land. This gives the explorer the ability to sell his services
in exchange for direction the exploration in a particular direction.

The explorer is also responsible for setting up camps and towns in the world as
well as building and upgrading transportation between settlements.

The goals of the explorer revolve around discovering artificats, expanding the
known world, and building/linking settlements

### The Governor

The governor is tasked with keeping order and constructing building that work
towards a common good. The governor is in charge of enforcing all contracts
between players and can invoke a fine or a prison sentence for players who break
contracts. The governor can also impose a tax on players, and use that money
fund public projects to be voted on by the players, while of course keeping some
of the money as a salary.


## Gameplay Overview

The game begins with players colonizing a new world. They will do this revealing
pieces of the map board and placing their starting buildings on them. The
Explorer goes first and as the additional bonus of being able to look at
"coastal" tiles before deciding where to settle. The explorer picks a location
to settle, reveals the tile to the other players, and then places the first
city on this tile, along with a worker. Play then continues in normal player
order. Subsequent players must pick a space adjacent to an already discovered
space on the map. Once the space is determined, they will then place a starting
building on their first space.

Some buildings can only be built on certain kinds of tiles, for example the
farmer cannot build a field in mountains, but can build mines and

In subsequent turns, for each worker that a player has, they may do one of the
following:

* move the worker
* bring one of their active workers not currently on the map onto any unoccupied
  city  
* work the tile that the worker is currently on
* build a building from their hand

in addition to these actions, there are actions that a player may perform
asynchronously during any part of their turn.
These include:

* pick up resources from the tile
* drop off resources from the tile
* rearrange any resources held by the worker
* initiate trades with other players
* use any special ability granted by an item

### Moving

When a player elects to move one of their workers, they will first determine the
number of movement points that that worker has avaialble. Movement points will
come from the workers base movement point number and any upgrades that grant
additional movement points. The worker can move anywhere on the board that they
have enough movement points to get to. Traveling over mountains takes a lot of
movement points, traveling across hills takes less. Over the course of the game,
The Explorer will build paths/roads/railroads etc to decrease the number of
movement points it takes to move between any two given locations on the map.

### Resource Management

When a worker produces some resource or a player makes a trade that nets them
some resources, they need a place to put it. Workers can carry a certain number
of resources with them, determined by the worker type and any upgrades that the
worker has. Players can build storage areas on the board that will let them keep
more resources than their workers can carry. If at any point, a worker has more
goods than they can carry, they must leave them on the tile. Other workers
(including from other players) can pick these resources up. Resources in
storage areas cannot be taken by other players.

### Trading

Trading can be done between players at any time. There are two kinds of trades:

* physical trades
* agreements

#### Physical trades

A physical trade is when players want to exchange goods, upgrades, buildings,
etc. Basically anything that's actually on the board. In order to exchange
physical goods, the players have to have workers in adjacent tiles who are
carrying those goods. So if you have a wood farm on one side of the board and a
player on the other side wants wood, one of you are going to have to move a
worker from one side to the other (or meet in the middle) to get it.

Money can only be traded if both players are adjacent to or in a city. Any of a
players workers can be used trade any money that the player has, i.e. you don't
have to allocate your money amongst your workers like you have to with resources.

#### Agreements

Agreements are trades where the only thing exchanged is words. For example
"I'll build a workshop if you give me $100". These can happen at any time
between players regardless of worker locations, but they must be announced to
all players (the trader can make them secretly).



## Buildings

### Farmer

* Field (yields grain)
* Pasture (yields livestock)
* Mine (yields ore)
* (yields wood)
* Clay pit (yields clay)
* Oil well (yields oil)
