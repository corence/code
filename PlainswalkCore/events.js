
//--some events:
 //--- when a creature has 0 toughness, move it to the graveyard
 //--- when a player plays a blue spell, gain 1 life
 //--- when you would draw a card, draw 2 cards
 //--- when a player would gain life, do nothing

var battlefield = {
    name: 'Battlefield',
    types: ['zone']
}

var nyxFleeceRam = {
    name: 'Nyx-Fleece Ram',
    types: ['creature', 'enchantment']
    power: 0,
    toughness: 5,
    color: 'W',
    abilities: [
        {
            costs: [
                trigger,
                query({ name: 'step', card: upkeep })
            ]
            results:  [
                stack(function () { gainLife(controller(this)); })
            ]
        }
    ]
}
// note that permanents and spells don't have a zone.

// how to handle targeting & attachments:
// a) an intrinsic property of their owners. This means that each card on the stack is associated with a Spell on the stack -- that links to its own card. all of the targets and choices are hard-coded in the spell. If a target is changed, the spell is destroyed and recreated. (this shouldn't be a problem because nothing can link to the spell -- only to its card.)
// b) a distinct object. In this case we'd have the following types of Thing: Link, Card. also maybe: Event, Permanent, Spell, Counter. In this case, switching targets is easier. This object is:
//   {
//       name: 'target1',
//       from: incrementalGrowthCardOnStack,
//       unto: grizzlyBearsCardOnBattlefield
//   }
// *should it be a card?*
// yes it should: because it will need triggered abilities:
//   -- if the link target moves from the battlezone, destroy this
//   -- when this first appears, trigger the 'target' event
// no it shouldn't: because it's not a fucking card! it'll never get targeted and it will need a query lookup system -- just like events.
// 

// hmmmm
// things that need queries:
//  1) Links (we just gained protection from red. are there any red spells targeting this permanent?) (are there any cards that don't belong to a zone? if so, exile them.)
//  2) Cards (are there any creature cards in the graveyard owned by player1?)
//  3) maybe: Abilities (it's the beginning of my main phase. what are my options right now?)
//  4) maybe: Triggered Abilities (it's the beginning of the upkeep step. What happens before priority?)
//  5) Events (an event happened -- was it a creature moving from the battlefield to the graveyard? if so, which?) (was it someone dealing damage to someone else? from, unto, how much?)

// 3rd option: *blur* the distinction between each of these things. the following definitions will hold:
// link: {
//     type: ['link']
//     from: card
//     unto: card
//     name: string
// }
// card: {
//     type: [<various, all strings>]
//     owner: playerID
//     power: int <optional>
//     toughness: int <optional>
//     manaCost: ManaCost <optional>
//     abilities: [Ability] -- this includes triggered, static and mana abilities
// }
// ability: {
//     costs: {
//         name: Ability
//     }
//     results: [Action]
// }

// queries:
// 1) what can i do right now?
var abilities = query(function (card) {
    var controlledByMe = {
        type: ['link'],
        from: player1,
        name: 'control',
        unto: card
    }
    if(!scan(controlledByMe)) {
        return [];
    }

    
});

var target = {
    name: 'target',
    abilities: [
        {
            costs: [
                static,
                from: incrementalGrowth,
                unto: nyxFleeceRam
            ]
        }
    ]
}

var giantStrength = {
    function cast() {
        var board2 = payManaCost('R', board);
        var target = chooseCard(query('creature'));
        var board3 = setTarget(something);
        var board4 = moveCard(this, stack);
        
        
        
        
        // 1) declare casting
        // 2) pay costs
        // 3) choose targets
        // 4) move to stack
        // moving to the stack creates a spell (if we need one, but i doubt it)
        // 5) wait for resolve
        
        // on resolve:
        // 1) confirm targets
        // 2) create permanent
        // 3) create attachment between this card and that card
        // 4) move card to battlefield
        // moving from the stack triggers spell evaporation
    }
    
    abilities: [
        {
        }
    ]
}

var rakingCanopy = {
    name: 'Raking Canopy',
    types: ['enchantment'],
    power: 0,
    toughness: 0,
    color: 'G',
    abilities: [
        {
            costs: [
                trigger,
                query({ name: 'attack', kk
        }
    ]
}

// blue rod
// incremental growth
// grave pact
// raking canopy

events = [
    {
        name: 'cast',
        card: nyxFleeceRam,
        board: null
    },
    {
        name: 'move',
        card: nyxFleeceRam,
        from: stack,
        into: battlefield
    },
    {
        name: 'target',
        card: nyxFleeceRam,
        source: lightningBolt
    }
]

/*
links = [
    {
        from: battlefield,
        unto: nyxFleeceRam
    }
]
*/
