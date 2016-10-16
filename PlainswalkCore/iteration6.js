
// events and effects are everything.
// effects are proto-events.
// backreferences exist.
// no code!!

/*
Incremental Blight
3BB
neg a target creature, neg neg another target creature, then neg neg neg a third target creature.

at time of resolution, some of the events should look like this:

{ name: 'neg', source_object: 44, target: 11, amount: 1 }
{ name: 'neg', source_object: 44, target: 12, amount: 2 }
{ name: 'neg', source_object: 44, target: 13, amount: 3 }

so the object should therefore look like this, just before resolution:

{
    name: 'Incremental Blight',
    types: ['Sorcery'],
    id: '44',
    targets: [11, 12, 13],
    costs: [
        { name: 'targetSomething', query: '<querystring>' }
    ],
    resolutions: [
        { name: 'neg', source_object: 44, target: $source_object.target[0], amount: 1 }
        { name: 'neg', source_object: 44, target: $source_object.target[1], amount: 2 }
        { name: 'neg', source_object: 44, target: $source_object.target[2], amount: 3 }
    ]
}

which means the object should look like this as it gets constructed on the stack:

event: cast
action: create on stack

{
    name: 'Incremental Blight'
    types: ['Sorcery'],
    id: '44',
    costs: [
        { name: 'targetSomething', query: '<querystring>' }
    ],
    resolutions: [
        { name: 'neg', source_object: 44, subject: $source_object.target[0], amount: 1 }
        { name: 'neg', source_object: 44, subject: $source_object.target[1], amount: 2 }
        { name: 'neg', source_object: 44, subject: $source_object.target[2], amount: 3 }
    ]
}

event: the card moves from (hand?) to stack
action: pay costs

{
    name: 'Incremental Blight'
    types: ['Sorcery'],
    id: '44',
    costs: [
        { name: 'targetSomething', query: '<querystring>' }
    ],
    targets: [11, 12, 13],
    resolutions: [
        { name: 'neg', source_object: 44, subject: $source_object.target[0], amount: 1 }
        { name: 'neg', source_object: 44, subject: $source_object.target[1], amount: 2 }
        { name: 'neg', source_object: 44, subject: $source_object.target[2], amount: 3 }
    ]
}

event: #11 is targeted
event: #12 is targeted
event: #13 is targeted

*/

/*
Clone
3U
Creature - Shapeshifter
* / *
When this enters the battlefield, choose a creature. Clone becomes a copy of that guy.
*/

var clone_trigger = {
    triggeredAbilities: [
        {
            trigger: { event: 'rezone', subject: '$source_object.id', toZone: 'battlefield' },
            effects: [
                { effect: 'copy', subject: '$source_object.id', fromObject: { choose: { types: 'Creature', zone: 'battlefield', id: '!$source_object.id' } } }
            ]
        }
    ]
}

/*
Damnation
2BB
Sorcery
Destroy all creatures. They can't be regenerated.
 */

var damnation_effects = {
    effects: [
        {
            effect: 'cannot happen',
            subject: {
                event: 'regenerate',
                subject: {
                    types: 'Creature',
                    zone: 'battlefield'
                }
            }
        }
        {
            effect: 'destroy',
            subject: {
                types: 'Creature',
                zone: 'battlefield'
            }
        }
    ]
}

/*
Demonic Pact
2BB
Enchantment
At the beginning of your upkeep, choose one that hasn't been chosen —
• Demonic Pact deals 4 damage to target creature or player and you gain 4 life.
• Target opponent discards two cards.
• Draw two cards.
• You lose the game.

how to implement that??
1) represent the choices as emblems. whenever it's your upkeep:
    a) create 4 emblems
    b) destroy the ones that have already been picked (hmmmmmm how does that event look exactly? don't think this will work with multiple instances)
    c) pick one of the ones remaining
    d) resolve it
    e) destroy the others

2) represent the choices as abilities that can't normally be activated. this way the event log can track which ones got executed.
3) create a custom effect called 'select_mode_never_chosen' -- it will only ever be used by this one card so just implement it with code rather than twisting the system to fit it


note that you can't *choose* one twice even if it never resolves (interesting!)

here's the full trigger:
a) it's your upkeep. the ability goes on the stack.
b) as part of "casting" the ability, choose a mode. record that choice.
 -- if no choosable modes remain, then it's an empty stack ability that does no-op.
OR
a) it's your upkeep. pick a mode and that one goes on the stack.
c) now that the mode is chosen, choose targets.
 -- you can't choose a mode if it has no valid targets (!)
 */

var pact_attempt1 = {
    triggeredAbilities: [
        {
            trigger: { event: 'step', name: 'upkeep' },
            effects: [
                {
                    effect: 'choose_effect_never_chosen_before', options: [
                        {
                            name: 'effect_sequence', sequence: [
                                { 'name': 'damage_target' }
                            ]
                        }
                    ]
                }
            ],
            trigger: { event: 'choose', source_object: '$source_object.id' },
            effects: [
                { effect: 'copy', subject: '$source_object.id', fromObject: { types: 'Creature', zone: 'battlefield', id: '! $source_object.id' } }
            ]
        }
    ]
}

var pact_attempt2 = {
    triggeredAbilities: [
        {
            trigger: { event: 'step', name: 'upkeep' },
            effects: [
                {
                    effect: 'choose_new_ability', candidates: [
                        {
                            types: ['Ability'],
                            costs: [
                                { effect: 'choose_target', source_object: '$source_object.id', candidates: [ { type: 'Creature', zone: 'battlefield' }, { type: 'Player' } ] }
                            ],
                            resolutions: [
                                { effect: 'damage', source_object: '$source_object.id', subject: '$source_object.target[0]', amount: 4 }
                            ]
                        },
                        {
                            types: ['Ability'],
                            costs: [
                                { effect: 'choose_target', source_object: '$source_object.id', candidates: [ { type: 'Player', controller: '!$source_object.controller' } ] }
                            ],
                            resolutions: [
                                { effect: 'discard', source_object: '$source_object.id', subject: '$source_object.target[0]', amount: 2 }
                            ]
                        },
                        {
                            types: ['Ability'],
                            resolutions: [
                                { effect: 'draw', source_object: '$source_object.id', amount: 2 }
                            ]
                        },
                        {
                            types: ['Ability'],
                            resolutions: [
                                { effect: 'lose_the_game', source_object: '$source_object.id' }
                            ]
                        },
                    ]
                }
            ]
        }
    ]
}

// [ { type: 'Creature', zone: 'battlefield' }, { type: 'Player' } ]
function search_objects(query) {
    return objects.filter(function (object) {
        return query.every(function (query_component) {
            return query_component_matches(query_component, object);
        });
    });
}

function query_component_matches(query_component, object) {
    return Object.keys(query_component).every(function (predicate) {
        return query_predicate_matches(predicate, query_component[predicate], object);
    });
}

function query_predicate_matches(predicate_key, predicate_value, object) {
    if (predicate_value[0] === '!') {
        return !query_predicate_matches(predicate_key, predicate_value.substr(1).trim(), object);
    }

    if (answer[0] === '$') {
        something;
    }

    if (!predicate_value.isArray) {
        predicate_value = [predicate_value];
    }

    var object_value = object[predicate_key];
    if (!object_value.isArray()) {
        object_value = [object_value];
    }

    return predicate_value.every(function (value) {
        object_value.containsValue(value);
    });
}

// { effect: 'choose_target', candidates: [ { type: 'Creature', zone: 'battlefield' }, { type: 'Player' } ] }
// input: source_object and stuff
// output: source_effect converted to a series of events
function choose_target(effect) {
    var co = choose_object(effect);
    return [
        co,
        {
            event: 'update_object',
            subject: effect.source_object.id,
            concatenate: {
                targets: co.results
            }
        };
    ]
}

var incremental_blight = {
    name: 'Incremental Blight'
    types: ['Sorcery'],
    id: '44',
    costs: [
        { name: 'targetSomething', query: '<querystring>' }
    ],
    targets: [11, 12, 13],
    resolutions: [
        { name: 'neg', source_object: 44, subject: $source_object.target[0], amount: 1 }
        { name: 'neg', source_object: 44, subject: $source_object.target[1], amount: 2 }
        { name: 'neg', source_object: 44, subject: $source_object.target[2], amount: 3 }
    ]
}
