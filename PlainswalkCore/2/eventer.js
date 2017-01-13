
/*
var event_log = [
    { "action": "declare_cast", "card": "<card>" },
    { "action": "clone", "subject": "<spell stub>", "zone": "stack" },
    { "action": "move", "subject": "<spell>", "destination": "stack" },
    { "action": "choose_all_modes", "subject": "<spell>" },
    { "action": "choose_all_targets", "subject": "<spell>" },
    { "action": "check_stack_legality", "subject": "<spell>" },
    { "action": "pay_costs", "subject": "<spell>" },
    { "action": "cast", "subject": "<spell>" }
]
*/

// -- utility --
function sequence(first_number, max) {
    const result = [];
    for (var i = first_number; i < max; ++i) {
        result.push(i);
    }
    return result;
}

function wrapping_sequence(first_number, max) {
    const result = [];
    for (var i = first_number; i < max; ++i) {
        result.push(i);
    }
    for (i = 0; i < first_number; ++i) {
        result.push(i);
    }
    return result;
}

function flatten(things) {
    const result = things.map(function (thing) {
        if (thing.constructor === Array) {
            return flatten(thing);
        } else {
            return thing;
        }
    });
    return [].concat(...result);
}

// ---------------

const event_log = [];
const trigger_pool = [];

const event_resolutions = {
};

function raise_event(event) {
    event_log.add(event);
    query_triggers(event).each(function (trigger) {
        trigger_pool.add(clone(trigger));
        attach(trigger, event);
    });
    resolve_event(event);
}

function query_triggers(event) {
    return query_objects({
        'zone': 'triggers',
        'event': event
    });
}

function resolve_triggers(active_player) {
    wrapping_sequence(active_player, num_players).each(function (player_id) {
        const triggers = trigger_pool.filter(function (trigger) { return trigger.controller === player_id; });
        if (triggers.length > 1) {
            raise_event({ 'action': 'choose_trigger', 'controller': player_id, 'triggers': trigger_pool });
            return true;
        } else if (triggers.length === 1) {
            resolve_trigger(trigger);
        }
    });

    return false;
}
