
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

function raise_event(event) {
    event_log.add(event);
    query_triggers(event).each(function (trigger) {
        trigger_pool.add(clone(trigger));
        attach(trigger, event);
    });
    process_event(event);
}

raise_event({ "action": "declare_cast", "card": "<card>" });
trigger_pool.add({ "action": "clone", "subject": "<spell stub>", "zone": "stack" });
trigger_pool.each(function (trigger) {
    raise_event(trigger);
});
raise_event({ "action": "move", "subject": "<spell>", "destination": "stack" });
trigger_pool.add("all of the things that are triggered by stack movements");
trigger_pool.each(function (trigger) {
    raise_event(trigger);
});

    { "action": "choose_all_modes", "subject": "<spell>" },
    { "action": "choose_all_targets", "subject": "<spell>" },
    { "action": "check_stack_legality", "subject": "<spell>" },
    { "action": "pay_costs", "subject": "<spell>" },
    { "action": "cast", "subject": "<spell>" }

var declare_cast_trigger = {
    "action": "create_trigger",
    "event": { "action": "declare_cast" },
    "resolution": [
        {
            "action": "clone",
            "subject": "$card",
            "zone": "stack"
        }
    ]
}

var stack_trigger = {
    "action": "create_trigger",
    "event": { "action": "move", "destination": "stack" },
    "resolution": [
        {
            "action": "choose_all_modes",
            "subject": "$subject"
        },
        {
            "action": "choose_all_targets",
            "subject": "$subject"
        },
        {
            "action": "check_stack_legality",
            "subject": "$subject"
        },
        {
            "action": "pay_costs",
            "subject": "$subject"
        },
        {
            "action": "cast",
            "subject": "$subject"
        }
    ]
}

const choose_all_modes = {
    "action": "create_trigger",
    "event": { "action": "choose_all_modes" },
    "resolution": [
        {
            "action": "player_choose_mode",
            "subject": "$subject"
        }
    ]
};


var card = {
    name: "Joraga Warcaller",
    colors: ["green"],
    types: ["Creature"],
    subtypes: ["Elf", "Warrior"],
    resolutions: ["summon"],
    controller: "player1",
    costs: {
        mana_cost: "G"
    }
    modes: ["multikicker 1G"]
}

