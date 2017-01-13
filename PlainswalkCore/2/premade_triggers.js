
/*
* trigger layers:
 - state-based effect
 - self-replacement effect
 - modify whose control under which a permanent enters the battlefield effect (!)
 - some permanent enters the battlefield as a clone of another object effect (!)
 - replacement effect (including damage prevention effects)
 - pre-trigger effect (eg Deathgrunter)
 - the actual event
 - post-trigger effect
*/

const trigger_ranks = [
    'state-based',
    'self-replacement',
    'etb control-changing',
    'etb cloning',
    'replacement',
    'pre-event',
    'post-event'
];

const fail = {
    "action": "fail"
}

const split_second = {
    "action": "create_trigger",
    "source_zone": "stack",
    "event": { "action": "move", "destination": "stack" },
    "resolution": [{ fail }],
    "rank": "replacement"
}

const flash = {
    "action": "create_trigger",
    "source_zone": "all",
    "event": { "action": "check_sorcery_cast_restriction", "subject": "$me" },
    "resolution": [],
    "rank": "replacement"
}

const declare_cast_causes_move_to_stack = {
    "action": "create_trigger",
    "source_zone": "all",
    "event": { "action": "declare_cast" },
}
    { "action": "declare_cast", "card": "<card>" },
