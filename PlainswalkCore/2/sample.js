
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


const card = JSON.stringify({
    'name': 'Natural Spring',
    'color': ['green'],
    'costs': {
        'mana': '3GG'
    },
    'type': ['Sorcery'],
    'spell_stub': {
        'resolution': [
            {
                'action': 'gain_life',
                'amount': 8
            }
        ]
    }
});
