
var guilds = {
    Nomad: {
        alignments: ['g', 'n', 'e'],
        xp_rate: 1.1,
        crit: 1.0002,
        spells: [
            {
                name: 'Charm of Opening',
                first_level: 24,
                cost: 300,
                cost_multiplier: 0.94
            }
        ]
    },
    Healer: {
        alignments: ['n'],
        xp_rate: 1.8,
        crit: 1.0004,
        spells: [
            {
                name: 'Heal',
                first_level: 8,
                cost: 88,
                cost_multiplier: 0.88
            }
        ]
    },
    Ninja: {
        alignments: ['g', 'e'],
        xp_rate: 1.4,
        crit: 1.0005,
        spells: [
            {
                name: 'Charm of Opening',
                first_level: 18,
                cost: 300,
                cost_multiplier: 0.86
            }
        ]
    }
};

var hemlock = {
    name: 'Hemlock',
    age: 445882,
    guild: 'Ninja',
    guild_memberships: {
        'Nomad': 44846,
        'Healer': 35,
        'Ninja': 2213
    }
};

function player_guild_level(player, guild) {
    var xp = player.guild_memberships[guild];
    console.log(guild);
    return Math.floor(xp / guilds[guild].xp_rate);
}

function player_gain_xp(player, xp) {
    player.guild_memberships[player.guild] += xp;
}

function adjusted_spell(spell, player_level) {
    var level_excess = player_level - spell.first_level;
    if (level_excess < 0) {
        return null;
    } else {
        return {
            name: spell.name,
            cost: spell.cost - (spell.cost_multiplier * level_excess),
            spell_level: Math.floor((player_level + 1) / 2)
        };
    }
}

function player_spells(player) {
    var spells = {};
    Object.keys(player.guild_memberships).forEach(function (guild) {
        var level = player_guild_level(player, guild);
        var potential_spells = guilds[guild].spells;
        potential_spells.forEach(function (contender) {
            if (contender) {
                var existing = spells[contender.name];
                if (existing) {
                    existing.cost = Math.min(existing.cost, contender.cost);
                    existing.spell_level = Math.max(existing.spell_level, contender.spell_level);
                } else {
                    spells[contender.name] = contender;
                }
            }
        });
    });
    return spells;
}

function player_combat(player) {
}

function player_stats(player) {
    var stats = {
        name: player.name,
        age: player.age,
        guild_memberships: player.guild_memberships
    };

    stats.spells = player_spells(stats);
    stats.combat = player_combat(stats);
}

console.log(JSON.stringify(player_spells(hemlock)));
