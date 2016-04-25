/*jshint loopfunc: true */

// generate all subsequent states from this one
function runStep(state) {
    var linksToBeFlowed = findLinksToBeFlowed(state);
    if (linksToBeFlowed.length) {
        return linksToBeFlowed.map(function (link) {
            flowLink(state, link);
        });
    }

    var pathsToBeLinked = findPathsToBeLinked(state);
    if (pathsToBeLinked) {
        return pathsToBeLinked.map(function (path) {
            createLink(state, path);
        });
    }

    return null;
}

function findLinksToBeFlowed(state) {
    var result = [];

    state.links.forEach(function (link) {
        if (link.source(state).canSend()) {
            if (link.dests(state).some(function (dest) {
                return dest.canReceive(link.source(state).outputColor());
            })) {
                result.push(link);
            }
        }
    });

    return result;
}

function findPathsToBeLinked(state)  {
    var result = [];

    state.paths.forEach(function (path) {
        if (path.source(state).canSend()) {
            var color = path.source(state).outputColor();
            if (path.dest(state).canReceive(color)) {
                result.push(path);
            }
        }
    });

    return result;
}

function flowLink(state, link) {
    state = duplicate(state);

    var source = link.source(state);
    var color = source.outputColor();
    source.decrementMana(state);

    link.dests(state).forEach(function (dest) {
        dest.receiveMana(state, color);
    });

    return state;
}

function createLink(state, path) {
    state = duplicate(state);
    state.links.push(newLink(path.sourceID, [path.destID]));
    return state;
}

function newLink(sourceID, destIDs) {
    return {
        sourceID: sourceID,
        destIDs: destIDs
    };
}

var networks = {
    '3-4': {
        nodes: {
            a: 'sender',
            b: 'sender',
            c: 'sender',
            d: 'sender'
        },
        mana: {
            a: 0,
            b: 1,
            c: 1,
            d: 0
        },
        limits: {
            a: 1,
            b: 2,
            c: 2,
            d: 1
        },
        multiPaths: {
            a: ['b', 'c', 'd'],
            b: ['a', 'c', 'd'],
            c: ['a', 'b', 'd'],
            d: ['a', 'b', 'c']
        },
        links: [
        ]
    }
};

function initState(state) {
    state = duplicate(state);

    state.paths = [];
    for (var sourceID in state.multiPaths) {
        state.multiPaths[sourceID].forEach(function (destID) {
            state.paths.push({
                sourceID: sourceID,
                destID: destID,
                source: function (state) {
                    return state.nodes[this.sourceID];
                },
                dest: function (state) {
                    return state.nodes[this.destID];
                }
            });
        });
    }

    return state;
}

function duplicate(obj) {
    return JSON.parse(JSON.stringify(obj));
}

console.log(networks['3-4']);

var state = initState(networks['3-4']);

console.log(runStep(state));
