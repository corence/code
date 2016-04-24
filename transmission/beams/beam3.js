
// generate all subsequent states from this one
function runStep(state) {
    var linkToFlow = findLinkToFlow(state);
    if (linkToFlow) {
        return flowLink(state, linkToFlow);
    }

    var pathToBeLinked = findPathToBeLinked(state);
    if (pathToBeLinked) {
        return createLink(state, pathToBeLinked);
    }

    return null;
}

function findLinkToFlow(state) {
    state.links().forEach(function (link) {
        if (link.source(state).canSend()) {
            if (link.dests(state).some(function (dest) {
                return dest.canReceive(link.source(state).outputColor());
            })) {
                return link;
            }
        }
    });
    return null;
}

function findPathToLink(state)  {
    state.paths().forEach(function (path) {
        if (path.source(state).canSend()) {
            var color = path.source(state).outputColor();
            if (path.dest(state).canReceive(color)) {
                return path;
            }
        }
    });
    return null;
}

function flowLink(state, link) {
    return newState;
}

function createLink(state, path) {
    return newState;
}
