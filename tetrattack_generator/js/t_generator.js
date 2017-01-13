

// return an action that could be applied to this board to generate stuff
// Chooses randomly!
function generate_next_thing(board) {
    // keep generating until we make one that doesn't suck
    var action = null;
    while (action === null) {
        const matches = board_get_matches(board);
        if (matches.length) {
            action = generate_next_move(matches);
        } else {
            if (random_bool()) {
                action = generate_next_move();
            } else {
                action = generate_next_create(board);
            }
        }
    }
    return action;
}

function board_cell_matches(board, pos, iterate_function) {
    const color = board.cells.get(pos);
    const match_length = 3;
    let iterator = pos;
    
    for (let i = 0; i < match_length; ++i) {
        iterator = iterate_function(iterator);
        if (board.cells.get(iterator) !== color) return false;
    }
    
    return true;
}

function board_get_matches(board) {
    const matches = [];
    const match_length = 3;
    
    for (let x = 0; x < board.num_columns; ++x) {
        for (let y = 0; y < board.num_rows; ++y) {
            const pos = {x, y};
            if (board_cell_matches(board, pos, pos_above) || board_cell_matches(board, pos, pos_right)) {
                matches.push(pos);
            }
        }
    }

    return matches;
}

function line_is_supported(board, line) {
    switch (line.isVert) {
        case true: return cell_is_supported(board, line.pos);
        case false: return line_cells(line).all(function (pos) { return cell_is_supported(board, pos); });
    }
}

function cell_is_supported(board, pos) {
    return pos.y === 0 || board.cells.get(pos_below(pos));
}

/*
-- generate a new line
--  horz or vert
--  position
--  color
--  length (todo: combos?)
-- make sure it's valid -- don't create a horz floating in space
-- generate a whole ton of them and pick the first good one
*/

/*
 * line:
 * {
 *     pos: pos,
 *     color: color,
 *     isVert: bool,
 *     length: int,
 *     cells(): [pos]
 * }
 */

function line_cells(line) {
    const result = [];
    let pos = line.pos;
    for (let i = 0; i < line.length; ++i) {
        result.push(pos);
        if (isVert) {
            pos = pos_above(pos);
        } else {
            pos = pos_right(pos);
        }
    }
    return result;
}

function generate_next_create(board) {
    const color = random_element(colors);
    const length = random_int_inclusive(3, 5);
    const x = random_int_exclusive(0, board.num_columns);
    const y = random_int_exclusive(0, board.num_rows);
    const isVert = random_bool();
    const line = {
        pos: {x, y},
        color,
        isVert,
        length
    };
    if (line_is_supported(board, line)) {
        return line;
    } else {
        return null;
    }
}

function generate_next_move(board) {
    const pos = random_element(board.cells).keys;

    if (!board.cells.get(pos)) {
        return null;
    }

    const candidates = generate_next_move_candidates(board, pos, pos);
    if (candidates && candidates.length) {
        const action = random_element(candidates);
        if (!board_get_matches(apply_action(board, action)).length) {
            return null;
        } else {
            return action;
        }
    } else {
        return null;
    }
}

function generate_next_move_candidates(board, pos, orig_pos) {
    // 2 kinds of move:
    const color = board.cells.get(pos);
    const results = [];
    
    // 1) swap with a full peer (swap)
    // this can't happen when we're above the pos (color == null protects us from this)
    
    if (color) {
        if (pos.x > 0 && board.cells.get(pos_left(pos)) !== color) {
            results.push(prepare_swap(pos_left(pos), pos));
        }

        if (pos_right(pos.x) < board.num_columns && board.cells.get(pos_right(pos)) !== color) {
            results.push(prepare_swap(pos, pos_right(pos)));
        }
    }
    
    // 2) move stuff from a column beside
    // in reverse this looks like taking the orig cell, maybe moving it up, then pushing it left or right (and shuffling other stuff up)
    if (pos.x > 0 && cell_is_supported(board, pos_left(pos))) {
        results.push(prepare_move(orig_pos, pos_left(pos)));
    }

    if (pos_right(pos.x) < board.num_columns && cell_is_supported(board, pos_right(pos))) {
        results.push(prepare_move(orig_pos, pos_right(pos)));
    }
    
    if (results.length && !board.cells.get(pos_above(pos))) {
        return results ++ generate_next_move_candidates(board, pos_above(pos), orig_pos);
    } else {
        return results;
    }
}
