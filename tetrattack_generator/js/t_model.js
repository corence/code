
const colors = ['red', 'yellow', 'blue', 'green', 'pink', 'black', 'orange'];

/* board:
 * {
 *  num_rows: 0,
 *  num_columns: 0,
 *  cells: Map
 * }
 */

/* pos: { x: 3, y: 9 } */

function prepare_swap(pos1, pos2) {
    return function swap(board) {
        color1 = board.cells.get(pos1);
        color2 = board.cells.get(pos2);
        board.cells.put(pos2, color1);
        board.cells.put(pos1, color2);
        return board;
    };
}

function prepare_remove(pos) {
    return function remove(board) {
        board.cells.set(pos, null);
        return board;
    };
}

function prepare_move(pos_source, pos_destination) {
    return function move(board) {
        const color = board.cells.get(pos_source);
        const board1 = prepare_remove(pos_source)(board);
        return prepare_insert(pos_destination, color)(board);
    };
}

function prepare_insert(pos, color) {
    return function insert(board) {
        if (board.cells.get(dest_pos)) {
            prepare_move(pos, pos_above(pos))(board);
        }
        board.cells.set(pos, color);
        return board;
    };
}

function pos_add(pos1, pos2) {
    if (!pos1) {
        return pos2;
    }
    if (!pos2) {
        return pos1;
    }
    return {
        x: pos1.x + pos2.x,
        y: pos1.y + pos2.y
    };
}

function pos_above(pos) {
    return pos_add(pos, {x: 0, y: 1});
}

function pos_below(pos) {
    return pos_add(pos, {x: 0, y: -1});
}

function pos_left(pos) {
    return pos_add(pos, {x: -1, y: 0});
}

function pos_below(pos) {
    return pos_add(pos, {x: 1, y: 0});
}
