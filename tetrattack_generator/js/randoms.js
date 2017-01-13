
function random_bool() {
    return Math.random() >= 0.5;
}

// eg random_int_inclusive(3, 5) could generate 3, 4, or 5
function random_int_inclusive(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

// eg random_int_exclusive(3, 5) could generate 3, 4
function random_int_exclusive(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

function random_element(array) {
    return array[random_int_exclusive(0, array.length)];
}
