
function find_milestone_breakeven_time(milestone, prerequisite, target) {
    return find_milestone_price(milestone, prerequisite) / find_milestone_dps(milestone, prerequisite, target);
}

function find_milestone_dps(milestone, prerequisite, target) {
    const quantity = Math.max(0, milestone.quantity - prerequisite.quantity);
    const old_target_dps = target.quantity * target.dps;
    const new_target_dps = old_target_dps * milestone.multiplier;
    const diff_prerequisite_dps = prerequisite.dps * quantity;
    return diff_prerequisite_dps + new_target_dps - old_target_dps;
}

function find_milestone_price(milestone, prerequisite) {
    const quantity = Math.max(0, milestone.quantity - prerequisite.quantity);
    return property_price(prerequisite.coefficient, prerequisite.unit_price, quantity);
}

function property_price(coefficient, unit_price, quantity) {
    let result = 0;
    while (quantity > 0) {
        result += unit_price;
        unit_price *= coefficient;
        --quantity;
    }
    return result;
}

