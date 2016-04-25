
% solveStep(Cells, ChangedCell, NewCells).
solveStep([Cell|Cells], ChangedCell, NewCells) :-
    Cell = (Value, Sources, Dests),
    length(Sources, 1),
    
