include "all_different.mzn";
include "member.mzn";
include "globals.mzn";

enum DissertationProfessors = {Tutor, Cotutor, Vocal, Covocal};
enum Slot = {Day, Time, President, Secretary};

int: num_dissertations;
int: num_professors;
int: num_days;

set of int: Days = 1..num_days;
set of int: Dissertations = 1..num_dissertations;
set of int: Professors = 1..num_professors;
set of int: ProfessorsAndNone = 0..num_professors;
set of int: Tribunals = 1..6;

array[Dissertations, DissertationProfessors] of ProfessorsAndNone: dissertations;
array[Dissertations, Slot] of int: slots;

array[Dissertations] of var Dissertations: assignments;

constraint all_different(assignments);

constraint forall (d in Dissertations) (
    alldifferent_except_0([
        slots[d, President], 
        slots[d, Secretary], 
        dissertations[assignments[d], Tutor],
        dissertations[assignments[d], Cotutor],
        dissertations[assignments[d], Vocal],
        dissertations[assignments[d], Covocal]
    ])
);

var int: Day_Cost;
constraint Day_Cost = sum (p in Professors) (
    nvalue([(slots[d, Day] + 1) * bool2int(member([
        slots[d, President], 
        slots[d, Secretary], 
        dissertations[assignments[d], Tutor],
        dissertations[assignments[d], Cotutor],
        dissertations[assignments[d], Vocal],
        dissertations[assignments[d], Covocal]
    ], p)) | d in Dissertations]) - 1
) - num_professors;

% Solve with Chuffed
% assignments = [2 ,3 ,6 ,9 ,8 ,4 ,14 ,15 ,16 ,17 ,7 ,10 ,11 ,12 ,13 ,5 ,18 ,19 ,1];

constraint assignments[1] = 2;
constraint assignments[2] = 3;
constraint assignments[3] = 6;

constraint assignments[4] = 9;

constraint assignments[6] = 4;
constraint assignments[7] = 14;

constraint assignments[12] = 10;
constraint assignments[13] = 11;
constraint assignments[14] = 12;

constraint assignments[16] = 5;

Day_Cost = 4;

solve satisfy;