include "member.mzn";
include "global_cardinality_low_up.mzn";
include "arg_min.mzn";
include "arg_max.mzn";

int: num_tribunals;
int: num_days;
int: num_time_steps;
int: initial_time_step;
int: num_start_times;
int: num_dissertations;
int: max_back_to_back = 4;
int: hours_preferred_over_day = 3;

set of int: Tribunals = 1..num_tribunals;
set of int: TribunalsAndNone = 0..num_tribunals;
set of int: Days = 1..num_days;
set of int: TimeSteps = 1..num_time_steps;
set of int: StartTimes = 1..num_start_times;

array[StartTimes] of 0..100: map_start_times;
array[Days, Tribunals, StartTimes] of 0..30: availability;

array[Days, TimeSteps] of var 0..(max_back_to_back * 3): durations;
array[Days, TimeSteps] of var TribunalsAndNone: schedule;

% Constrain schedule based on tribunal availability
constraint forall (d in Days, ts in TimeSteps) (
    schedule[d, ts] in {t * 
        bool2int(exists(st in StartTimes) (
            t > 0 /\
            map_start_times[st] <= ts + initial_time_step - 1 /\
            map_start_times[st] + availability[d, t, st] >= ts + initial_time_step
        )) | t in TribunalsAndNone}
);

% Fill in time slots in the schedule
constraint forall (d in Days, ts in TimeSteps) (
    if ts > 1 then
      durations[d, ts] = 
          bool2int(schedule[d, ts] != 0) *
              (1 + bool2int(schedule[d, ts - 1] == 0 \/ schedule[d, ts] == schedule[d, ts - 1])
                      * durations[d, ts - 1])
    else
      durations[d, ts] = bool2int(schedule[d, ts] != 0)
    endif
);

% Each dissertation should take 3 consecutive slots (45 minutes)
constraint forall (d in Days, ts in TimeSteps) (
    (ts = num_time_steps \/ durations[d, ts + 1] <= durations[d, ts])
    -> 
    durations[d, ts] mod 3 == 0
);

% All dissertations must be covered
constraint num_dissertations * 3 = sum (d in Days, ts in TimeSteps) (
    bool2int(schedule[d, ts] != 0)
);

% There should be a minimun number of dissertations per tribunal
int: num_diss_per_trib_min = num_dissertations div num_tribunals;
constraint global_cardinality_low_up(
    [schedule[d, ts] | d in Days, ts in TimeSteps], 
    [t | t in Tribunals], 
    [num_diss_per_trib_min * 3 | t in Tribunals],
    [(num_diss_per_trib_min + 1) * 3 | t in Tribunals]);

% Calculate the cost of the schedule in terms of days
var int: Day_Cost;
constraint Day_Cost >= 0;
constraint Day_Cost = sum (d in Days, t in Tribunals) (
    bool2int(member(schedule[d, TimeSteps], t))
) - num_tribunals;

% Calculate the cost of the schedule in terms of hours (unsued space)
var int: Hour_Cost;
constraint Hour_Cost >= 0;
constraint Hour_Cost = sum (t in Tribunals, d in Days) (
    if member(schedule[d,TimeSteps], t) then 
        sum (ts in 1..(num_time_steps-1)) (
              ts * bool2int(schedule[d,ts] != t /\ schedule[d,ts + 1] == t)
            - ts * bool2int(schedule[d,ts] == t /\ schedule[d,ts + 1] != t)
        ) - (
            if schedule[d,1] != t then 
                arg_min([if schedule[d, ts] == t then ts else 101 endif | ts in TimeSteps]) - 1
            else 0 endif
        ) + (
            if schedule[d,num_time_steps] != t then 
                arg_max([if schedule[d, ts] == t then ts else 0 endif | ts in TimeSteps])
            else 0 endif
        )
    else 0 endif
);

schedule = 
[| 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
 | 3, 3, 3, 3, 3, 3, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1
 | 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
 |];

solve minimize Hour_Cost + hours_preferred_over_day * 4 * Day_Cost;