-module(warden_tests).
-include_lib("eunit/include/eunit.hrl").

%%test to check that prisonerlist increments by 1 when prisoner added
addPrisonerToList_test() ->
  WardenPID = warden:start({[],[]}),
  P1_PID = prisoner:create("prisonerA", random, []),
  ListCount = warden:add(WardenPID,P1_PID),
  ?assert(1=:= ListCount).


%%test to check that rankScores() sorts a given list of tuples by the 3rd tuple element
rankScores_test() ->
  P1_PID = prisoner:create("prisonerA", random, []),
  P2_PID = prisoner:create("prisonerB", random, []),
  TestTupleList = [{"john", P1_PID, 123 },{"kevin", P2_PID, 103}],
  ?assert([{"kevin", P2_PID, 103},{"john", P1_PID, 123 }] =:= warden:rankScores(TestTupleList)).