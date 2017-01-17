-module(prisoner_tests).

-include_lib("eunit/include/eunit.hrl").

%%-export([getChoice/2]).
%%
%%getChoice(PrisonerPID, OtherPrisoner) ->
%%  PrisonerPID!{self(), choose, OtherPrisoner},
%%  receive
%%    {PrisonerPID,choice,State} ->
%%      State
%%  end.


%% tests to check the correct sentence is returned with a given tuple
calculateSentenceCoopDefc_test() ->
  ?assert(3=:=prisoner:calculateSentence({"cooperated", "defected"})).

calculateSentenceCoopCoop_test() ->
  ?assert(1=:=prisoner:calculateSentence({"cooperated", "cooperated"})).

calculateSentence_testDefcDefc_test() ->
  ?assert(2=:=prisoner:calculateSentence({"defected", "defected"})).

calculateSentenceDefcCoop_test() ->
  ?assert(0=:=prisoner:calculateSentence({"defected", "cooperated"})).

%%tests to cover all the strategy choices and variations of them

titForTatChoiceStart_test() ->
  PID = spawn(prisoner, titForTat, ["TesterA",[{}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert((Choice =:= "cooperated") or (Choice =:= "defected")).

titForTatChoiceCoopCoop_test() ->
  PID = spawn(prisoner, titForTat, ["TesterA",[{"TesterB","cooperated","cooperated"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

titForTatChoiceCoopDefect_test() ->
  PID = spawn(prisoner, titForTat, ["TesterA",[{"TesterB","cooperated","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

titForTatChoiceDefCoop_test() ->
  PID = spawn(prisoner, titForTat, ["TesterA",[{"TesterB","defected","cooperated"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

titForTatChoiceDefDef_test() ->
  PID = spawn(prisoner, titForTat, ["TesterA",[{"TesterB","defected","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

suspiciousTitForTatChoiceStart_test() ->
  PID = spawn(prisoner, suspiciousTitForTat, ["TesterA",[{}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

suspiciousTitForTatChoiceCoopCoop_test() ->
  PID = spawn(prisoner, suspiciousTitForTat, ["TesterA",[{"TesterB","cooperated","cooperated"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

suspiciousTitForTatChoiceCoopDefect_test() ->
  PID = spawn(prisoner, suspiciousTitForTat, ["TesterA",[{"TesterB","cooperated","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

suspiciousTitForTatChoiceDefCoop_test() ->
  PID = spawn(prisoner, suspiciousTitForTat, ["TesterA",[{"TesterB","defected","cooperated"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

suspiciousTitForTatChoiceDefDef_test() ->
  PID = spawn(prisoner, suspiciousTitForTat, ["TesterA",[{"TesterB","defected","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

titForTatRandomChoiceStart_test() ->
  PID = spawn(prisoner, titForTatRandom, ["TesterA",[{}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert((Choice =:= "defected") or (Choice =:= "cooperated")).


titForTatRandomChoice_test() ->
  PID = spawn(prisoner, titForTatRandom, ["TesterA",[{"TesterB","defected","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert((Choice =:= "defected") or (Choice =:= "cooperated")).

randomChoiceStart_test() ->
  PID = spawn(prisoner, random, ["TesterA",[{}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert((Choice =:= "defected") or (Choice =:= "cooperated")).

randomChoice_test() ->
  PID = spawn(prisoner, random, ["TesterA",[{"TesterB","defected","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert((Choice =:= "defected") or (Choice =:= "cooperated")).

alwaysCoopChoiceStart_test() ->
  PID = spawn(prisoner, alwaysCoop, ["TesterA",[{}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

alwaysCoopChoiceCoopCoop_test() ->
PID = spawn(prisoner, alwaysCoop, ["TesterA",[{"TesterB","cooperated","cooperated"}]]),
Choice = prisoner:getChoice(PID, "TesterB"),
?assert(Choice =:= "cooperated").

alwaysCoopChoiceCoopDef_test() ->
  PID = spawn(prisoner, alwaysCoop, ["TesterA",[{"TesterB","cooperated","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

alwaysCoopChoiceDefCoop_test() ->
  PID = spawn(prisoner, alwaysCoop, ["TesterA",[{"TesterB","defected","cooperated"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

alwaysCoopChoiceDefDef_test() ->
  PID = spawn(prisoner, alwaysCoop, ["TesterA",[{"TesterB","defected","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "cooperated").

alwaysDefectChoiceStart_test() ->
  PID = spawn(prisoner, alwaysDefect, ["TesterA",[{}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

alwaysDefectChoiceCoopCoop_test() ->
  PID = spawn(prisoner, alwaysDefect, ["TesterA",[{"TesterB","cooperated","cooperated"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

alwaysDefectChoiceCoopDef_test() ->
  PID = spawn(prisoner, alwaysDefect, ["TesterA",[{"TesterB","cooperated","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

alwaysDefectChoiceDefCoop_test() ->
  PID = spawn(prisoner, alwaysDefect, ["TesterA",[{"TesterB","defected","cooperated"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").

alwaysDefectChoiceDefDef_test() ->
  PID = spawn(prisoner, alwaysDefect, ["TesterA",[{"TesterB","defected","defected"}]]),
  Choice = prisoner:getChoice(PID, "TesterB"),
  ?assert(Choice =:= "defected").


%%test to check getName() function returns the name of the prisoner
getName_test()->
  PID = spawn(prisoner, alwaysDefect, ["TesterA",[{"TesterB","defected","defected"}]]),
  ?assert("TesterA" =:= prisoner:getName(PID)).
