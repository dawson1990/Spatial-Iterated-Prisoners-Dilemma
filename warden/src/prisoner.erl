%%%-------------------------------------------------------------------
%%% @author Kevin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2016 16:41
%%%-------------------------------------------------------------------
-module(prisoner).
-author("Kevin").

%% API
-export([create/3, getName/1, getStrategy/1, getState/1,alwaysCoop/2,alwaysDefect/2,titForTat/2,suspiciousTitForTat/2,random/2,titForTatRandom/2, calculateSentence/1]).
-include_lib("eunit/include/eunit.hrl").


create(Name, Strategy, State) ->
  PID = spawn(?MODULE, Strategy, [Name,State]),
  PID.

getName(PrisonerPID) ->
  PrisonerPID!{self(), name},
  receive
    {PrisonerPID,name, Name} ->
      Name
  end.


getStrategy(PrisonerPID) ->
  PrisonerPID!{self(), strategy},
  receive
    {PrisonerPID,strategy, Strategy } ->
      Strategy
  end.

getState(PrisonerPID) ->
  PrisonerPID!{self(), state},
  receive
    {PrisonerPID,state,State} ->
      State
  end.


titForTat(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,titForTat};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      HasDealtWith = lists:filter(
        fun(Turn) ->
          case Turn of
            {PartnerName,_,_} ->
              true;
            _ ->
              false
          end
        end,State),
      MyChoice = case HasDealtWith of
                   [{_,"defected",_}|_] ->
                     "defected";
                   _->
                      "cooperated"
                 end,
      Sender!{self(),choice,MyChoice},
      receive
        {Sender, result, TheirChoice} ->
          MySentence = calculateSentence({MyChoice, TheirChoice}),
          Sender!{self(), result, MySentence},
          titForTat(Name, [{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  titForTat(Name,State).

suspiciousTitForTat(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,titForTat};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      HasDealtWith = lists:filter(
        fun(Turn) ->
          case Turn of
            {PartnerName,_,_} ->
              true;
            _ ->
              false
          end
        end,State),
      MyChoice = case HasDealtWith of
                     [{_,"cooperated",_}|_] ->
                       "cooperated";
                     _->
                       "defected"
                 end,
      Sender!{self(),choice,MyChoice},
      receive
        {Sender, result, TheirChoice} ->
          MySentence = calculateSentence({MyChoice, TheirChoice}),
          Sender!{self(), result, MySentence},
          titForTat(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  titForTat(Name,State).

titForTatRandom(Name, State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,titForTat};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      HasDealtWith = lists:filter(
        fun(Turn) ->
          case Turn of
            {PartnerName,_,_} ->
              true;
            _ ->
              false
          end
        end,State),
      PercentChance = rand:uniform(100), %% picks a random number between 1 and 100
      if PercentChance < 90 ->
          MyChoice = case HasDealtWith of
            [{_,"defected",_}|_] ->
              "defected";
            _->
              "cooperated"
          end;
        true ->
          ChoicesList = ["cooperated","defected"],
          Choice = rand:uniform(2),
          MyChoice = lists:nth(Choice, ChoicesList)
        end,
      Sender!{self(),choice,MyChoice},
      receive
        {Sender, result, TheirChoice} ->
          MySentence = calculateSentence({MyChoice, TheirChoice}),
          Sender!{self(), result, MySentence},
          titForTatRandom(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  titForTatRandom(Name,State).

alwaysDefect(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,alwaysDefect};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      MyChoice = "defected",
      Sender!{self(),choice,MyChoice},
      receive
        {Sender,result,TheirChoice} ->
          MySentence = if
                      TheirChoice == "cooperated" ->
                        0;
                      TheirChoice == "defected" ->
                        2
                    end,
          Sender!{self(), result, MySentence},
          alwaysDefect(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  alwaysDefect(Name,State).

alwaysCoop(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,alwaysCoop};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      MyChoice = "cooperated",
      Sender!{self(),choice,MyChoice},
      receive
        {Sender,result, TheirChoice}->
          MySentence = if
                      TheirChoice == "cooperated" ->
                        1;
                      TheirChoice == "defected" ->
                        3
                    end,
          Sender!{self(), result, MySentence},
          alwaysCoop(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  alwaysCoop(Name,State).

random(Name, State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,alwaysCoop};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      ChoicesList = ["cooperated","defected"],
      Choice = rand:uniform(2),
      MyChoice = lists:nth(Choice, ChoicesList),
      Sender!{self(),choice,MyChoice},
      receive
        {Sender,result, TheirChoice}->
          MySentence = calculateSentence({MyChoice, TheirChoice}),
          Sender!{self(), result, MySentence},
          random(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  random(Name,State).


%%function to calculate score, takes in a tuple containing first inmate and next inmates strategy choice and returns their sentence
calculateSentence({MyChoice, TheirChoice}) ->
 case {MyChoice, TheirChoice}  of
   {MyChoice, TheirChoice} when MyChoice == "cooperated", TheirChoice == "cooperated"  ->
     1;
   {MyChoice, TheirChoice} when MyChoice == "cooperated", TheirChoice == "defected"  ->
     3;
   {MyChoice, TheirChoice}when MyChoice == "defected", TheirChoice == "defected"  ->
     2;
   {MyChoice, TheirChoice} when MyChoice == "defected", TheirChoice == "cooperated"  ->
     0
 end.






