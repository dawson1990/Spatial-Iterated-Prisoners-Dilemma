%%%-------------------------------------------------------------------
%%% @author Kevin Dawson
%%% @copyright (C) 2016,
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

%returns name of inmate
getName(PrisonerPID) ->
  PrisonerPID!{self(), name},
  receive
    {PrisonerPID,name, Name} ->
      Name
  end.

%returns the strategy name that the inmate chose
getStrategy(PrisonerPID) ->
  PrisonerPID!{self(), strategy},
  receive
    {PrisonerPID,strategy, Strategy } ->
      Strategy
  end.

%returns State which is a list of tuples contains name of other prisoner, their choice and inmates choice
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
      %filters the list to find if inmate has dealt with other inmate before
      HasDealtWith = lists:filter(
        fun(Turn) ->
          case Turn of
            {PartnerName,_,_} ->
              true;
            _ ->
              false
          end
        end,State),
      %choice is chosen based on other inmates last choice
      MyChoice = case HasDealtWith of
                   [{_,"defected",_}|_] ->
                     "defected";
                   _->
                      "cooperated"
                 end,
      Sender!{self(),choice,MyChoice},
      receive
        {Sender, result, TheirChoice} ->
          %sends a tuple with inmates choice and other inmates choice and returns the sentence they will serve
          MySentence = calculateSentence({MyChoice, TheirChoice}),
          %sends their score to warden
          Sender!{self(), result, MySentence},
          %updates state
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
      %similar to titForTat only it starts with defected
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
      %random number between 1 and 100 is put into 'PercentageChance'
      PercentChance = rand:uniform(100),
      %if PercentChance is less than 90% titForTat strategy is used
      if PercentChance < 90 ->
          MyChoice = case HasDealtWith of
            [{_,"defected",_}|_] ->
              "defected";
            _->
              "cooperated"
          end;
        %else if its above 90% its random strategy
        true ->
          %list containing two choices
          ChoicesList = ["cooperated","defected"],
          Choice = rand:uniform(2), %randomly pick 1 or 2
          MyChoice = lists:nth(Choice, ChoicesList) %inmates choice is the nth('Choice') element in 'ChoicesList
        end,
      %sends choice to Warden
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
      %choice is always defected
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
      %list containing the two choice
      ChoicesList = ["cooperated","defected"],
      %random number between 1 and 2
      Choice = rand:uniform(2),
      %MyChoice is gotten from getting the value at position in 'Choice' from list 'ChoicesList'
      MyChoice = lists:nth(Choice, ChoicesList),
      Sender!{self(),choice,MyChoice},
      receive
        {Sender,result, TheirChoice}->
          %sends a tuple with inmates choice and other inmates choice and returns the sentence they will serve
          MySentence = calculateSentence({MyChoice, TheirChoice}),
          Sender!{self(), result, MySentence},
          random(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  random(Name,State).


%%function to calculate score, takes in a tuple containing first inmate and next inmates strategy choice and returns their sentence
calculateSentence({MyChoice, TheirChoice}) ->
  %takes the tuple from parameter and pattern matches them with cases
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






