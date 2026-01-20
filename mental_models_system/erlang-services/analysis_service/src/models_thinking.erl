%%%-------------------------------------------------------------------
%%% @doc Mental Models - Thinking Tools Category
%%% @end
%%%-------------------------------------------------------------------
-module(models_thinking).
-export([get_models/0]).

get_models() -> [
    #{id => <<"inversion">>, name => <<"Inversion">>,
      category => <<"Thinking Tools">>,
      description => <<"Think backward from failure to avoid it.">>,
      key_insight => <<"Many problems are best solved by inverting them">>,
      application => <<"Pre-mortem analysis, risk assessment">>,
      failure_modes => [<<"Paralysis by analysis">>, <<"Excessive pessimism">>],
      keywords => [<<"invert">>, <<"backward">>, <<"failure">>]},

    #{id => <<"circle_of_competence">>, name => <<"Circle of Competence">>,
      category => <<"Thinking Tools">>,
      description => <<"Know the boundaries of your expertise.">>,
      key_insight => <<"Knowing what you don't know is more valuable">>,
      application => <<"Investment decisions, career choices">>,
      failure_modes => [<<"Overconfidence">>, <<"Missed opportunities">>],
      keywords => [<<"competence">>, <<"expertise">>, <<"boundaries">>]},

    #{id => <<"first_principles">>, name => <<"First Principles Thinking">>,
      category => <<"Thinking Tools">>,
      description => <<"Break down problems into fundamental truths.">>,
      key_insight => <<"Most constraints are assumed, not real">>,
      application => <<"Innovation, problem-solving">>,
      failure_modes => [<<"Reinventing the wheel">>, <<"Analysis paralysis">>],
      keywords => [<<"fundamental">>, <<"basic">>, <<"reasoning">>]},

    #{id => <<"second_order_thinking">>, name => <<"Second-Order Thinking">>,
      category => <<"Thinking Tools">>,
      description => <<"Consider the consequences of consequences.">>,
      key_insight => <<"First-level thinking is simplistic">>,
      application => <<"Policy decisions, strategy">>,
      failure_modes => [<<"Overthinking">>, <<"Delayed action">>],
      keywords => [<<"consequences">>, <<"effects">>, <<"long-term">>]},

    #{id => <<"occams_razor">>, name => <<"Occam's Razor">>,
      category => <<"Thinking Tools">>,
      description => <<"The simplest explanation is usually correct.">>,
      key_insight => <<"Complexity should not be assumed without evidence">>,
      application => <<"Debugging, hypothesis formation">>,
      failure_modes => [<<"Oversimplification">>, <<"Missing nuance">>],
      keywords => [<<"simple">>, <<"explanation">>, <<"parsimony">>]},

    #{id => <<"hanlons_razor">>, name => <<"Hanlon's Razor">>,
      category => <<"Thinking Tools">>,
      description => <<"Never attribute to malice what can be explained by incompetence.">>,
      key_insight => <<"Most mistakes are honest errors">>,
      application => <<"Conflict resolution, team dynamics">>,
      failure_modes => [<<"Naivety">>, <<"Missing actual malice">>],
      keywords => [<<"malice">>, <<"stupidity">>, <<"mistake">>]},

    #{id => <<"probabilistic_thinking">>, name => <<"Probabilistic Thinking">>,
      category => <<"Thinking Tools">>,
      description => <<"Think in probabilities rather than certainties.">>,
      key_insight => <<"The future is a distribution of possibilities">>,
      application => <<"Risk assessment, forecasting">>,
      failure_modes => [<<"False precision">>, <<"Ignoring base rates">>],
      keywords => [<<"probability">>, <<"likelihood">>, <<"odds">>]},

    #{id => <<"thought_experiment">>, name => <<"Thought Experiment">>,
      category => <<"Thinking Tools">>,
      description => <<"Use imagination to explore hypothetical scenarios.">>,
      key_insight => <<"Mental simulation reveals hidden assumptions">>,
      application => <<"Philosophy, physics, ethics">>,
      failure_modes => [<<"Unrealistic assumptions">>, <<"Overthinking">>],
      keywords => [<<"imagine">>, <<"hypothetical">>, <<"scenario">>]},

    #{id => <<"map_territory">>, name => <<"Map is Not the Territory">>,
      category => <<"Thinking Tools">>,
      description => <<"Models are simplifications, not reality.">>,
      key_insight => <<"All models are wrong, some are useful">>,
      application => <<"Decision-making, planning">>,
      failure_modes => [<<"Confusing model with reality">>],
      keywords => [<<"model">>, <<"abstraction">>, <<"reality">>]}
].
