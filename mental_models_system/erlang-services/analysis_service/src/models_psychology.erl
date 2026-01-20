%%%-------------------------------------------------------------------
%%% @doc Mental Models - Psychology & Biases Category
%%% @end
%%%-------------------------------------------------------------------
-module(models_psychology).
-export([get_models/0]).

get_models() -> [
    #{id => <<"confirmation_bias">>, name => <<"Confirmation Bias">>,
      category => <<"Psychology">>,
      description => <<"Tendency to search for info that confirms beliefs.">>,
      key_insight => <<"We see what we want to see">>,
      application => <<"Research, decision-making">>,
      failure_modes => [<<"Echo chambers">>, <<"Poor decisions">>],
      keywords => [<<"bias">>, <<"confirm">>, <<"belief">>]},

    #{id => <<"loss_aversion">>, name => <<"Loss Aversion">>,
      category => <<"Psychology">>,
      description => <<"Losses feel twice as painful as equivalent gains.">>,
      key_insight => <<"Fear of loss outweighs desire for gain">>,
      application => <<"Negotiation, pricing">>,
      failure_modes => [<<"Holding losers too long">>],
      keywords => [<<"loss">>, <<"pain">>, <<"gain">>]},

    #{id => <<"availability_heuristic">>, name => <<"Availability Heuristic">>,
      category => <<"Psychology">>,
      description => <<"Judging probability by how easily examples come to mind.">>,
      key_insight => <<"Vivid events seem more likely than they are">>,
      application => <<"Risk assessment, media literacy">>,
      failure_modes => [<<"Overreacting to news">>],
      keywords => [<<"available">>, <<"memory">>, <<"vivid">>]},

    #{id => <<"anchoring">>, name => <<"Anchoring">>,
      category => <<"Psychology">>,
      description => <<"Over-relying on first piece of information.">>,
      key_insight => <<"First impressions create reference points">>,
      application => <<"Negotiation, pricing">>,
      failure_modes => [<<"Arbitrary anchors">>],
      keywords => [<<"anchor">>, <<"first">>, <<"reference">>]},

    #{id => <<"hindsight_bias">>, name => <<"Hindsight Bias">>,
      category => <<"Psychology">>,
      description => <<"Believing past events were predictable.">>,
      key_insight => <<"We overestimate our ability to have predicted">>,
      application => <<"Learning from mistakes">>,
      failure_modes => [<<"Overconfidence in predictions">>],
      keywords => [<<"hindsight">>, <<"predictable">>, <<"knew">>]},

    #{id => <<"sunk_cost">>, name => <<"Sunk Cost Fallacy">>,
      category => <<"Psychology">>,
      description => <<"Continuing due to past investment, not future value.">>,
      key_insight => <<"Past costs are irrelevant to future decisions">>,
      application => <<"Project management, investing">>,
      failure_modes => [<<"Throwing good money after bad">>],
      keywords => [<<"sunk">>, <<"cost">>, <<"investment">>]},

    #{id => <<"dunning_kruger">>, name => <<"Dunning-Kruger Effect">>,
      category => <<"Psychology">>,
      description => <<"Unskilled people overestimate their ability.">>,
      key_insight => <<"Incompetence prevents recognition of incompetence">>,
      application => <<"Self-assessment, hiring">>,
      failure_modes => [<<"Overconfidence">>, <<"Underestimating experts">>],
      keywords => [<<"overconfidence">>, <<"skill">>, <<"incompetence">>]},

    #{id => <<"social_proof">>, name => <<"Social Proof">>,
      category => <<"Psychology">>,
      description => <<"Following others' actions in uncertain situations.">>,
      key_insight => <<"We assume others know something we don't">>,
      application => <<"Marketing, leadership">>,
      failure_modes => [<<"Herd behavior">>, <<"Groupthink">>],
      keywords => [<<"social">>, <<"proof">>, <<"crowd">>]},

    #{id => <<"recency_bias">>, name => <<"Recency Bias">>,
      category => <<"Psychology">>,
      description => <<"Overweighting recent events in predictions.">>,
      key_insight => <<"Recent events feel more relevant">>,
      application => <<"Forecasting, investing">>,
      failure_modes => [<<"Ignoring long-term trends">>],
      keywords => [<<"recent">>, <<"memory">>, <<"trend">>]},

    #{id => <<"survivorship_bias">>, name => <<"Survivorship Bias">>,
      category => <<"Psychology">>,
      description => <<"Focusing on successes while ignoring failures.">>,
      key_insight => <<"We only see what survived">>,
      application => <<"Business strategy, research">>,
      failure_modes => [<<"False conclusions from incomplete data">>],
      keywords => [<<"survivor">>, <<"success">>, <<"failure">>]}
].
