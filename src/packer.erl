%We should add some rules about which atoms can be used with tuples. If peers can trick us into decoding new atoms, they can overflow erlang with too many atoms.
-module(packer).
-export([pack/1,unpack/1,test/0, untup/1, unpack_helper/1]).
-define(LIST_KEY, -6).
-define(TUPLE_KEY, -7).
untup(X) when is_tuple(X) and is_atom(element(1, X)) -> lists:map(fun(Z) ->untup(Z) end, tuple_to_list(X));
untup(X) when is_tuple(X) -> lists:map(fun(Z) ->untup(Z) end, [?TUPLE_KEY|tuple_to_list(X)]);
untup(X) when is_list(X) -> [?LIST_KEY|lists:map(fun(Z)->untup(Z) end,X)];
untup(X) when is_binary(X) -> base64:encode(X);
%untup(X) when is_atom(X) -> 
untup(X) -> X.
unpack(I) when is_integer(I) -> I;
unpack(JSON) -> unpack_helper(jiffy:decode(JSON)).
unpack_helper(J) when is_binary(J) -> base64:decode(J);
%unpack_helper(J) when is_binary(J) -> J;%bad
unpack_helper(J) when not is_list(J) -> J;
unpack_helper(J) when hd(J) == ?LIST_KEY -> 
    lists:map(fun(X) -> unpack_helper(X) end, tl(J));
unpack_helper(J) when hd(J) == ?TUPLE_KEY ->
    list_to_tuple(lists:map(fun(X) -> unpack_helper(X) end, tl(J)));
unpack_helper(J) -> 
    K = hd(J),
    B = is_b_atom(K),
    Out = if
	      %is_binary(K) -> binary_to_atom(K, latin1);
	      B -> 
		  binary_to_atom(K, latin1);
	      is_integer(K) -> K;
	      true -> 1=2
	  end,
    list_to_tuple([Out|lists:map(fun(X) -> unpack_helper(X) end, tl(J))]).
pack(X) -> iolist_to_binary(jiffy:encode(untup(X))).
-record(d, {a = "", b = "" }).
is_b_atom(<<"contract">>) -> true;
is_b_atom(<<"contracts">>) -> true;
is_b_atom(<<"accounts">>) -> true;
is_b_atom(<<"sub_accounts">>) -> true;
is_b_atom(<<"sub_channels">>) -> true;
is_b_atom(<<"trades">>) -> true;
is_b_atom(<<"new_contract_tx">>) -> true;
is_b_atom(<<"contract_new_tx">>) -> true;
is_b_atom(<<"contract_use_tx">>) -> true;
is_b_atom(<<"sub_spend_tx">>) -> true;
is_b_atom(<<"contract_evidence_tx">>) -> true;
is_b_atom(<<"contract_resolve_tx">>) -> true;
is_b_atom(<<"contract_timeout_tx">>) -> true;
is_b_atom(<<"contract_timeout_tx2">>) -> true;
is_b_atom(<<"contract_winnings_tx">>) -> true;
is_b_atom(<<"contract_simplify_tx">>) -> true;
is_b_atom(<<"pair_buy_tx">>) -> true;
is_b_atom(<<"pair_buy_offer">>) -> true;
is_b_atom(<<"team_buy_tx">>) -> true;
is_b_atom(<<"sortition_new_tx">>) -> true;
is_b_atom(<<"sortition_claim_tx">>) -> true;
is_b_atom(<<"sortition_waiver_tx">>) -> true;
is_b_atom(<<"sortition_contract_tx">>) -> true;
is_b_atom(<<"sortition_timeout_tx">>) -> true;
is_b_atom(<<"sortition_block_tx">>) -> true;
is_b_atom(<<"sortition_final_spend_tx">>) -> true;
is_b_atom(<<"rng_result_tx">>) -> true;
is_b_atom(<<"rng_challenge_tx">>) -> true;
is_b_atom(<<"rng_challenge_cleanup_tx">>) -> true;
is_b_atom(<<"rng_response_tx">>) -> true;
is_b_atom(<<"rng_refute_tx">>) -> true;
is_b_atom(<<"rng_confirm_tx">>) -> true;
is_b_atom(<<"final_spend">>) -> true;
is_b_atom(<<"x">>) -> true;
is_b_atom(<<"waiver">>) -> true;
is_b_atom(<<"owner">>) -> true;
is_b_atom(<<"owner_layer">>) -> true;
is_b_atom(<<"tree">>) -> true;
is_b_atom(<<"sid">>) -> true;
is_b_atom(<<"sid_before">>) -> true;
is_b_atom(<<"before">>) -> true;
is_b_atom(<<"priority">>) -> true;
is_b_atom(<<"priority_before">>) -> true;
is_b_atom(<<"add">>) -> true;
is_b_atom(<<"take">>) -> true;
is_b_atom(<<"tx_scan">>) -> true;
is_b_atom(<<"version">>) -> true;
is_b_atom(<<"get_offer_contract">>) -> true;
is_b_atom(<<"get_offers">>) -> true;
is_b_atom(<<"oracle_list">>) -> true;
is_b_atom(<<"nc_offer">>) -> true;
is_b_atom(<<"nc_accept">>) -> true;
is_b_atom(<<"channel_sig">>) -> true;
is_b_atom(<<"send">>) -> true;
is_b_atom(<<"read">>) -> true;
is_b_atom(<<"block_hash">>) -> true;
is_b_atom(<<"governance">>) -> true;
is_b_atom(<<"existence">>) -> true;
is_b_atom(<<"new_scalar_oracle">>) -> true;
is_b_atom(<<"scalar">>) -> true;
is_b_atom(<<"binary">>) -> true;
is_b_atom(<<"price">>) -> true;
is_b_atom(<<"lookup">>) -> true;
is_b_atom(<<"post">>) -> true;
is_b_atom(<<"ewah">>) -> true;
is_b_atom(<<"sync_normal">>) -> true;
is_b_atom(<<"sync_quick">>) -> true;
is_b_atom(<<"multi_tx">>) -> true;
is_b_atom(<<"time_value">>) -> true;
is_b_atom(<<"blocks">>) -> true;
is_b_atom(<<"work">>) -> true;
is_b_atom(<<"problem">>) -> true;
is_b_atom(<<"mining_data">>) -> true;
is_b_atom(<<"create_account_tx">>) -> true;
is_b_atom(<<"spend_tx">>) -> true;
is_b_atom(<<"combine_cancel_assets">>) -> true;
is_b_atom(<<"cancel_trade">>) -> true;
is_b_atom(<<"list_oracles">>) -> true;
is_b_atom(<<"keys_status">>) -> true;
is_b_atom(<<"keys_unlock">>) -> true;
is_b_atom(<<"keys_new">>) -> true;
is_b_atom(<<"halt">>) -> true;
is_b_atom(<<"ss">>) -> true;
is_b_atom(<<"exist">>) -> true;
is_b_atom(<<"key">>) -> true;
is_b_atom(<<"roots">>) -> true;
is_b_atom(<<"roots2">>) -> true;
is_b_atom(<<"roots3">>) -> true;
is_b_atom(<<"roots4">>) -> true;
is_b_atom(<<"roots5">>) -> true;
is_b_atom(<<"proof">>) -> true;
is_b_atom(<<"coinbase">>) -> true;
is_b_atom(<<"settle_bets">>) -> true;
is_b_atom(<<"market">>) -> true;
is_b_atom(<<"markets">>) -> true;
is_b_atom(<<"oracles">>) -> true;
is_b_atom(<<"market_data">>) -> true;
is_b_atom(<<"learn_secret">>) -> true;
is_b_atom(<<"give_block">>) -> true;
is_b_atom(<<"channel_sync">>) -> true;
is_b_atom(<<"locked_payment">>) -> true;
is_b_atom(<<"peers">>) -> true;
is_b_atom(<<"header">>) -> true;
is_b_atom(<<"headers">>) -> true;
is_b_atom(<<"channel_payment">>) -> true;
is_b_atom(<<"emsg">>) -> true;
is_b_atom(<<"new_channel">>) -> true;
is_b_atom(<<"txs">>) -> true;
is_b_atom(<<"ok">>) -> true;
is_b_atom(<<"pow">>) -> true;
is_b_atom(<<"prev_hashes">>) -> true;
is_b_atom(<<"error">>) -> true;
is_b_atom(<<"block">>) -> true;
is_b_atom(<<"block_plus">>) -> true;
is_b_atom(<<"ex">>) -> true;
is_b_atom(<<"timeout">>) -> true;
is_b_atom(<<"signed">>) -> true;
is_b_atom(<<"bet">>) -> true;
is_b_atom(<<"spk">>) -> true;
is_b_atom(<<"ctc">>) -> true;
is_b_atom(<<"ctc2">>) -> true;
is_b_atom(<<"gc">>) -> true;
is_b_atom(<<"delete_acc_tx">>) -> true;
is_b_atom(<<"cs">>) -> true;
is_b_atom(<<"nc">>) -> true;
is_b_atom(<<"cr">>) -> true;
is_b_atom(<<"spend">>) -> true;
is_b_atom(<<"create_acc_tx">>) -> true;
is_b_atom(<<"unmatched">>) -> true;
is_b_atom(<<"unmatched_head">>) -> true;
is_b_atom(<<"matched">>) -> true;
is_b_atom(<<"oracle_bets">>) -> true;
is_b_atom(<<"csc">>) -> true;
is_b_atom(<<"repo">>) -> true;
is_b_atom(<<"d">>) -> true;
is_b_atom(<<"channel">>) -> true;
is_b_atom(<<"gov">>) -> true;
is_b_atom(<<"oracle">>) -> true;
is_b_atom(<<"trees">>) -> true;
is_b_atom(<<"trees2">>) -> true;
is_b_atom(<<"trees3">>) -> true;
is_b_atom(<<"trees4">>) -> true;
is_b_atom(<<"trees5">>) -> true;
is_b_atom(<<"share">>) -> true;
is_b_atom(<<"acc">>) -> true;
is_b_atom(<<"sub_acc">>) -> true;
is_b_atom(<<"msg">>) -> true;
is_b_atom(<<"cd">>) -> true;
is_b_atom(<<"ob">>) -> true;
is_b_atom(<<"order">>) -> true;
is_b_atom(<<"orders">>) -> true;
is_b_atom(<<"r">>) -> true;
is_b_atom(<<"freq">>) -> true;
is_b_atom(<<"sync">>) -> true;
is_b_atom(<<"height">>) -> true;
is_b_atom(<<"off">>) -> true;
is_b_atom(<<"balance">>) -> true;
is_b_atom(<<"mempool">>) -> true;
is_b_atom(<<"top">>) -> true;
is_b_atom(<<"sign">>) -> true;
is_b_atom(<<"mine_block">>) -> true;
is_b_atom(<<"add_peer">>) -> true;
is_b_atom(<<"load_key">>) -> true;
is_b_atom(<<"create_account">>) -> true;
is_b_atom(<<"delete_account">>) -> true;
is_b_atom(<<"account">>) -> true;
is_b_atom(<<"sub_account">>) -> true;
is_b_atom(<<"repo_account">>) -> true;
is_b_atom(<<"channel_state">>) -> true;
is_b_atom(<<"channel_balance">>) -> true;
is_b_atom(<<"channel_balance2">>) -> true;
is_b_atom(<<"channel_timeout">>) -> true;
is_b_atom(<<"new_channel_with_server">>) -> true;
is_b_atom(<<"pull_channel_state">>) -> true;
is_b_atom(<<"add_secret">>) -> true;
is_b_atom(<<"channel_spend">>) -> true;
is_b_atom(<<"new_channel_tx">>) -> true;
is_b_atom(<<"close_channel_with_server">>) -> true;
is_b_atom(<<"grow_channel">>) -> true;
is_b_atom(<<"channel_solo_close">>) -> true;
is_b_atom(<<"channel_team_close">>) -> true;
is_b_atom(<<"channel_repo">>) -> true;
is_b_atom(<<"channel_slash">>) -> true;
is_b_atom(<<"channel_close">>) -> true;
is_b_atom(<<"lightning_spend">>) -> true;
is_b_atom(<<"new_difficulty_oracle">>) -> true;
is_b_atom(<<"new_question_oracle">>) -> true;
is_b_atom(<<"new_governance_oracle">>) -> true;
is_b_atom(<<"oracle_new">>) -> true;
is_b_atom(<<"oracle_bet">>) -> true;
is_b_atom(<<"oracle_close">>) -> true;
is_b_atom(<<"oracle_winnings">>) -> true;
is_b_atom(<<"oracle_unmatched">>) -> true;
is_b_atom(<<"pubkey">>) -> true;
is_b_atom(<<"new_pubkey">>) -> true;
is_b_atom(<<"channel_keys">>) -> true;
is_b_atom(<<"market_match">>) -> true;
is_b_atom(<<"new_market">>) -> true;
is_b_atom(<<"trade">>) -> true;
is_b_atom(<<"test_it_out">>) -> true;
is_b_atom(<<"dump_channels">>) -> true;
is_b_atom(<<"f">>) -> true;
is_b_atom(<<"test">>) -> true;
is_b_atom(<<"return">>) -> true;
is_b_atom(<<"checkpoint">>) -> true;
is_b_atom(<<"private">>) -> true;
is_b_atom(<<"read_private">>) -> true;
is_b_atom(<<"swap_tx">>) -> true;
is_b_atom(<<"swap_tx2">>) -> true;
is_b_atom(<<"swap_offer">>) -> true;
is_b_atom(<<"swap_offer2">>) -> true;
is_b_atom(<<"history">>) -> true;
is_b_atom(<<"market_new_tx">>) -> true;
is_b_atom(<<"market_liquidity_tx">>) -> true;
is_b_atom(<<"market_swap_tx">>) -> true;
is_b_atom(<<"trade_cancel_tx">>) -> true;
is_b_atom(<<"buy_shares">>) -> true;
is_b_atom(<<"combine_shares">>) -> true;
is_b_atom(<<"inflate">>) -> true;
is_b_atom(<<"nonce">>) -> true;
is_b_atom(<<"stablecoin_new_tx">>) -> true;
is_b_atom(<<"receipts">>) -> true;
is_b_atom(<<"close_oracles">>) -> true;
is_b_atom(<<"withdraw_from_oracles">>) -> true;
is_b_atom(<<"tx">>) -> true;
is_b_atom(<<"empty">>) -> true;
is_b_atom(<<"job">>) -> true;
is_b_atom(<<"jobs">>) -> true;
is_b_atom(<<"job_create_tx">>) -> true;
is_b_atom(<<"job_receive_salary_tx">>) -> true;
is_b_atom(<<"job_buy_tx">>) -> true;
is_b_atom(<<"job_adjust_tx">>) -> true;
is_b_atom(<<"job_team_adjust_tx">>) -> true;
is_b_atom(X) when is_binary(X) -> 
    io:fwrite("FAILED TO UNPACK ATOM "),
    io:fwrite(X),
    io:fwrite("\n"),
    false;
is_b_atom(_) -> false.
test() -> 
    Record = #d{a=[1, 2, <<"abc">>, [], #d{}], b = <<1,2,3,200, 0:80000>> },
    %ABC = {unlock, 24001,1,[{signed,{channel_block,0,3,-9500,3,[],24001,false,259,0,0,0],"TUVZQ0lRQzlwVkxjQ0hReXhpWE0zOU43bVFOS1pTV01WS0MxMkNUYjUwZSs4MkRnd3dJaEFPZG1lWlp0VXdjUXU0UjQzazhRWkREd29tb1BuQ05TWlhDSEl0QU5PemRj",[-6],[-6]],0]],
    %New = ["unlock2",24001,1,[["signed",["channel_block",0,3,-9500,3,[-6],24001,false,259,0,0,0],"TUVZQ0lRQzlwVkxjQ0hReXhpWE0zOU43bVFOS1pTV01WS0MxMkNUYjUwZSs4MkRnd3dJaEFPZG1lWlp0VXdjUXU0UjQzazhRWkREd29tb1BuQ05TWlhDSEl0QU5PemRj",[-6],[-6]],0]],
    List = [[],3,[4]],
    Int = 123,
    Int = unpack(pack(Int)),
    List = unpack(pack(List)),
    true = is_record(unpack(pack(Record)), d),
    Record = unpack(pack(Record)),
    true = is_binary(pack(Record)),
    Tuple = {{1,2},{<<>>}, [{<<1,2>>, <<>>}, 3, Record]},
    Tuple = unpack(pack(Tuple)),
    success.
