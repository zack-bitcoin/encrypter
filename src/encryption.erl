
-module(encryption).
-export([test/0,bin_enc/2,bin_dec/2,send_msg/4,get_msg/2,msg/1,id/1,encrypt/2,decrypt/2]).
-record(msg, {sig = "", msg = "", id = 0}).
-record(emsg, {key = "", msg = ""}).
-define(iv, <<0:128>>).
msg(Msg) -> Msg#msg.msg.
id(Msg) -> Msg#msg.id.

%si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
%si(Key) -> crypto:stream_init(aes_ctr, crypto:hmac(sha256, "", Key), <<0:128>>).%because each key is used for encryption at most once, we can use a static initialization vector.
si(Key) -> 
    crypto:stream_init(aes_ctr, Key, <<0:128>>).%because each key is used for encryption at most once, we can use a static initialization vector.

%This version can be used exactly once with each private key.
bin_enc(Key, Bin) ->
    {_, X} = crypto:stream_encrypt(si(Key), Bin),
    X.
bin_dec(Key, Msg) ->
    {_, Y} = crypto:stream_decrypt(si(Key), Msg),
    Y.
%sym_enc(Key, Msg) -> bin_enc(Key, term_to_binary(Msg)).
sym_enc(Key, Msg) -> bin_enc(Key, packer:pack(Msg)).
%sym_dec(Key, Emsg) -> binary_to_term(bin_dec(Key, Emsg)).
sym_dec(Key, Emsg) -> 
    B = bin_dec(Key, Emsg),
    packer:unpack(bin_dec(Key, Emsg)).
%this version is good for a blockchain situation where you want to send a message to someone who's public key you know.
send_msg(M, ToPub, FromPub, FromPriv) -> 
    {EphPub, EphPriv} = sign:new_key(),
    Msg = #msg{sig=sign:sign(EphPub, base64:decode(FromPriv)), msg=M, id = FromPub},
    SS = sign:shared_secret(ToPub, EphPriv),
    Emsg = sym_enc(base64:decode(SS), Msg),
    #emsg{key=EphPub, msg=base64:encode(Emsg)}.
get_msg(Msg, Priv) ->
    SS = sign:shared_secret(Msg#emsg.key, Priv),
    Sig = sym_dec(base64:decode(SS), base64:decode(Msg#emsg.msg)),
    true = sign:verify_sig(Msg#emsg.key, Sig#msg.sig, base64:decode(Sig#msg.id)),%(data, signature, pubkey)
    Sig.
%This version can be used over and over to encrypt data with the same secret.
to_priv(X) ->
    base64:encode(crypto:hash(sha256, X)).
encrypt(File, Secret) ->
    Random = crypto:strong_rand_bytes(32),
    TempPriv = to_priv(Random),
    Priv = to_priv(Secret),
    {TempPub, TempPriv} = sign:new_key(TempPriv),
    {Pub, Priv} = sign:new_key(Priv),
    send_msg(File, Pub, TempPub, TempPriv).
decrypt(Encrypted, Secret) ->
    Priv = to_priv(Secret),
    (get_msg(Encrypted, Priv))#msg.msg.
    
test() ->
    Pub1_64 = <<"BNFsD42eXjwHd4PyP4lODu+aybYjmVJF0bA0UYNcJ2/cELBl5Z6IA639jUl1km+8YAD3aL3of+SqLI8emuhsa2c=">>,
    Priv1_64 = <<"wruxx99+2hK4cT+j1SkJhV6VzBxgbl11iHxnq6ghASA=">>,
    Pub2_64 = <<"BA7HtKYPIvUwQjmUqNQ0UMsxHu+KtISveg45Jl+tl/y6OMgtyC3rE4/YrEHLtTprCPsxcus5CbhmlWo9IDKfnzo=">>,
    Priv2_64 = <<"4a6E2IK3hhhP2dK8xGYaUqg23Fk/n/Ms2VuORKC5Xvo=">>,
    M = [1,2,3],
    SM = send_msg(M, Pub2_64, Pub1_64, Priv1_64),
%["emsg","QlBKTURaYTZHTEdBc2FuM2Y5c3pjR0tja29KblVoLyt3NE92c21kT0hDa3pEcjlESlRDVmhHTFlqNWdINnhSYmszSlFkbzdRZ2ttZHByQVlVbDBiZkpBPQ==","ejFEWmM0TDEyY1g3Q3F0Q0ZZK3NETHptTld4UFhTeFpKSVAwUk9BZkZqWFVLRGUwQkdDMGl2ZFQ2Rk9IT0ZtTHJPSGRwbit0bWpKYzNjYzlKdEFLQW5aY3kxRVBmekNTSTRONWN4RDhFbzg3dEdWSUwzKytSbDdaZ1JWZE5STUp5MEhrUDJIZmVmSWZaQjV2VW9YTWhuYytKSVB3M0hmVFBlbjFUM29qdmxsanNBM3l6OC8vNGU5eWpKeDIwV0pHMnBFV3BmWEJYZDVJZklmeG53QWJTUGNhMTRGNE8rN1hYRjA0bks2U0ZQckZkYzgrUGxFZUZqbmxKRG9YOTMwenE3MHcrMDZjeElMV096RDE2bCtlSldZbzg5OGhSWUxHUlJvRTFGSE5XcjV3WDBCeWNjL3creWhCbDl3dkpsckM="]
    io:fwrite(packer:pack(SM)),
    test2().
test2() ->
    {Pub, Priv} = sign:new_key(),
    {Pub2, Priv2} = sign:new_key(),
    Val = <<"1234">>,
    Binary = <<2,3,4>>,
    Secret = <<1,2,3,4,5,6,7,8,
              1,2,3,4,5,6,7,8,
              1,2,3,4,5,6,7,8,
              1,2,3,4,5,6,7,8>>,
    true = bin_dec(Secret, bin_enc(Secret, Val)) == Val,
    true = bin_dec(Secret, bin_enc(Secret, Binary)) == Binary,
    Record = {f, Binary},
    SM = send_msg(Record, Pub2, Pub, Priv),
    %io:fwrite(SM),
                            %{emsg,<<"BI+wqb4WGYa2NQkQ3LtgGyT/9JjZjCQmlJkWZPtgNy6YeQKieeDVWD5EVmivPUvBdCvCFrZngA50RbBqqbS6Obs=">>,
                            %      <<"88sF3lodacpZkcVrZUsMtXPCUxzMD8Pss66d0KebKHJVDK5eeTgJV9CZCfkHVoOkBmZI2oz3fjjo1Z9aMs7+FN6bGaCrVocB"...>>},
    %io:fwrite(Priv2), %gFEvbHycbSEnigYBtqhQaBOQ+1mQvWlpApvzZLpJ0AI=
    io:fwrite("\n"),
    %io:fwrite(SM),
    io:fwrite("\n"),
    io:fwrite(integer_to_list(size(element(3, SM)))), %324
    io:fwrite("\n"),
    Sig = get_msg(send_msg(Record, Pub2, Pub, Priv), Priv2),
    true = Sig#msg.msg == Record,
    Secret2 = <<1,5,3,7,4>>,
    Record = decrypt(encrypt(Record, Secret2), Secret2), 
    
    success.
