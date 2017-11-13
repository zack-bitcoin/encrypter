-module(sign).
-export([test/0,new_key/0,new_key/1,sign/2,verify_sig/3,shared_secret/2]).
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, de(Pub), de(Priv), params())).
new_key() -> 
    {Pub, Priv} = crypto:generate_key(ecdh, params()),
    %crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)) 
    {en(Pub), en(Priv)}.
new_key(P) ->
   {Pub, Priv} = crypto:generate_key(ecdh, params(), de(P)),
    {en(Pub), en(Priv)}.
    
sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, serialize(S), [Priv, params()])).
verify_sig(S, Sig, Pub) -> 
    SD = de(Sig),
    PD = Pub,
    crypto:verify(ecdsa, sha256, serialize(S), SD, [PD, params()]).
serialize(X) when is_binary(X) -> 
    S = size(X),
    <<0:8, S:32, X/binary>>;
serialize(L) when is_list(L) ->
    A = serialize_list(L),
    S = length(L),
    <<1:8, S:32, A/binary>>;
serialize(X) when is_tuple(X) -> 
    A = serialize_list(tuple_to_list(X)),
    S = size(X),
    <<2:8, S:32, A/binary>>;
serialize(X) when is_integer(X) -> 
    <<3:8, X:512>>;
serialize(X) when is_atom(X) -> 
    A = list_to_binary(atom_to_list(X)),
    S = size(A),
    <<4:8, S:32, A/binary>>;
serialize(X) -> 
    io:fwrite("testnet sign serialize error"),
    io:fwrite(packer:pack(X)),
    1=2.
serialize_list([]) -> <<>>;
serialize_list([A|B]) -> 
    C = serialize(A),
    D = serialize_list(B),
    <<C/binary, D/binary>>.

test() ->
    {Pub, Priv} = new_key(),
    {Pub2, Priv2} = new_key(),
    S = <<"abc">>,
    verify_sig(S, sign(S, Priv), Pub),
    SS = shared_secret(Pub, Priv2),
    SS = shared_secret(Pub2, Priv),
    success.
