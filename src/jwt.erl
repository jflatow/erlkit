-module(jwt).
-author("Jared Flatow").

-export([encode/1,
         encode/2,
         decode/1,
         decode/2]).

-export([alg_jwt/1,
         alg_erl/1,
         sign/2,
         verify/3]).

encode(Payload) ->
    encode(Payload, []).

encode(Payload, Opts) ->
    Header = #{<<"alg">> => alg_jwt(util:get(Opts, alg))},
    H = base64url:encode(json:encode(Header)),
    P = base64url:encode(json:encode(Payload)),
    S = base64url:encode(sign({H, P}, Opts)),
    <<H/binary, ".", P/binary, ".", S/binary>>.

decode(Bin) ->
    decode(Bin, []).

decode(Bin, Opts) ->
    [H, P, S] = binary:split(Bin, <<".">>, [global]),
    Valid = verify({H, P}, base64url:decode(S), Opts),
    Header = json:decode(base64url:decode(H)),
    Payload = json:decode(base64url:decode(P)),
    {Valid, Header, Payload}.

alg_jwt({hmac,  sha256}) -> <<"HS256">>;
alg_jwt({hmac,  sha384}) -> <<"HS384">>;
alg_jwt({hmac,  sha512}) -> <<"HS512">>;
alg_jwt({rsa,   sha256}) -> <<"RS256">>;
alg_jwt({rsa,   sha384}) -> <<"RS384">>;
alg_jwt({rsa,   sha512}) -> <<"RS512">>;
alg_jwt({ecdsa, sha256}) -> <<"ES256">>;
alg_jwt({ecdsa, sha384}) -> <<"ES384">>;
alg_jwt({ecdsa, sha512}) -> <<"ES512">>;
alg_jwt(undefined) ->
    <<"none">>.

alg_erl(<<"HS256">>) -> {hmac,  sha256};
alg_erl(<<"HS384">>) -> {hmac,  sha384};
alg_erl(<<"HS512">>) -> {hmac,  sha512};
alg_erl(<<"RS256">>) -> {rsa,   sha256};
alg_erl(<<"RS384">>) -> {rsa,   sha384};
alg_erl(<<"RS512">>) -> {rsa,   sha512};
alg_erl(<<"ES256">>) -> {ecdsa, sha256};
alg_erl(<<"ES384">>) -> {ecdsa, sha384};
alg_erl(<<"ES512">>) -> {ecdsa, sha512};
alg_erl(<<"none">>) ->
    undefined.

sign({H, P}, Opts) ->
    sign(<<H/binary, ".", P/binary>>, Opts);
sign(Data, Opts) ->
    cipher:sign(util:get(Opts, alg), util:get(Opts, key), Data).

verify({H, P}, Signature, Opts) ->
    verify(<<H/binary, ".", P/binary>>, Signature, Opts);
verify(Data, Signature, Opts) ->
    cipher:verify(util:get(Opts, alg), util:get(Opts, key), Data, Signature).
