-module(cipher).
-author("Jared Flatow").

-export([sign/3,
         verify/4]).

sign({hmac, Type}, Key, Data) ->
    crypto:hmac(Type, Key, Data);
sign({rsa, Type}, Key, Data) when is_list(Key) ->
    crypto:sign(rsa, Type, Data, Key);
sign({ecdsa, Type}, Key, Data) when is_list(Key) ->
    crypto:sign(ecdsa, Type, Data, Key);
sign({_, Type}, Key, Data) when is_tuple(Key) ->
    public_key:sign(Data, Type, Key);
sign(undefined, undefined, _) ->
    <<>>.

verify({hmac, Type}, Key, Data, Signature) ->
    crypto:hmac(Type, Key, Data) =:= Signature;
verify({rsa, Type}, Key, Data, Signature) when is_list(Key) ->
    crypto:verify(rsa, Type, Data, Signature, Key);
verify({ecdsa, Type}, Key, Data, Signature) when is_list(Key) ->
    crypto:verify(ecdsa, Type, Data, Signature, Key);
verify({_, Type}, Key, Data, Signature) when is_tuple(Key) ->
    public_key:verify(Data, Type, Signature, Key);
verify(undefined, undefined, _, <<>>) ->
    true;
verify(undefined, undefined, _, _) ->
    false.
