-module(veraerl_util).
-export([local_vera/2, remote_vera/4, discover/0, discover/1, bootstrap/0]).
-export([login/2, get_session/3, get_device/3]).
-include("veraerl.hrl").

distill_suffix(S) ->
    distill_suffix(S, "").
distill_suffix([], R) ->
    R;
distill_suffix([{K, V}|T], R) ->
    distill_suffix(T, R ++ "&" ++ K ++ "=" ++ V).

remote_vera(Host, Suffix, Session, PKID) ->
    Url = "https://" ++ Host ++ "/relay/relay/relay/device/" ++ integer_to_list(PKID) ++ "/port_3480/data_request?output_format=json" ++ distill_suffix(Suffix),
    case httpc:request(get, {Url, [{"MMSSession", Session}]}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, <<"OK">>}} ->
            ok;
        {ok, {{_, 200, _}, _H, Body}} ->
            jsx:decode(Body)
    end.

local_vera(Host, Suffix) ->
    Url = "http://" ++ Host ++ ":3480/data_request?output_format=json" ++ distill_suffix(Suffix),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, <<"OK">>}} ->
            ok;
        {ok, {{_, 200, _}, _H, Body}} ->
            jsx:decode(Body)
    end.

distill_device(Device) when is_list(Device) ->
    F = fun(K) ->
                case proplists:get_value(K, Device) of
                    V when is_binary(V) ->
                        binary_to_list(V);
                    V when V /= undefined ->
                        V
                end
        end,
    [
     {ip, F(<<"InternalIP">>)},
     {mac, F(<<"MacAddress">>)},
     {id, F(<<"PK_Device">>)},
     {servers, [
                {device, F(<<"Server_Device">>)},
                {account, F(<<"Server_Account">>)}
               ]}
    ].

discover() ->
    discover(undefined).
discover(AccountSession) ->
    Url = "https://vera-us-oem-authd.mios.com/locator/locator/locator",
    Headers = if AccountSession == undefined ->
                      [];
                 true ->
                      [{"MMSSession", AccountSession}]
              end,
    case httpc:request(get, {Url, Headers}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, Body}} ->
            case jsx:decode(Body) of
                JSON when is_list(JSON) ->
                    case proplists:get_value(<<"Devices">>, JSON) of
                        Devices when is_list(Devices) ->
                            lists:map(fun distill_device/1, Devices)
                    end
            end
    end.

bootstrap() ->
    application:load(magicbeam),
    magicbeam_util:start_deps(),
    application:start(magicbeam),
    application:load(veraerl),
    {ok, Deps} = application:get_key(veraerl, applications),
    start_deps(Deps),
    application:start(veraerl).

start_deps([]) ->
    ok;
start_deps([App|Deps]) ->
    application:load(App),
    case application:get_key(App, applications) of
        {ok, AppDeps} ->
            start_deps(AppDeps);
        undefined ->
            ok
    end,
    case application:start(App) of
        ok -> start_deps(Deps);
        {error,{already_started,App}} -> start_deps(Deps)
    end.

hash_pass(User, Password) ->
    string:to_lower([ element(C+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$A,$B,$C,$D,$E,$F}) || <<C:4>> <= crypto:hash(sha, User ++ Password ++ ?MIOS_SEED)]).

get_auth(User, Password) when is_binary(User) ->
    get_auth(binary_to_list(User), Password);
get_auth(User, Password) when is_binary(Password) ->
    get_auth(User, binary_to_list(Password));
get_auth(User, Password) when is_list(User), is_list(Password) ->
    Url = ?MIOS_AUTH_SERVER ++ "autha/auth/username/" ++ User ++ "?SHA1Password=" ++ hash_pass(User, Password) ++ "&PK_Oem=1",
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, Body}} ->
            case jsx:decode(Body) of
                PL when is_list(PL) ->
                    F = fun(K) ->
                                binary_to_list(proplists:get_value(K, PL))
                        end,
                    {ok, F(<<"Identity">>), F(<<"IdentitySignature">>), F(<<"Server_Account">>)}
            end
    end.

get_session(Server, AuthToken, AuthSig) ->
    Url = "https://" ++ Server ++ "/info/session/token",
    Headers = [
               {"MMSAuth", AuthToken},
               {"MMSAuthSig", AuthSig}
              ],
    case httpc:request(get, {Url, Headers}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, Body}} when is_binary(Body) ->
            {ok, binary_to_list(Body)}
    end.

login(User, Password) ->
    case get_auth(User, Password) of
        {ok, AuthToken, AuthSig, Server} ->
            case get_session(Server, AuthToken, AuthSig) of
                {ok, Session} ->
                    {ok, AuthToken, AuthSig, Session}
            end
    end.

get_device(Server, AccountSession, PKID) ->
    Url = "https://" ++ Server ++ "/device/device/device/" ++ integer_to_list(PKID),
    case httpc:request(get, {Url, [{"MMSSession", AccountSession}]}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, Body}} ->
            case jsx:decode(Body) of
                JSON when is_list(JSON) ->
                    F = fun(K) ->
                                proplists:get_value(list_to_binary(K), JSON)
                        end,
                    [
                     {servers, [
                                {relay, F("Server_Relay")}
                               ]
                     }
                    ]
            end
    end.
