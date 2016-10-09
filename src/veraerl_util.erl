-module(veraerl_util).
-export([local_vera/2, remote_vera/4, discover/0, discover/3, bootstrap/0]).
-export([login/2, get_session/3, get_device/3, decode_token/1, device_session/4, relay_session/5]).
-include("veraerl.hrl").

distill_suffix(S) ->
    distill_suffix(S, "").
distill_suffix([], R) ->
    R;
distill_suffix([{K, V}|T], R) ->
    distill_suffix(T, R ++ "&" ++ K ++ "=" ++ V).

vera_request(Method, Request) ->
    case httpc:request(Method, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, <<"OK">>}} ->
            ok;
        {ok, {{_, 200, _}, _H, Body}} ->
            {ok, Body};
        {ok, {{_, 401, _}, _H, _Body}} ->
            {error, auth};
        {error,socket_closed_remotely} ->
            {error, network};
        {error, {failed_connect, _E}} ->
            {error, network};
        {error, {{_, 500, _}, _H, <<"ERROR:Tunnel not found">>}} ->
            {error, tunnel}
    end.

remote_vera(Host, Suffix, Session, PKID) ->
    Url = "https://" ++ Host ++ "/relay/relay/relay/device/" ++ integer_to_list(PKID) ++ "/port_3480/data_request?output_format=json" ++ distill_suffix(Suffix),
    case vera_request(get, {Url, [{"MMSSession", Session}]}) of
        ok ->
            ok;
        {ok, Body} when is_binary(Body) ->
            jsx:decode(Body);
        {error, _} = E ->
            E
    end.

local_vera(Host, Suffix) ->
    Url = "http://" ++ Host ++ ":3480/data_request?output_format=json" ++ distill_suffix(Suffix),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, <<"OK">>}} ->
            ok;
        {ok, {{_, 200, _}, _H, Body}} ->
            jsx:decode(Body);
        {error,socket_closed_remotely} ->
            {error, network};
        {error, {failed_connect, _E}} ->
            {error, network};
        {error, {tcp_error, _, _E}} ->
            {error, network}
    end.

distill_device(Device) when is_list(Device) ->
    F = fun(K) ->
                case proplists:get_value(K, Device) of
                    V when is_binary(V) ->
                        binary_to_list(V);
                    V ->
                        V
                end
        end,
    FF = fun
             ({_, undefined}) -> false;
             (_) -> true
         end,
    lists:filter(FF, [
     {ip, F(<<"InternalIP">>)},
     {mac, F(<<"MacAddress">>)},
     {id, case F(<<"PK_Device">>) of
              I when is_integer(I) -> I;
              L when is_list(L) -> list_to_integer(L)
          end},
     {servers, lists:filter(FF, [
                {device, F(<<"Server_Device">>)},
                {account, F(<<"Server_Account">>)}
               ])}
    ]).

discover() ->
    discover("https://vera-us-oem-authd.mios.com/locator/locator/locator",undefined).
discover(Server, PK_Account, AccountSession) when is_list(Server), is_integer(PK_Account), is_list(AccountSession) ->
    discover("https://" ++ Server ++ "/account/account/account/" ++ integer_to_list(PK_Account) ++ "/devices", AccountSession).

discover(Url, AccountSession) ->
    Headers = if AccountSession == undefined ->
                      [];
                 true ->
                      [{"MMSSession", AccountSession}]
              end,
    case vera_request(get, {Url, Headers}) of
        {ok, Body} when is_binary(Body) ->
            case jsx:decode(Body) of
                JSON when is_list(JSON) ->
                    case proplists:get_value(<<"Devices">>, JSON) of
                        Devices when is_list(Devices) ->
                            lists:map(fun distill_device/1, Devices)
                    end
            end;
        {error, _} = E ->
            E
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
    case vera_request(get, {Url, []}) of
        {ok, Body} when is_binary(Body) ->
            case jsx:decode(Body) of
                PL when is_list(PL) ->
                    F = fun(K) ->
                                binary_to_list(proplists:get_value(K, PL))
                        end,
                    {ok, F(<<"Identity">>), F(<<"IdentitySignature">>), F(<<"Server_Account">>)}
            end;
        {error, _} = E ->
            E
    end.

get_session(Server, AuthToken, AuthSig) ->
    Url = "https://" ++ Server ++ "/info/session/token",
    Headers = [
               {"MMSAuth", AuthToken},
               {"MMSAuthSig", AuthSig}
              ],
    case vera_request(get, {Url, Headers}) of
        {ok, Body} when is_binary(Body) ->
            {ok, binary_to_list(Body)};
        {error, _} = E ->
            E
    end.

login(User, Password) ->
    case get_auth(User, Password) of
        {ok, AuthToken, AuthSig, AuthServer} ->
            case get_session(AuthServer, AuthToken, AuthSig) of
                {ok, Session} ->
                    {ok, AuthToken, AuthSig, Session, AuthServer};
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.

get_device(Server, AccountSession, PKID) ->
    Url = "https://" ++ Server ++ "/device/device/device/" ++ integer_to_list(PKID),
    case vera_request(get, {Url, [{"MMSSession", AccountSession}]}) of
        {ok, Body} when is_binary(Body) ->
            case jsx:decode(Body) of
                JSON when is_list(JSON) ->
                    F = fun(K) ->
                                binary_to_list(proplists:get_value(list_to_binary(K), JSON))
                        end,
                    [
                     {servers, [
                                {relay, F("Server_Relay")}
                               ]
                     }
                    ]
            end;
        {error, _} = E ->
            E
    end.

decode_token(AuthToken) ->
    case jsx:decode(base64:decode(AuthToken)) of
        PL when is_list(PL) ->
            F = fun(K) ->
                        proplists:get_value(K, PL)
                end,
            {ok, binary_to_list(F(<<"Server_Auth">>)), F(<<"PK_Account">>)}
    end.

relay_session(Device, AT, AS, DServer, DSession) ->
    case veraerl_util:get_device(DServer, DSession, Device) of
        PL when is_list(PL) ->
            RServer = proplists:get_value(relay, proplists:get_value(servers, PL)),
            case veraerl_util:get_session(RServer, AT, AS) of
                {ok, RSession} ->
                    {ok, RServer, RSession};
                {error, _} = E ->
                    E
            end
    end.

device_session(Device, AT, AS, [DObj|T]) ->
    F = fun(K) ->
                proplists:get_value(K, DObj)
        end,
    case F(id) of
        Device ->
            DServer = proplists:get_value(device, F(servers)),
            case veraerl_util:get_session(DServer, AT, AS) of
                {ok, DSession} ->
                    {ok, DServer, DSession};
                {error, _} = E ->
                    E
            end;
        OtherDevice when is_integer(OtherDevice) ->
            device_session(Device, AT, AS, T);
        undefined ->
            device_session(Device, AT, AS, T)
    end.
