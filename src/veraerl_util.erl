-module(veraerl_util).

-export([hit_vera/3, discover/0, bootstrap/0]).

distill_suffix(S) ->
    distill_suffix(S, "").
distill_suffix([], R) ->
    R;
distill_suffix([{K, V}|T], R) ->
    distill_suffix(T, R ++ "&" ++ K ++ "=" ++ V).

hit_vera(Host, Suffix, Method) ->
    Url = "http://" ++ Host ++ ":3480/data_request?output_format=json" ++ distill_suffix(Suffix),
    case httpc:request(Method, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, <<"OK">>}} ->
            ok;
        {ok, {{_, 200, _}, _H, Body}} ->
            jsx:decode(Body)
    end.

distill_device(Device) when is_list(Device) ->
    F = fun(K) ->
                case proplists:get_value(K, Device) of
                    V when V /= undefined ->
                        V
                end
        end,
    [
     {ip, F(<<"InternalIP">>)},
     {mac, F(<<"MacAddress">>)},
     {id, F(<<"PK_Device">>)}
    ].

discover() ->
    Url = "https://vera-us-oem-authd.mios.com/locator/locator/locator",
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
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
