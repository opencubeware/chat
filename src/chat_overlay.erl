-module(chat_overlay).

-export([set_owner/0,
         add_logo/0,
         add_logo/4,
         delete_segment/1]).

-on_load(on_load/0).

on_load() ->
    LibFile = filename:join(["priv", "lib", ?MODULE]),
    erlang:load_nif(LibFile, 0).

set_owner() ->
    error(nif_not_loaded).

add_logo() ->
    LogoFile = filename:join(["priv", "assets", "logo_720.png"]),
    add_logo(LogoFile, 1125, 35, 0.4).

add_logo(_File, _X, _Y, _Alpha) ->
    error(nif_not_loaded).

delete_segment(_Id) ->
    error(nif_not_loaded).
