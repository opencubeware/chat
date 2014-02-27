-module(chat_overlay).

-export([new/0,
         new/2,
         add_logo/0,
         add_logo/4,
         save/1]).

-on_load(on_load/0).

on_load() ->
    LibFile = filename:join(["priv", "lib", ?MODULE]),
    erlang:load_nif(LibFile, 0).

new() ->
    new(1280, 720).

new(_Width, _Height) ->
    error(nif_not_loaded).

add_logo() ->
    LogoFile = filename:join(["priv", "assets", "logo_720.png"]),
    add_logo(LogoFile, 1125, 35, 0.5).

add_logo(_File, _X, _Y, _Alpha) ->
    error(nif_not_loaded).

save(_File) ->
    error(nif_not_loaded).
