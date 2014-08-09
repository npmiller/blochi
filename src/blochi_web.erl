%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for blochi.

-module(blochi_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/4]).

%% External API

start(Options) ->
	{[DocRoot, CacheRoot, StaticRoot], Options1} = get_options([docroot, cacheroot, staticroot], Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot, StaticRoot, CacheRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot, StaticRoot, CacheRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
				case string:tokens(Path, "/") of
                    ["static" | EndPath] ->
						Req:serve_file(EndPath, StaticRoot);
					["raw" | EndPath] ->
						Req:serve_file(EndPath, DocRoot);
                    [] ->
                        serve_md(Req, [], DocRoot, CacheRoot);
                    Tokens ->
                        serve_md(Req, filename:join(Tokens), DocRoot, CacheRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_options(Opt_list, Options) ->
	Props = lists:map(fun(O) -> proplists:get_value(O, Options) end, Opt_list),
	List = lists:foldl(fun(O, Acc) -> proplists:delete(O, Acc) end, Options, Opt_list),
	{Props, List}.

serve_md(Req, Path, DocRoot, CacheRoot) ->
	case mochiweb_util:safe_relative_path(Path) of
		undefined -> 
			Req:not_found();
		RelPath ->
			case filelib:is_dir(filename:join([DocRoot, RelPath])) of
				true ->
					serve_md_file(Req, DocRoot, CacheRoot, [RelPath, "index"]);
				false ->
					serve_md_file(Req, DocRoot, CacheRoot, RelPath)
			end
	end.

serve_md_file(Req, DocRoot, CacheRoot, RelPath) ->
	SourcePath = filename:join([DocRoot, [RelPath, ".md"]]),
	CachePath = filename:join([CacheRoot, [RelPath, ".html"]]),
	TemplatePath = find_template(DocRoot, RelPath),
	case check_cache(SourcePath, CachePath, TemplatePath) of
		error ->
			Req:not_found();
		render ->
			render_md(SourcePath, CachePath, TemplatePath),
			Req:serve_file([RelPath, ".html"], CacheRoot);
		render_tpl ->
			render_tpl(CachePath, TemplatePath),
			Req:serve_file([RelPath, ".html"], CacheRoot);
		ok -> 
			Req:serve_file([RelPath, ".html"], CacheRoot)
	end.

render_md(SrcPath, CachePath, TemplatePath) ->
	filelib:ensure_dir(CachePath),
	os:cmd(["pandoc -o ", CachePath, ".srccache", " ", SrcPath]),
	render_tpl(CachePath, TemplatePath).

render_tpl(CachePath, TemplatePath) ->
	{ok, Tpl} = file:read_file(TemplatePath),
	{ok, Content} = file:read_file([CachePath, ".srccache"]),
	[Top, Bottom] = binary:split(Tpl, <<"++Content++">>),
	file:write_file(CachePath, [Top, Content, Bottom]).

check_cache(SourcePath, CachePath, TemplatePath) ->
	Src = filelib:last_modified(SourcePath),
	SrcCache = filelib:last_modified([CachePath, ".srccache"]),
	Cache = filelib:last_modified(CachePath),
	Tpl = filelib:last_modified(TemplatePath),
	if
		Src =:= 0 -> error; 
		Tpl =:= 0 -> error;
		Src > SrcCache -> render;
		Tpl > Cache -> render_tpl;
		true -> ok
	end.

find_template(DocRoot, RelPath) ->
	FileTpl = filename:join([DocRoot, [RelPath, ".html"]]),
	case filelib:is_file(FileTpl) of
		true ->
			FileTpl;
		false ->
			find_dir_template(DocRoot, filename:dirname(FileTpl))
	end.

find_dir_template(DocRoot, "/") ->
	filename:join([DocRoot, "_template.html"]);
find_dir_template(DocRoot, ".") ->
	filename:join([DocRoot, "_template.html"]);
find_dir_template(DocRoot, DirPath) ->
	DirTpl = filename:join([DirPath, "_template.html"]),
	case filelib:is_file(DirTpl) of
		true ->
			DirTpl;
		false ->
			find_dir_template(DocRoot, filename:dirname(DirPath))
	end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
