-module(connection_watcher).
-export([start/0]).

-define(LOG_FILE, "/home/your_user/入站连接日志.log").
-define(TMP_FILE, "/tmp/当前连接.tmp").
-define(SLEEP_INTERVAL, 10000). % 10 秒


start() ->
    ensure_log_file(),
    io:format("~n🌸 姐妹 Erlang 入站连接监听启动啦 🌸~n"),
    loop([]).

loop(Cache) ->
    print_spinner(),
    Lines = get_established_conns(),
    {NewCache, NewEntries} = process_lines(Lines, Cache),
    write_new_entries(NewEntries),
    timer:sleep(?SLEEP_INTERVAL),
    loop(NewCache).

%% ✅ 保证日志文件存在
ensure_log_file() ->
    case file:read_file(?LOG_FILE) of
        {ok, _} -> ok;
        _ -> file:write_file(?LOG_FILE, <<"时间戳 | 来源IP | 目标端口\n">>)
    end.

%% 🌐 获取当前 TCP 连接（非127.0.0.1）
get_established_conns() ->
    Output = os:cmd("lsof -iTCP -nP | grep ESTABLISHED | grep -v 127.0.0.1"),
    string:split(Output, "\n", all).

%% 🎯 提取 IP 和端口并判断是否新连接
process_lines(Lines, Cache) ->
    lists:foldl(
        fun(Line, {AccCache, AccNew}) ->
            case parse_conn(Line) of
                {ok, SrcIp, DstPort} ->
                    Entry = {SrcIp, DstPort},
                    case lists:member(Entry, AccCache) of
                        true -> {AccCache, AccNew}; % 已存在，跳过
                        false -> {[Entry | AccCache], [Entry | AccNew]}
                    end;
                _ -> {AccCache, AccNew}
            end
        end,
        {Cache, []},
        Lines
    ).

%% ✂️ 从 lsof 行提取 IP 和端口
parse_conn(Line) ->
    case string:tokens(Line, " ") of
        [_P, _PID, _U, _FD, _T, _D, _NA, _S, _F, Path | _] ->
            case string:tokens(Path, "->") of
                [_Local, Remote] ->
                    case string:tokens(Remote, ":") of
                        [Ip, Port] ->
                            {ok, Ip, Port};
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

%% 📝 写入新连接
write_new_entries([]) -> ok;
write_new_entries(Entries) ->
    Timestamp = format_time(calendar:local_time()),
    Lists = [io_lib:format("~s | ~s | ~s\n", [Timestamp, Ip, Port]) || {Ip, Port} <- Entries],
    file:write_file(?LOG_FILE, Lists, [append]),
    [io:format("✨📥 新连接：~s → 本地端口 ~s~n", [Ip, Port]) || {Ip, Port} <- Entries],
    ok.

%% ⏲️ 动画（Erlang 无法真的动起来，这里就是搞气氛）
print_spinner() ->
    io:format("🌼 监听中，请稍候...\n").

%% 🕒 格式化时间戳
format_time({{Y, M, D}, {H, Min, S}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Min, S]).
