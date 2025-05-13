%% ------------------------------------------------------------------
%% VulnScanner 扫描器主模块
%%  ©2025 上饶满星科技
%%  作者: 钟智强 @ctkqiang
%%  邮箱: johnmelodymel@qq.com
%%
%%  本软件受中华人民共和国著作权法保护
%%  未经授权，不得复制、修改或分发本代码
%%
%% 这个模块实现了基础的安全扫描功能，涵盖了常见的渗透测试与安全检测需求，具体包括：
%%   - 检查依赖工具是否安装（sqlmap、nmap、whatweb、subfinder、katana 等）
%%   - MySQL 端口扫描，快速发现数据库暴露风险
%%   - .env 文件敏感信息扫描，检测网站配置泄露
%%   - 网站技术栈探测，辅助后续漏洞分析
%%   - SQL 注入检测，自动化发现注入点
%%   - 目录扫描，发现隐藏资源
%%   - SSH 端口安全检测
%%   - JS 文件收集与分析，辅助 XSS/敏感信息挖掘
%%   - 启动本地监控脚本，实时监控入站连接
%%
%% 每个函数都配有详细注释，方便理解和维护～
%%
%% ------------------------------------------------------------------
%%
%%  模块导出函数使用说明
%%  直接在 Erlang shell 中调用，所有参数均为字符串
%%
%%  1. 启动依赖检查
%%     scanner:start().
%%
%%  2. MySQL 端口扫描
%%     scanner:scanSQL("192.168.1.100").
%%
%%  3. .env 敏感信息扫描
%%     scanner:scanENV("http://example.com").
%%
%%  4. 网站技术栈探测
%%     scanner:getTech("http://example.com").
%%
%%  5. SQL 注入检测（需带协议，参数可自定义）
%%     scanner:sqlInjection("http://example.com/page?id=1", "--batch --dbs").
%%
%%  6. 目录扫描
%%     scanner:dirsearch("http://example.com").
%%
%%  7. SSH 端口安全检测
%%     scanner:scanSSH("192.168.1.100").
%%
%%  8. JS 文件收集与分析
%%     scanner:jsanal("http://example.com").
%%
%%  9. 子域名收集
%%     scanner:scanSubdomain("example.com").
%%
%% 10. 执行任意系统命令（慎用！）
%%     scanner:exec("ls -l").
%%
%% 11. 启动本地监控脚本
%%     scanner:scan().
%%
%% 说明：
%%   - 所有函数结果直接输出到控制台
%%   - URL 参数请带 http:// 或 https://
%%   - 请确保相关外部工具已安装并在 PATH 中
%%
%% ------------------------------------------------------------------
%%
%%
-module(scanner).
%%
%% 导出接口函数，方便外部调用
-export([scanSQL/1]).           %% 扫描 MySQL 端口
-export([scanENV/1]).           %% 扫描 .env 文件
-export([getTech/1]).           %% 探测网站技术栈
-export([sqlInjection/2]).      %% SQL 注入检测（支持自定义参数）
-export([dirsearch/1]).         %% 目录扫描
-export([scanSSH/1]).           %% SSH 端口扫描
-export([jsanal/1]).            %% JS 文件收集与分析
-export([scanSubdomain/1]).     %% 子域名收集
-export([exec/1]).              %% 执行系统命令，返回命令输出
%%
-export([start/0]).             %% 检查依赖并启动
-export([scan/0]).              %% 启动监控脚本
%%
%% ------------------------------------------------------------------
%% start/0
%% 功能：检查扫描器依赖的外部工具是否已安装
%% 步骤：
%%   1. 依次检查 sqlmap、nmap、whatweb、subfinder、katana 是否存在于系统环境变量中
%%   2. 如果缺少依赖会温柔提示用户安装，否则提示一切就绪
%% 返回值：
%%   - ok    ：所有依赖已安装
%%   - error ：有依赖缺失
%% ------------------------------------------------------------------
start() ->
    RequiredTools = [
        {"sqlmap", "sqlmap"},
        {"nmap", "nmap"},
        {"whatweb", "whatweb"},
        {"subfinder", "subfinder"},
        {"katana", "katana"}
    ],
    Missing = lists:filter(
        fun({_, Cmd}) ->
            case os:cmd("which " ++ Cmd) of
                []   -> true;
                "\n" -> true;
                _    -> false
            end
        end,
        RequiredTools
    ),
    case Missing of
        [] ->
            io:fwrite("\e[35m★･｡･ﾟﾟ･｡.✧ 所有依赖已安装，扫描器可以启动啦 ✧.｡･ﾟﾟ･｡★~n\e[0m"),
            ok;
        _  ->
            Names = [Name || {Name, _} <- Missing],
            io:fwrite("\e[31m[错误] 缺少必要工具: ~p，请先安装后再运行扫描器。\n\e[0m", [Names]),
            error
    end.

%% ------------------------------------------------------------------
%% scanSQL/1
%% 功能：使用 nmap 扫描目标 IP 的 MySQL 端口（3306），判断数据库是否暴露
%% 参数：
%%   - IP：目标主机 IP 地址（字符串）
%% 步骤：
%%   1. 执行 nmap 基础端口扫描
%%   2. 输出扫描结果
%%   3. 预留更深度探测命令（未自动执行）
%% ------------------------------------------------------------------
scanSQL(IP) ->
    io:fwrite("\e[35m★･｡･ﾟﾟ･｡.✧ 正在扫描MYSQL端口 ✧.｡･ﾟﾟ･｡★~n\e[0m"),
    Cmd = "nmap -Pn -p 3306 " ++ IP,
    _GetReason = "sudo nmap -Pn -sS -p3306 --reason " ++ IP, % 端口为 FILTERED 时可用
    _GrabCreds = "sudo nmap -sV -p 3306 --script=mysql-info,mysql-databases,mysql-users " ++ IP, % 端口为 OPEN 时可用
    Result = os:cmd(Cmd),
    Lines = string:tokens(Result, "\n"),
    lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, Lines).

%% ------------------------------------------------------------------
%% scanENV/1
%% 功能：尝试访问目标网站的 /.env 文件，检测是否存在敏感信息泄露
%% 参数：
%%   - URL：目标网站地址（不带结尾斜杠）
%% 步骤：
%%   1. 使用 curl 请求 /.env
%%   2. 输出响应内容
%% ------------------------------------------------------------------
scanENV(URL) ->
    io:fwrite("\e[35m★･｡･ﾟﾟ･｡.✧ 正在扫描 .env 文件 ✧.｡･ﾟﾟ･｡★~n\e[0m"),
    Cmd = "curl " ++ URL ++ "/.env",
    Result = os:cmd(Cmd),
    Lines = string:tokens(Result, "\n"),
    lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, Lines).

%% ------------------------------------------------------------------
%% getTech/1
%% 功能：使用 whatweb 探测目标网站的技术栈信息
%% 参数：
%%   - URL：目标网站地址
%% 步骤：
%%   1. 执行 whatweb 探测
%%   2. 输出详细技术栈信息
%% ------------------------------------------------------------------
getTech(URL) ->
    io:fwrite("\e[35m★･｡･ﾟﾟ･｡.✧ 正在探测网站技术栈 ✧.｡･ﾟﾟ･｡★~n\e[0m"),
    Cmd = "whatweb " ++ URL ++ " --verbose",
    Result = os:cmd(Cmd),
    Lines = string:tokens(Result, "\n"),
    lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, Lines).

%% ------------------------------------------------------------------
%% scan/0
%% 功能：启动本地 shell 脚本“监控入站连接.sh”，用于监控入站连接
%% 步骤：
%%   1. 直接调用 shell 脚本
%% ------------------------------------------------------------------
scan() ->
    os:cmd("sh 监控入站连接.sh").

%% ------------------------------------------------------------------
%% sqlInjection/2
%% 功能：检测目标 URL 是否存在 SQL 注入风险
%% 参数：
%%   - Url      ：目标 URL（需带 http/https 协议）
%%   - Argument ：sqlmap 额外参数（如注入点参数等）
%% 步骤：
%%   1. 用 httpc 请求目标页面，简单判断响应内容是否包含 "error"
%%   2. 调用 sqlmap 工具做进一步检测，并输出结果
%%   3. 支持自定义参数，默认参数为 "--batch --dbs"
%% ------------------------------------------------------------------
sqlInjection(Url, Argument) ->
    io:fwrite("\e[35m★･｡･ﾟﾟ･｡.✧ 正在检测 SQL 注入 ✧.｡･ﾟﾟ･｡★~n\e[0m"),
    InjectUrl = Url,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            % 检查响应中是否包含错误信息
            case string:str(Body, "error") > 0 of
                true  ->
                    io:put_chars("\e[31m[警告] "),
                    io:put_chars(Argument),
                    io:put_chars(" 存在 SQL 注入风险！\n\e[0m");
                false ->
                    io:put_chars("\e[32m[安全] "),
                    io:put_chars(Argument),
                    io:put_chars(" 未检测到 SQL 注入\n\e[0m")
            end;
        {ok, {{_, Code, _}, _, _}} ->
            io:format("\e[33m[提示] HTTP 状态码 ~p，无法判断 SQL 注入~n\e[0m", [Code]);
        {error, Reason} ->
            io:format("\e[31m[错误] 请求失败：~p~n\e[0m", [Reason])
    end,
    % 调用 sqlmap 进一步检测，并美观输出
    SqlmapCmd = "sqlmap --batch --disable-coloring -u \"" ++ InjectUrl ++ Argument ++ "\"",
    SqlmapResult = os:cmd(SqlmapCmd),
    io:put_chars(SqlmapResult).

%% ------------------------------------------------------------------
%% dirsearch/1
%% 功能：调用 dirsearch 工具对目标网站进行目录扫描，发现隐藏资源
%% 参数：
%%   - Url：目标网站地址
%% 步骤：
%%   1. 执行 dirsearch 扫描
%%   2. 输出扫描结果
%% ------------------------------------------------------------------
dirsearch(Url) ->
    DirsearchCmd = "dirsearch -u \"" ++ Url ++ " -e * -x 403 404",
    DirsearchResult = os:cmd(DirsearchCmd),
    io:put_chars(DirsearchResult).

%% ------------------------------------------------------------------
%% scanSSH/1
%% 功能：调用 nmap 对目标主机的 SSH 端口（22）进行安全检测
%% 参数：
%%   - Url：目标主机地址
%% 步骤：
%%   1. 执行 nmap 扫描 22 端口
%%   2. 输出扫描结果
%% ------------------------------------------------------------------
scanSSH(Url) ->
    SshAuditCmd = "nmap -p22 " ++ Url,
    SshAuditResult = os:cmd(SshAuditCmd),
    io:put_chars(SshAuditResult).

%% ------------------------------------------------------------------
%% jsanal/1
%% 功能：调用 katana 工具对目标网站进行 JS 文件收集与分析，辅助 XSS/敏感信息挖掘
%% 参数：
%%   - Url：目标网站地址
%% 步骤：
%%   1. 执行 katana 爬取 JS 文件（深度为 3）
%%   2. 输出收集到的 JS 文件及相关信息
%% ------------------------------------------------------------------
jsanal(Url) ->
    io:fwrite("\e[35m★･｡･ﾟﾟ･｡.✧ 正在收集和分析 JS 文件 ✧.｡･ﾟﾟ･｡★~n\e[0m"),
    KatanaCmd = "katana -u \"" ++ Url ++ "\" -depth 3",
    KatanaResult = os:cmd(KatanaCmd),
    io:put_chars(KatanaResult).

%% ------------------------------------------------------------------
%% scanSubdomain/1
%% 功能：调用 subfinder 工具对目标域名进行子域名收集，辅助资产梳理与渗透测试
%% 参数：
%%   - Url：目标主域名（如 example.com）
%% 步骤：
%%   1. 执行 subfinder 收集子域名
%%   2. 输出收集到的子域名列表
%% ------------------------------------------------------------------
scanSubdomain(Url) ->
    io:fwrite("\e[35m★･｡･ﾟﾟ･｡.✧ 正在收集子域名 ✧.｡･ﾟﾟ･｡★~n\e[0m"),
    SubfinderCmd = "subfinder -d " ++ Url,
    SubfinderResult = os:cmd(SubfinderCmd),
    io:put_chars(SubfinderResult).

%% ------------------------------------------------------------------
%% exec/1
%% 功能：执行任意系统命令，并返回命令输出（慎用！仅限受信任环境）
%% 参数：
%%   - Args：要执行的命令字符串
%% 步骤：
%%   1. 直接调用 os:cmd 执行命令
%%   2. 返回命令执行结果
%% ------------------------------------------------------------------
exec(Args) ->
    os:cmd(Args).
