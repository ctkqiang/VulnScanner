#!/bin/bash
# ©2025 上饶满星科技
# 作者: 钟智强
# 邮箱: johnmelodymel@qq.com
#
# 本软件受中华人民共和国著作权法保护
# 未经授权，不得复制、修改或分发本代码
#
# 入站连接监控工具
# 版本: 1.0.0

# 📂 日志文件路径
LOG_FILE="$HOME/入站连接日志.log"
TMP_FILE="/tmp/当前连接.tmp"

# 🖊️ 初始化日志文件
if [ ! -f "$LOG_FILE" ]; then
    echo "时间戳 | 来源IP | 目标端口" >"$LOG_FILE"
fi

echo "\033[1;35m🌸 开始监控入站连接啦，姐妹！🌸\033[0m"

# 动画函数：女生风小花转圈圈
spin() {
    local -a marks=('🌸' '🌼' '🌺' '🌻' '🌷' '🌹')
    local i=0
    for ((j = 0; j < 10; j++)); do
        printf "\r\033[1;35m%s 正在温柔地检测中，请稍等哦~\033[0m" "${marks[$i]}"
        i=$(((i + 1) % ${#marks[@]}))
        sleep 0.2
    done
    printf "\r\033[K" # 清除动画行
}

# 端口说明函数
explain_port() {
    case "$1" in
    # 基础系统与连接端口
    20) echo "FTP-Data - 文件传输协议数据通道" ;;
    21) echo "FTP - 文件传输协议，用于机器间文件传输" ;;
    22) echo "SSH - 安全远程访问机器的协议" ;;
    23) echo "Telnet - 不安全的远程登录服务，应避免使用" ;;
    25) echo "SMTP - 用于发送电子邮件的协议" ;;
    53) echo "DNS - 域名系统，将域名转换为IP地址" ;;
    67) echo "DHCP-Server - 动态主机配置协议服务端" ;;
    68) echo "DHCP-Client - 动态主机配置协议客户端" ;;
    69) echo "TFTP - 简易文件传输协议，用于小文件传输" ;;
    80) echo "HTTP - 未加密网页流量的标准协议" ;;
    110) echo "POP3 - 邮局协议3，用于接收电子邮件" ;;
    123) echo "NTP - 网络时间协议，用于时间同步" ;;
    143) echo "IMAP - 互联网邮件访问协议，用于同步电子邮件" ;;
    161) echo "SNMP - 简单网络管理协议，用于管理和监控网络设备" ;;
    162) echo "SNMP-Trap - SNMP陷阱，用于设备向管理站发送通知" ;;
    389) echo "LDAP - 轻型目录访问协议，用于目录服务" ;;
    443) echo "HTTPS - 加密的网页流量（HTTP over SSL/TLS）" ;;
    445) echo "SMB - 服务器消息块，用于文件共享" ;;
    465) echo "SMTPS - SMTP加密协议，保障邮件发送的安全" ;;
    514) echo "Syslog - 系统日志协议，用于网络设备日志" ;;
    587) echo "SMTP Submission - 邮件客户端提交邮件的端口" ;;
    636) echo "LDAPS - 安全的LDAP，通过SSL加密" ;;
    853) echo "DNS over TLS - 加密DNS请求" ;;
    993) echo "IMAPS - IMAP加密协议，通过SSL/TLS加密邮件传输" ;;
    995) echo "POP3S - POP3加密协议，通过SSL加密" ;;
    1194) echo "OpenVPN - 开源VPN解决方案的默认端口" ;;
    1433) echo "MS SQL - Microsoft SQL Server数据库服务" ;;
    1434) echo "MS SQL Monitor - SQL Server浏览器服务" ;;
    1521) echo "Oracle - Oracle数据库的默认监听端口" ;;
    1701) echo "L2TP - 第2层隧道协议，常用于VPN" ;;
    1723) echo "PPTP - 点对点隧道协议，用于VPN连接" ;;
    1812) echo "RADIUS - 远程认证拨号用户服务" ;;
    1813) echo "RADIUS Accounting - RADIUS计费服务" ;;
    2049) echo "NFS - 网络文件系统，用于文件共享" ;;
    2375) echo "Docker - Docker REST API（不安全）" ;;
    2376) echo "Docker-TLS - Docker安全API" ;;
    3306) echo "MySQL - MySQL数据库的默认通信端口" ;;
    3389) echo "RDP - 远程桌面协议，用于远程访问Windows机器" ;;
    4369) echo "EPMD - Erlang端口映射守护进程" ;;
    5222) echo "XMPP - 可扩展消息与存在协议，用于即时通讯" ;;
    5223) echo "Apple Push Notification Service - 苹果推送通知服务端口，用于iOS/macOS设备接收通知" ;;
    5228) echo "Google Play Services - 用于Google Play服务的端口，负责推送通知和数据同步" ;;
    5229) echo "Google Play Services Alternative - Google服务备用端口" ;;
    5230) echo "Google Play Services Alternative - Google服务备用端口" ;;
    5432) echo "PostgreSQL - PostgreSQL数据库的默认通信端口" ;;
    5666) echo "NRPE - Nagios远程插件执行器" ;;
    5672) echo "AMQP - 高级消息队列协议" ;;
    5900) echo "VNC - 虚拟网络计算，用于远程桌面访问" ;;
    5901) echo "VNC-1 - VNC显示1" ;;
    5984) echo "CouchDB - CouchDB数据库HTTP接口" ;;
    6379) echo "Redis - 内存数据结构存储，常用于缓存或数据库" ;;
    6443) echo "Kubernetes API - Kubernetes集群API服务器" ;;
    6514) echo "Syslog-TLS - 加密的系统日志协议" ;;
    6667) echo "IRC - 互联网中继聊天" ;;
    8000) echo "Web开发端口 - 常用于各种Web服务和开发环境" ;;
    8008) echo "HTTP Alternative - HTTP备用端口" ;;
    8080) echo "HTTP-Alt - HTTP的替代端口，常用于开发或代理" ;;
    8081) echo "HTTP-备用端口 - 常用于Web服务、开发或测试环境" ;;
    8086) echo "InfluxDB - 时序数据库HTTP API" ;;
    8088) echo "Hadoop - Apache Hadoop的备用端口" ;;
    8389) echo "Apache Airflow - 工作流管理平台" ;;
    8443) echo "HTTPS-Alt - HTTPS的备用端口，常用于安全网站" ;;
    8500) echo "Consul - 服务发现和配置工具" ;;
    8883) echo "MQTT-SSL - 加密的消息队列遥测传输" ;;
    8888) echo "JupyterLab - Web开发与数据科学环境" ;;
    9000) echo "Portainer/SonarQube - 容器管理/代码质量平台" ;;
    9090) echo "Prometheus - 监控系统和时序数据库" ;;
    9092) echo "Kafka - 分布式流处理平台" ;;
    9200) echo "Elasticsearch - 分布式搜索和分析引擎HTTP" ;;
    9300) echo "Elasticsearch-Transport - Elasticsearch节点间通信" ;;
    9418) echo "Git - Git协议端口" ;;
    10000) echo "Webmin - Web系统管理界面" ;;
    11211) echo "Memcached - 高性能分布式内存缓存系统" ;;
    15672) echo "RabbitMQ Management - RabbitMQ管理界面" ;;
    27017) echo "MongoDB - MongoDB NoSQL数据库的默认端口" ;;
    27018) echo "MongoDB Shard - MongoDB分片服务器" ;;
    27019) echo "MongoDB Config - MongoDB配置服务器" ;;
    33060) echo "MySQL X Protocol - MySQL文档存储协议" ;;
    44818) echo "EtherNet/IP - 工业自动化协议" ;;
    49152) echo "动态端口 - IANA定义的动态端口范围开始" ;;
    50000) echo "SAP - 常用于SAP系统" ;;
    54321) echo "PostgreSQL Alternative - PostgreSQL备用端口" ;;
    *) echo "未知端口 - 请查阅IANA端口注册表获取更多信息" ;;
    esac
}

# ♾️ 无限循环，每10秒检查一次
while true; do
    spin
    # 👀 使用 lsof 抓取当前 ESTABLISHED 状态的 TCP 连接
    lsof -iTCP -nP | grep ESTABLISHED | grep -v "127.0.0.1" >"$TMP_FILE"

    while read -r line; do
        # 🧼 提取来源 IP 和目标端口
        remote=$(echo "$line" | awk '{print $9}' | awk -F'->' '{print $2}')
        src_ip=$(echo "$remote" | awk -F':' '{print $1}')
        dst_port=$(echo "$remote" | awk -F':' '{print $2}')
        timestamp=$(date '+%Y-%m-%d %H:%M:%S')

        # 📌 日志去重：如果已记录，就跳过
        grep -F "$src_ip" "$LOG_FILE" | grep -F "$dst_port" >/dev/null

        if [ $? -ne 0 ]; then
            port_desc=$(explain_port "$dst_port")
            echo "$timestamp | $src_ip | $dst_port ($port_desc)" >>"$LOG_FILE"
            echo "\033[1;36m✨📥 新连接发现！\033[1;33m $src_ip \033[1;35m→\033[1;32m 本地端口 $dst_port \033[0m\033[1;34m($port_desc)\033[0m"
        fi
    done <"$TMP_FILE"

    sleep 10 # ⏲️ 间隔10秒
done
