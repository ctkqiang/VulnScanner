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
    22) echo "SSH - 安全远程访问机器的协议" ;;
    80) echo "HTTP - 未加密网页流量的标准协议" ;;
    443) echo "HTTPS - 加密的网页流量（HTTP over SSL/TLS）" ;;
    3306) echo "MySQL - MySQL数据库的默认通信端口" ;;
    5432) echo "PostgreSQL - PostgreSQL数据库的默认通信端口" ;;
    6379) echo "Redis - 内存数据结构存储，常用于缓存或数据库" ;;
    27017) echo "MongoDB - MongoDB NoSQL数据库的默认端口" ;;
    21) echo "FTP - 文件传输协议，用于机器间文件传输" ;;
    25) echo "SMTP - 用于发送电子邮件的协议" ;;
    3389) echo "RDP - 远程桌面协议，用于远程访问Windows机器" ;;
    8080) echo "HTTP-Alt - HTTP的替代端口，常用于开发或代理" ;;
    8081) echo "HTTP-备用端口 - 常用于Web服务、开发或测试环境" ;;
    5228) echo "Google Play Services - 用于Google Play服务的端口，负责推送通知和数据同步" ;;
    5223) echo "Apple Push Notification Service - 苹果推送通知服务端口，用于iOS/macOS设备接收通知" ;;
    110) echo "POP3 - 邮局协议3，用于接收电子邮件" ;;
    143) echo "IMAP - 互联网邮件访问协议，用于同步电子邮件" ;;
    993) echo "IMAPS - IMAP加密协议，通过SSL/TLS加密邮件传输" ;;
    465) echo "SMTPS - SMTP加密协议，保障邮件发送的安全" ;;
    5900) echo "VNC - 虚拟网络计算，用于远程桌面访问" ;;
    11211) echo "Memcached - 高性能分布式内存缓存系统" ;;
    69) echo "TFTP - 简易文件传输协议，用于小文件传输" ;;
    161) echo "SNMP - 简单网络管理协议，用于管理和监控网络设备" ;;
    8443) echo "HTTPS-Alt - HTTPS的备用端口，常用于安全网站" ;;
    *) echo "未知端口" ;;
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
