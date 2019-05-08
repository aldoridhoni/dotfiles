#!/bin/env bash
# Quickly change ip address in hosts file
# Author : Aldo Ridhoni

# immediately exit if any command has a non-zero exit status
set -e
# If any command in a pipeline fails,
# that return code will be used as the return code of the whole pipeline.
set -o pipefail

hosts_file=/etc/hosts
opendns_ip=208.67.222.222

if [[ $# -lt 1 ]]
then
    echo "Usage: $0 hostname [ip]"
    exit 1;
fi

host=$1

if [ ! -z "$2" ]
then
    ip=$2
else
    if hash dig 2>/dev/null; then
        ip=$(dig +short @$opendns_ip -p5353 $host A | tail -n 1)
    elif hash drill 2>/dev/null; then
        resolv_temp=$(mktemp /tmp/resolv.XXXXXXXXXX) ||\
            { echo "Failed to create temp file" 1>&2; exit 1; }
        echo "nameserver $opendns_ip" > $resolv_temp
        ip=$(drill -4 -c $resolv_temp -p5353 $host | sed '/^;;/ d' |\
                 sed '/^\d*$/ d' | grep 'IN[[:space:]]A'| head -1 |\
                 awk '{print $5}');
        rm $resolv_temp; # this will not run if pipefail
    else
        # We could use host but that would not bypass port 53 interception.
        echo "Command dig(1) not found." 1>&2;
        exit 1;
    fi
fi

echo $ip;
if [[ $ip ]]; then
    echo "IP: $ip"

    # GNU sed
    # sed -ir "/$host/s/0.0.0.0/$ip/" $hosts_file
    if hash sudo 2>/dev/null; then
        sudo sed -i "/\b$host\b/d" $hosts_file;
        echo "$ip $host" | sudo tee -a $hosts_file;
    elif [ "$(id -u)" == "0" ]; then
        sed -i "/\b$host\b/d" $hosts_file;
        echo "$ip $host" | tee -a $hosts_file;
    else
        echo "Please run as root or install sudo" 1>&2;
    fi
else
    echo "DNS Lookup error" 1>&2;
    exit 1;
fi
