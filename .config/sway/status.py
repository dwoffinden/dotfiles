#!/usr/bin/env python3

from datetime import datetime
from psutil._common import bytes2human
from time import sleep

import psutil

def net_diff(old):
    stats = psutil.net_io_counters(nowrap=True)
    return stats, stats.bytes_sent - old.bytes_sent, stats.bytes_recv - old.bytes_recv

def bps(n):
    return bytes2human(n) + '/s'

net_stats = psutil.net_io_counters(nowrap=True)

print('{"version":1}\n[', end='')
while True:
    # TODO: less awful code, inefficiency is probs fine
    # TODO: colours? https://man.archlinux.org/man/swaybar-protocol.7.en
    # TODO: Lil' history graphs? Click to expand?

    # TODO: AC/not 🔌⚡, charge/discharge time
    bat = psutil.sensors_battery().percent

    # TODO: eth vs wifi SSID? latency? strength? errors/drops?
    net_stats, netup, netdn = net_diff(net_stats)

    # TODO: load avg? frequency? pegged cores?
    cpu = psutil.cpu_percent(interval=None)

    # TODO: be more dynamic? include more sensors?
    temp = psutil.sensors_temperatures()['coretemp'][0].current

    time = datetime.now().strftime('%Y-%m-%d %k:%M:%S')

    line = f'[{{"full_text":"⬆️{bps(netup)}⬇️{bps(netdn)}"}},{{"full_text":"🔋{bat:.0f}%"}},{{"full_text":"CPU {cpu:.0f}%"}},{{"full_text":"🌡️{temp:.0f}°C"}},{{"full_text":"{time}"}}],'
    print(line, end='', flush=True)
    sleep(1)
