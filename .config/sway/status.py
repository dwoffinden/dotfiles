#!/usr/bin/env python3

from datetime import datetime
from numpy import clip
from psutil._common import bytes2human
from time import perf_counter, sleep

import psutil

def net_diff(old):
    stats = psutil.net_io_counters(nowrap=True)
    return stats, stats.bytes_sent - old.bytes_sent, stats.bytes_recv - old.bytes_recv

def bps(n):
    return bytes2human(n) + '/s'

def block(full_text):
    # TODO: optional colours, etc (see https://man.archlinux.org/man/swaybar-protocol.7.en)
    return f'{{"full_text":"{full_text}"}}'

net_stats = psutil.net_io_counters(nowrap=True)

print('{"version":1}\n[', end='')
while True:
    start_time = perf_counter()
    # TODO: Lil' history graphs? Click to expand?

    # TODO: AC/not 🔌⚡, charge/discharge time
    bat = psutil.sensors_battery().percent

    # TODO: eth vs wifi SSID? latency? strength? errors/drops?
    net_stats, netup, netdn = net_diff(net_stats)

    # TODO: load avg? frequency? pegged cores?
    cpu = psutil.cpu_percent(interval=None)

    # TODO: be more dynamic? include more sensors? include fans?
    temp = psutil.sensors_temperatures()['coretemp'][0].current

    time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    print('[', end='', flush=False)
    print(
            block(f'⬆️{bps(netup)}⬇️{bps(netdn)}'),
            block(f'🔋{bat:.0f}%'),
            block(f'CPU {cpu:.0f}%'),
            block(f'🌡️{temp:.0f}°C'),
            block(time),
            sep=',',
            end='],',
            flush=True)

    elapsed = perf_counter() - start_time

    sleep(1 - clip(elapsed, 0, 1))
