#!/usr/bin/env python3

from datetime import datetime
from numpy import clip
from time import perf_counter, sleep

import psutil

def net_diff(old):
    stats = psutil.net_io_counters(nowrap=True)
    return stats, stats.bytes_sent - old.bytes_sent, stats.bytes_recv - old.bytes_recv

def find_first(dict, keys):
    return next((dict[key] for key in keys if key in dict), None)

def find_temp():
    return find_first(psutil.sensors_temperatures(), ['coretemp', 'k10temp'])[0].current

# TODO: multiple/no fans? more compact formatting? krpm for some?
def find_fan():
    return find_first(psutil.sensors_fans(), ['thinkpad', 'nouveau'])[0].current

# nicked from psutil._common bytes2human:
# https://github.com/giampaolo/psutil/blob/95db8bb96caf5540c45b9eff2229c0401b578c31/psutil/_common.py#L728-L745
def bytes2human(n):
    symbols = ('B', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y')
    prefix = {}
    for i, s in enumerate(symbols[1:]):
        prefix[s] = 1 << (i + 1) * 10
    for symbol in reversed(symbols[1:]):
        if n >= prefix[symbol]:
            value = float(n) / prefix[symbol]
            return f'{value: >5.1f}{symbol}/s'
    return f'{n: >5.1f}{symbols[0]}/s'


def bps(n):
    return bytes2human(n)

def block(full_text):
    # TODO: optional colours, etc (see https://man.archlinux.org/man/swaybar-protocol.7.en)
    return f'{{"full_text":"{full_text}"}}'

net_stats = psutil.net_io_counters(nowrap=True)

print('{"version":1}\n[', end='')
while True:
    start_time = perf_counter()
    # TODO: Lil' history graphs? Click to expand?

    # TODO: AC/not 🔌⚡, charge/discharge time
    bat = psutil.sensors_battery()

    # TODO: eth vs wifi SSID? latency? strength? errors/drops?
    net_stats, netup, netdn = net_diff(net_stats)

    # TODO: load avg? frequency? pegged cores?
    cpu = psutil.cpu_percent(interval=None)

    ram = psutil.virtual_memory().percent

    # TODO: be more dynamic? include more sensors?
    temp = find_temp()

    fan = find_fan()

    time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    # TODO: refactor this to, e.g, a list of generator functions?
    #     allow each to return None?
    print('[', end='', flush=False)
    print(
            block(f'⬇️{bps(netdn)}'),
            block(f'⬆️{bps(netup)}'),
            sep=',',
            end=',',
            flush=False)

    if bat:
        print(block(f'🔋{bat.percent:.0f}%' if bat else 'no battery'), end=',', flush=False)

    print(
            block(f'CPU {cpu: >2.0f}%'),
            block(f'RAM {ram: >2.0f}%'),
            block(f'🌡️{temp:.0f}°C'),
            block(f'🌀{fan: >4.0f} RPM'),
            block(time),
            sep=',',
            end='],',
            flush=True)

    elapsed = perf_counter() - start_time

    sleep(1 - clip(elapsed, 0, 1))
