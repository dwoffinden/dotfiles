#!/usr/bin/env python3

import json

from colour import Color
from datetime import datetime
from sys import stdout
from time import perf_counter, sleep

import psutil


def net_diff(old):
    stats = psutil.net_io_counters(nowrap=True)
    return stats, stats.bytes_sent - old.bytes_sent, stats.bytes_recv - old.bytes_recv


def find_first(dict, keys):
    first = next((dict[key] for key in keys if key in dict), None)
    if first:
        return first
    else:
        raise Exception(f"No recognised keys in {dict.keys()}, expected one of {keys}")


def find_temp():
    return find_first(psutil.sensors_temperatures(), ["coretemp", "k10temp"])[0].current


# TODO: multiple/no fans? more compact formatting? krpm for some?
def find_fan():
    return find_first(psutil.sensors_fans(), ["thinkpad", "nouveau"])[0].current


# nicked from psutil._common bytes2human:
# https://github.com/giampaolo/psutil/blob/95db8bb96caf5540c45b9eff2229c0401b578c31/psutil/_common.py#L728-L745
def bytes2human(n):
    symbols = ("B", "K", "M", "G", "T", "P", "E", "Z", "Y")
    prefix = {}
    for i, s in enumerate(symbols[1:]):
        prefix[s] = 1 << (i + 1) * 10
    for symbol in reversed(symbols[1:]):
        if n >= prefix[symbol]:

            value = float(n) / prefix[symbol]
            return f"{value: >5.1f}{symbol}/s"
    return f"{n: >5.1f}{symbols[0]}/s"


def bps(n):
    return bytes2human(n)


def block(full_text, colour=None, urgent=None):
    # see https://man.archlinux.org/man/swaybar-protocol.7.en
    d = {"full_text": full_text}
    if colour:
        d["color"] = colour.hex_l
    if urgent is not None:
        d["urgent"] = urgent
    return d


def clip(a, a_min, a_max):
    return max(a_min, min(a, a_max))


BATTERY_COLOURS = list(Color("red").range_to(Color("lime"), 60))


def print_battery():
    # TODO: AC/not 🔌⚡, charge/discharge time
    bat = psutil.sensors_battery()

    if not bat:
        return None

    pc = bat.percent
    # 20% is full red, 80% is full lime
    colour = BATTERY_COLOURS[clip(int(pc) - 20, 0, 59)]
    urgent = True if pc < 20 else None

    return block(f"🔋{pc:.0f}%", urgent=urgent, colour=colour)


def main():
    net_stats = psutil.net_io_counters(nowrap=True)

    print('{"version":1}\n[', end="")
    while True:
        start_time = perf_counter()
        # TODO: Lil' history graphs? Click to expand?

        # TODO: eth vs wifi SSID? latency? strength? errors/drops?
        net_stats, netup, netdn = net_diff(net_stats)

        # TODO: load avg? frequency? pegged cores?
        cpu = psutil.cpu_percent(interval=None)

        ram = psutil.virtual_memory().percent

        # TODO: be more dynamic? include more sensors?
        temp = find_temp()

        fan = find_fan()

        time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        # TODO: refactor this to, e.g, a list of generator functions?
        #     Some could capture necessary state, most could be
        #     iter(lambda: block(...), None)
        #     allow each to return None to be skipped (e.g. no fans/battery)?

        d = [
            block(f"⬇️{bps(netdn)}"),
            block(f"⬆️{bps(netup)}"),
        ]
        b = print_battery()
        if b:
            d.append(b)

        d.append(block(f"CPU {cpu: >2.0f}%"))
        d.append(block(f"RAM {ram: >2.0f}%"))
        d.append(block(f"🌡️{temp:.0f}°C"))
        d.append(block(f"🌀{fan: >4.0f} RPM"))
        d.append(block(time))

        json.dump(d, stdout, ensure_ascii=False, separators=(",", ":"))
        stdout.write(",")
        stdout.flush()

        elapsed = perf_counter() - start_time

        sleep(1 - clip(elapsed, 0, 1))


if __name__ == "__main__":
    main()
