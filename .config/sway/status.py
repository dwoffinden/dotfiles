#!/usr/bin/env python3

from datetime import datetime
from time import sleep

import psutil

print('{"version":1}\n[', end='')
while True:
    # TODO: less awful code, inefficiency is probs fine
    # TODO: colours? https://man.archlinux.org/man/swaybar-protocol.7.en
    # TODO: net traffic?

    # TODO: AC/not 🔌⚡, charge/discharge time
    bat = f"{psutil.sensors_battery().percent:.0f}"

    # TODO: load avg? frequency? pegged cores?
    cpu = str(psutil.cpu_percent(interval=None))

    # TODO: be more dynamic? include more sensors?
    temp = f"{psutil.sensors_temperatures()['coretemp'][0].current:.0f}"

    time = datetime.now().strftime('%Y-%m-%d %k:%M:%S')

    line = '[{"full_text":"🔋' + bat + '%"},{"full_text":"CPU ' + cpu + '%"},{"full_text":"🌡️' + temp + '°C"},{"full_text":"' + time + '"}],'
    print(line, end='', flush=True)
    sleep(1)
