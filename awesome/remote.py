#!/bin/env python3

import dbus, sys
from time import sleep

awesome = dbus.SessionBus().get_object('org.naquadah.awesome.awful', '/')
lua_eval = awesome.get_dbus_method('Eval', dbus_interface='org.naquadah.awesome.awful.Remote')


def marq(text, callback, width=None):
    text_len = len(text)
    if width:
        if int(width) > text_len:
            width = text_len - 1
    else:
        width = text_len - 1

    i = 0
    while True:
        output = text[i % text_len:] + text[:i % text_len]
        callback(output[:width])
        print("\r" + str(i) +  output[:width], end="")
        sleep(0.3)
        if (i % text_len) is 0:
            sleep(0.3)
            i = 1
        else:
            i = i + 1

def settextbox(text):
    msg = "mytextbox:set_text(\"|{}|\")".format(text)
    lua_eval(msg)

def main():
    text = "aldo ridhoni"
    if len(sys.argv) > 1:
        text = sys.argv[1]
    text = text + " "
    marq(text, settextbox)

if __name__ == "__main__":
    main()

