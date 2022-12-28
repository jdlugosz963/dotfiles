#!/bin/sh

# rm /tmp/arduino-sketch-*/ -r
~/.local/bin/arduino-cli cache clean
~/.local/bin/arduino-cli compile --fqbn esp8266:esp8266:nodemcuv2
~/.local/bin/arduino-cli upload -p /dev/ttyUSB0 --fqbn esp8266:esp8266:nodemcuv2
