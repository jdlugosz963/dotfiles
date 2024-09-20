FROM python:3.12-rc-slim

RUN apt update && apt install -y wget && \
    wget https://downloads.arduino.cc/arduino-cli/nightly/arduino-cli_nightly-latest_Linux_64bit.tar.gz && \
    tar -C /usr/bin/ -xf  arduino-cli_nightly-latest_Linux_64bit.tar.gz

RUN mkdir ~/.arduino15 && \
    echo "board_manager:" > ~/.arduino15/arduino-cli.yaml && \
    echo "  additional_urls:" >> ~/.arduino15/arduino-cli.yaml && \
    echo "    - http://arduino.esp8266.com/stable/package_esp8266com_index.json" >> ~/.arduino15/arduino-cli.yaml


RUN arduino-cli core update-index && \
    arduino-cli core install esp8266:esp8266

WORKDIR /src