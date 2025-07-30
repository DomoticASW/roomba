# A Roomba simulated device for testing DomoticASW

## Run with docker

```sh
docker run -e SERVER_DISCOVERY_PORT=30000 ventus218/domoticasw-roomba
```

The following configurations can be passed to the container as environment variables.

Mandatory ones are marked with a \*.

For example:

```sh
docker run \
    -e SERVER_DISCOVERY_PORT=30000 \
    -e NAME=MyRoomba \
    -e BATTERY=10 \
    ventus218/domoticasw-roomba
```

| Variable name            | Default value                          | Explanation                                          | Admissible values                               |
| ------------------------ | -------------------------------------- | ---------------------------------------------------- | ----------------------------------------------- |
| ID                       | roomba                                 | Device id                                            | Any string unique in the system                 |
| NAME                     | Roomba                                 | Robot name                                           | Any string                                      |
| BATTERY                  | 50                                     | Initial battery level                                | 0–100                                           |
| BATTERY_RATE_MS          | 1000                                   | Battery update interval (ms)                         | Integers >= 0                                   |
| MODE                     | Performance                            | Initial mode                                         | Silent, Deep cleaning, Performance               |
| CHANGE_ROOM_RATE_MS      | 4000                                   | Room change interval (ms)                            | Integers >= 0                                   |
| ROOMS                    | Kitchen, Bedroom, Livingroom, Bathroom | Rooms in the house                                   | Comma-separated list of strings                 |
| INIT_ROOM                | (First of the provided rooms)          | Starting room                                        | One of the listed rooms                         |
| CHARGING_ROOM            | (Last of the provided rooms)           | Room with charging station                           | One of the listed rooms                         |
| STATE                    | Cleaning                               | Initial robot state                                  | Cleaning, Charging, GoingCharging               |
| DISCOVERY_BROADCAST_ADDR | 255.255.255.255                        | Broadcast address to which send discovery announces  | Any valid broadcast address (ex: 192.168.1.255) |
| \*SERVER_DISCOVERY_PORT  |                                        | Port on which the server expects discovery announces | Any valid port                                  |
| SERVER_ADDRESS           |                                        | Should be set if roomba is already registered        | \<host>:\<port>                                 |
| PORT                     | 8080                                   | Port on which the device will listen                 | Any valid port                                  |
