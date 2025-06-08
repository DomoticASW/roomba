# A Roomba simulated device for testing DomoticASW

## Run with docker

```sh
docker run ventus218/domoticasw-roomba
```

The following configurations can be passed to the container as environment variables.

For example `docker run -e NAME=MyRoomba -e BATTERY=10 ventus218/domoticasw-roomba`

| Variable name       | Default value                          | Explanation                                       | Admissible values                              |
| ------------------- | -------------------------------------- | ------------------------------------------------- | ---------------------------------------------- |
| HOST                | 0.0.0.0                                | The host to which bind the web server             | Any IPv4 interface                             |
| PORT                | 8080                                   | The port to which bind the web server             | Any available port                             |
| NAME                | Roomba                                 | The name of the robot                             | Any string                                     |
| BATTERY             | 50                                     | Initial battery of the robot                      | Values from 0 to 100 inclusive                 |
| BATTERY_RATE_MS     | 1000                                   | Ms needed for the battery to increase or decrease | Integers >= 0                                  |
| MODE                | Performance                            | Working mode of the robot                         | Silent, DeepCleaning, Performance              |
| CHANGE_ROOM_RATE_MS | 4000                                   | Ms needed for the roomba to increase change room  | Integers >= 0                                  |
| ROOMS               | Kitchen, Bedroom, Livingroom, Bathroom | All rooms in the house                            | A list of strings separated by commas ","      |
| INIT_ROOM           | (First of the provided rooms)          | The initial room                                  | Any string, it must be part of the given rooms |
| CHARGING_ROOM       | (Last of the provided rooms)           | The room with the charging station                | Any string, it must be part of the given rooms |
| STATE               | Cleaning                               | Initial state of the robot                        | Cleaning, Charging, GoingCharging              |
