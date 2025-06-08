# A Roomba simulated device for testing DomoticASW

## Run with docker

```sh
docker run ventus218/domoticasw-roomba
```

The following configurations can be passed to the container as environment variables.

For example `docker run -e NAME=MyRoomba -e BATTERY=10 ventus218/domoticasw-roomba`

| Variable name       | Default value                          | Explanation                  | Admissible values                 |
| ------------------- | -------------------------------------- | ---------------------------- | --------------------------------- |
| HOST                | 0.0.0.0                                | Web server host              | Any IPv4 interface                |
| PORT                | 8080                                   | Web server port              | Any available port                |
| NAME                | Roomba                                 | Robot name                   | Any string                        |
| BATTERY             | 50                                     | Initial battery level        | 0–100                             |
| BATTERY_RATE_MS     | 1000                                   | Battery update interval (ms) | Integers >= 0                     |
| MODE                | Performance                            | Initial mode                 | Silent, DeepCleaning, Performance |
| CHANGE_ROOM_RATE_MS | 4000                                   | Room change interval (ms)    | Integers >= 0                     |
| ROOMS               | Kitchen, Bedroom, Livingroom, Bathroom | Rooms in the house           | Comma-separated list of strings   |
| INIT_ROOM           | (First of the provided rooms)          | Starting room                | One of the listed rooms           |
| CHARGING_ROOM       | (Last of the provided rooms)           | Room with charging station   | One of the listed rooms           |
| STATE               | Cleaning                               | Initial robot state          | Cleaning, Charging, GoingCharging |
