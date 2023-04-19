# Weather Cache

Weather Cache is a caching server that stores geospatial weather data for different times. The server can be queried by clients for specific locations and times, and returns cached data if available, otherwise it calls the OpenWeather API, saves the result in the cache, and returns the result to the client.

## Usage

To query the server, use the following format:

```
?lat=FLOAT&lon=FLOAT or ?name=STRING or ?zipcode=STRING (one of three options); optional dt=INT (default: now)
```


- `lat` and `lon`: latitude and longitude of the location
- `name`: name of the location (currently not implemented)
- `zipcode`: zip code of the location (currently not implemented)
- `dt`: timestamp of the time to query in UNIX format (optional, default is current time).

The server requires the user to provide an OpenWeather API key in the `OW_APIKEY` environment variable.

## Configuration

To run the server, the user should provide a configuration file in the following format in the file named `config` in the root of the repo:

```
(
  (port INT)
  (locations
    ((Coords ((lat FLOAT) (lon FLOAT)))
     (Coords ((lat FLOAT) (lon FLOAT)))
     (Name STRING)
     (Zipcode STRING)))
  (update_period_sec INT)
  (time_accuracy_sec INT)
  (distance_accuracy_km INT)
)
```

- `port`: port number to run the server on.
- `locations`: a list of locations to pre-fetch weather data for, at regular intervals. Each location can be specified as coordinates (`Coords`), a name (`Name`), or a zip code (`Zipcode`).
- `update_period_sec`: the time in seconds between updates of the pre-cached data.
- `time_accuracy_sec`: the accuracy of the time data in the cache, in seconds.
- `distance_accuracy_km`: the accuracy of the geospatial data in the cache, in kilometers.

## Building and Running

To build the server, run:

```
dune build
```

To run the server, provide a configuration file in the format described above, and run:

```
dune exec bin/main.exe -- CONFIG_FILE
```

where `CONFIG_FILE` is the path to the configuration file.
