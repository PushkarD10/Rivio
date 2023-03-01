This is the sub-project containing backend code written in [haskell] powering nammayatri servers.

[haskell]: https://www.haskell.org/
## Getting Started

Getting Started with building and running the project.

### Pre-requisites

Before we can build the mobility project, there are some pre-requisites in the form of external dependencies which you must install, depending on you OS.

#### Haskell language toolchain

You'd need the Haskell language toolchain (GHC, cabal) installed in order build the project.

[GHCup](https://https://www.haskell.org/ghcup) is the preferred method to install Haskell.


#### Tools

These tools are required when working with the mobility repository:-

1. [Docker](https://www.docker.com/products/docker-desktop/) - we use docker and docker-compose for containers.
2. [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) - we use the stack build tool for building and running the entire project. ([GHCup](https://https://www.haskell.org/ghcup) can also be used to install Stack)

For Mac users, some additional tools may be required:-

1. [Xcode](https://developer.apple.com/xcode/)
2. [Home brew](https://brew.sh)

#### Linters and formatters

Install haskell linter and formatter by running this command in home directory, after you have stack installed.

```
$ stack install hlint ormolu
```

#### Extra Dependencies

Depending on your OS, you'd need install these dependencies or their equivalents.

For Mac

```
$ brew install libpq
$ brew install librdkafka
$ brew install postgres
$ brew install dhall
$ brew install jq
$ brew install parallel
```

For M1 or newer Macs (other dependencies)

```
arch -x86_64 /usr/local/bin/brew install libpq
arch -x86_64 /usr/local/bin/brew install librdkafka
arch -x86_64 /usr/local/bin/brew install postgres
arch -x86_64 /usr/local/bin/brew install dhall
arch -x86_64 /usr/local/bin/brew install jq
arch -x86_64 /usr/local/bin/brew install parallel
```

For Linux (other dependencies) or your package-manager equivalents

```
$ sudo apt-get install libpq-dev
$ sudo apt-get install librdkafka-dev
$ sudo apt-get install postgresql
$ sudo apt-get install dhall
$ sudo apt-get install jq
$ sudo apt-get install parallel
```

### Building and Development

After you've all the pre-requisite tools & dependencies installed, we can build the project for development.

#### Building

To build the project for development, we should compile the project with the command

```
$ stack build --fast
```

The `--fast` flag disables some compile-time optimizations for faster compile times and should only be used for development builds.

> **_Note:_**  For deployment, `stack build` command should be used to compile with optimizations.

This should start building the project and all it's dependencies.

#### Development

Once the above build command completes successfully, we can run the project for development.

The `dev/` folder at the project top-level contains all the relevant files and configs, should you need to change or inspect them.

##### Setting up development environment

To set up your development environment, from project root run

```
$ ./Backend/dev/setup.sh
```


##### Running the services
To run the project, we'd first need to run some services. These are provided via helpful `make` commands.


For running the database, redis, passetto and kafka run this command
```
$ make run-svc
```

That should run most of the services required.

More services, if needed, can be run with the following commands.

For running pgadmin run this command
```
$ make run-pgadmin
```

For running monitoring services like prometheus and grafana use this command
```
$ make run-monitoring
```


### Testing

The project comes with a range of tests in it's test-suites. These tests should pass for each correct build.

To run the test-suite for the project, first ensure you have the services running (see [running servcies section](#running-the-services)).

Run the following command in the project root folder after the services are up and running:-

```
$ stack test
```


## Usage

Each of the application has particular set of defined APIs and Schemas. To get available APIs/Schemas of any particular application, follow these steps

1. Copy the swagger json from `http://localhost:<port>/swagger` and use the relevant port (enlisted below)

| Application                              | Port   |
| -----------------------------------------|--------|
| rider-app                                | `8013` |
| static-offer-driver-app                  | `8014` |
| beckn-gateway                            | `8015` |
| dynamic-offer-driver-app                 | `8016` |
| mock-registry                            | `8020` |
| transporter-scheduler                    | `8053` |
| allocation-service                       | `9996` |

2. To run the requests one can use the Postman or any other API platform.

## Project Structure

The top level of the project has a very descriptive folder structure with helpful names.

The entire project is structured as a collection of smaller focused packages, which can be found listed in the top level `stack.yaml` file, under the _packages_ section.

Each package has clear separation of focuses w.r.t the functionality it provides, which helps with maintenance and development and provides clear designated areas to look at for a specific desired behavior and functionality. A good overview of the app structure be found in the table below:-

```text
├── rider-platform                                  : encapsulates all the rider side microservices
|   ├── rider-app (rider-app-exe)                   : Frontend facing APIs, rider app
|   └── public-transport
|       ├── Main (public-transport-rider-platform-exe)
|       └── search-consumer	(public-transport-search-consumer-exe)
├── provider-platform                               : encapsulates all the provider side microservices
|   ├── static-offer-driver-app                     : Microservices that power fixed price ride
|   |   |                                             hailing service
|   |   ├── Allocator (allocation-service-exe)      : Allocation service that matches a driver
|   |   |                                             to a ride
|   |   ├── Main (static-offer-driver-app-exe)      : Frontend facing APIs, driver app
|   |   └── Scheduler (transporter-scheduler-exe)   : Job scheduler for scheduling rental rides
|   ├── dynamic-offer-driver-app                    : Microservices that power dynamic pricing,
|   |   |                                             as quoted by the driver, service
|   |   ├── Allocator (driver-offer-allocator-exe)  : Allocation service that matches a driver
|   |   |                                             to a ride
|   |   └── Main (dynamic-offer-driver-app-exe)     : Frontend facing APIs, driver app
|   ├── driver-tracking-health-check
├── dashboard
|   ├── rider-dashboard (rider-dashboard-exe)       : Rider specific ops dashboard APIs
|   └── provider-dashboard (provider-dashboard-exe) : Provider specific ops dashboard APIs
├── kafka-consumers                                 : Microservices that consume messages from kafka
|                                                     to perform various tasks
├── mocks                                           : Mock servers that mock various
|                                                     third party APIs, used for local testing
└── utils
    ├── image-api-helper (image-api-helper-exe)
    └── route-extractor	(route-extractor-exe)
```

## FAQs

1. My project doesn't build.

    This could be because of a number of reasons. Most commonly though, we've found that more often than not it's because of the following reasons:-

    * We're missing a (OS specific) dependency.
    * Have an older or really new version of a required dependency.
    * Missing a required tool like stack or GHC.
    * Have incompatible version of the above tools like stack or GHC (though using stack minimizes this)
    * Our PATH environment/shell variable is not correctly configured.
    * We're running an incompatible, older or unsupported OS or OS version.

2. I can't run the services correctly.

    To make sure you can run the services correctly for development and testing, please ensure that you have Docker & Docker-compose installed correctly, are configured correctly in you PATH variable and you have the required permissions to run them.

3. I can't figure out the project structure.

    Please refer to the [Project Structure Section](#project-structure)

4. TBD...
