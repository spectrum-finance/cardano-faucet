# cardano-faucet

## How to dockerize
- Copy executable binary to ./temp-build/ in project's root
- Specify configuration in ./config/config.dhall or mount on startup with ``` -v "${pwd}/config.dhall:/etc/cardano-faucet/config.dhall"```
- run docker build command.
- pull docker image on a machine and run 

## Architecture

```mermaid
flowchart TD 

A0(HTTP API) -->|Req| A(Faucet)
A -->|"DripRequest{asset}"| C[RegisterDrip]
A -->|"AckUTxO{txOutRef}"| B2[RegisterFundingUTxO]
C -->|enqueue| D("Queue{asset=X}")
C -->|enqueue| F("Queue{asset=..}")
B2 --> G(UTxOStore)

D -->|dequeueN| X1{Executor}
F -->|dequeueN| X2{Executor}

G <-->|"GetUTxO | AckUTxO"| X1{Executor}
G <-->|"GetUTxO | AckUTxO"| X2{Executor}
```
