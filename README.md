# dmm2-project
 
```mermaid
sequenceDiagram
    participant Application as App
    participant Scheduler as Sched
    participant DataSource as DS
    participant RenewableControl as RC

    App ->> Sched: Start Application
    Sched ->> DS: Fetch Energy Data
    alt Data Fetch Successful
        DS -->> RC: Energy Data
        RC -->> App: Data Processed
    else Data Fetch Failed
        DS --x Sched: Error
    end
    loop Every 15 minutes
        Sched ->> DS: Fetch Energy Data
        alt Data Fetch Successful
            DS -->> RC: Energy Data
            RC -->> App: Data Processed
        else Data Fetch Failed
            DS --x Sched: Error
        end
    end
