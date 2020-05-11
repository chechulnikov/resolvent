# Resolvent
Business process tracker and organizer

# Roadmap for MVP
- [x] Process designer
- [ ] Board designer
- [ ] Board
- [ ] Calendar
- [ ] Routines
- [ ] Cycles
- [ ] Header
- [ ] Authentication & authorization
- [ ] Assemble all parts into app
- [ ] Backend

## How to run locally
### Docker way
To build
```bash
./scripts/build.sh
```

To start
```bash
./scripts/run.sh
```

To stop
```bash
./scripts/stop.sh
```

To restart
```bash
./scripts/restart.sh
```

### Debug way
1. Install elm, elm-format and elm-test by following
    ```
    brew install elm
    brew install elm-format
    npm install -g elm-test
    npm install -g elm-live
    ```
2. `./scripts/debug.run.sh`
