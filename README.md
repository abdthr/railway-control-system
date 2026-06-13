# Model Railway Control System

A multi-component control system for a model railway (NMBS / Infrabel / Z21
DCC), written in **Racket**. It models trains, switches, signals and level
crossings, routes trains autonomously across a track graph, and drives either a
**software simulator** or **real Z21/DCC hardware** through the same backend —
all coordinated over a **TCP client/server** protocol.

> Built as the Programming Project 2 (Programmeerproject 2) at the
> Vrije Universiteit Brussel. ~2,900 lines of Racket across 16+ modules.

<!-- TODO: export your architecture diagram from the project report to
     docs/architecture.png and embed it here — it's the single best thing you
     can put at the top of this README:
     ![architecture](docs/architecture.png) -->

## Why it's interesting

- **Three-tier architecture (NMBS → Infrabel → Z21/DCC).** A clean separation
  between the control cockpit (NMBS), the stateful backend that owns the railway
  (Infrabel), and the hardware/simulation layer (Z21/DCC). The same backend core
  drives a simulator or physical hardware behind one abstraction.
- **TCP client/server protocol.** The cockpit and backend communicate over
  sockets with an explicit message protocol (request/response, ok/error
  framing), so they can run as separate processes.
- **Autonomous routing on a track graph.** Breadth-first pathfinding that
  respects the *physics of switches* — a route is only valid if the switch
  transitions along it are physically possible.
- **Concurrency done carefully.** A position-tracker thread, per-train drive
  monitors, and **track-segment reservations** that prevent two trains from
  claiming the same piece of track.
- **ADT-driven design.** Trains, switches, signals, crossings, and the railway
  topology are each modelled as abstract data types with documented
  dependencies.

## Module map

| Layer | Files | Responsibility |
|-------|-------|----------------|
| **NMBS** (cockpit / client) | `nmbs.rkt`, `nmbs-client.rkt`, `GUI.rkt` | User-facing control + GUI; talks to Infrabel over TCP |
| **Infrabel** (backend) | `infrabel-core.rkt`, `infrabel-server.rkt` | Owns railway state, drives trains, runs the TCP server |
| **Protocol** | `tcp-protocol.rkt`, `protocols.rkt` | Message framing and request/response protocol |
| **Routing** | `routing.rkt`, `graph.rkt` | Track graph + BFS pathfinding with switch constraints |
| **Domain model** | `railway.rkt`, `train.rkt`, `switch.rkt`, `signal.rkt`, `crossing.rkt` | Railway topology and component ADTs |
| **Support** | `util.rkt`, `scenario.txt` | Utilities and an example scenario |

## Provided dependencies (not included)

This project runs on top of **two modules provided by the course (VUB)**, neither
of which is mine to redistribute, so **both are excluded from this repository**:

- **`simulator/`** — the VUB-provided railway **simulator** (software backend).
- **`hardware-library/`** — the VUB-provided **Z21/DCC hardware** abstraction
  library (real-hardware backend).

`infrabel-core.rkt` imports both:

```racket
(prefix-in sim: "simulator/interface.rkt")        ; VUB-provided simulator
(prefix-in hw:  "hardware-library/interface.rkt") ; VUB-provided Z21/DCC library
```

The code I wrote sits *on top* of these: it can drive either backend
(`'simulator` or `'hardware`) through one abstraction. To run it, place the
course-provided `simulator/` and `hardware-library/` folders at the repository
root so those paths resolve.

## Running it

```bash
# 1. Place the provided simulator/ and hardware-library/ folders at the repo
#    root (see "Provided dependencies" above).
# 2. Start the Infrabel backend (TCP server):
racket infrabel-server.rkt
# 3. In another terminal, start the NMBS cockpit/client:
racket nmbs.rkt
```

The TCP port and host are configurable (see `tcp-protocol.rkt`,
`infrabel-server.rkt`, `nmbs-client.rkt`); the default port is `45678`.

## What I learned / would do differently

<!-- TODO (recruiters love this — 3–5 honest bullets), e.g.:
  - Designing a protocol so the cockpit and backend are truly decoupled.
  - Reservation logic was the hardest concurrency problem.
  - Would add automated tests for the routing graph next. -->

## License

MIT (my own code only) — see [LICENSE](LICENSE). The VUB-provided `simulator/`
and `hardware-library/` modules are not covered and are not redistributed here.
