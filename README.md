# ProMatch — Prolog B2B Matchmaking (SWI-Prolog)

[![Docker Pulls](https://img.shields.io/docker/pulls/irfanuruchi/promatch)](https://hub.docker.com/r/irfanuruchi/promatch)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

---

ProMatch is an SWI-Prolog web application for event matchmaking. It manages events, participants, roles, timeslots, and per-person availability blocks, then generates conflict-free schedules using multiple algorithms (Greedy, CLP(FD) Optimal, Stable pairing). It also exports schedules to CSV and includes an AI intro assistant (optional Cerebras API + safe fallback template).

---

# Features

It allows for event creation and participant management (with roles). Timeslot management and per-person avaiability blocks.
Scheduling console with : Greedy scheduler , Optimal scheduler, Stable pairing plus scheduling. Also in rerun modes it replaces auto or fills gaps only. Built-in constraint checks (e.g double booking). CSV export

Also it has a built in AI assistant page with intro message generation fallback template or using external API call. Dockerized with healthcheck and persistent data volume.

---

# Quick start

For running the docker image recommended is using the terminal(its multi arch meaning you can use on Mac, linux or Windows via WSL), command to run:

```bash
docker pull irfanuruchi/promatch:latest
docker run --rm -p 3000:3000 \
  -v "$(pwd)/data:/app/data" \
  irfanuruchi/promatch:latest
```

Then from there you will see "Started server at http://localhost:3000/" so just go to localhost:3000 in your browser to use the UI, where for demo purposes the first run will seed demo data if the DB is empty/

For enabling persistence you can use the following terminal command :

```bash
docker run --rm -p 3000:3000 \
  -v "$(pwd)/data:/app/data" \
  irfanuruchi/promatch:latest
```

Enable Cerebras AI (optional):
```bash
docker run --rm -p 3000:3000 \
  -e CEREBRAS_API_KEY="YOUR_KEY_HERE" \
  -e CEREBRAS_MODEL="llama-3.3-70b" \
  -v "$(pwd)/data:/app/data" \
  irfanuruchi/prematch:latest
```

If CEREBRAS_API_KEY is not set, ProMatch automatically uses the built-in fallback template.

---

# Note 

Build as a course project for Logical and Functional Programming. The topic was chosen to demonstrate real constraint reasoning in Prolog:
availability, meeting uniqueness, schedule feasibility, and multi-algorithm scheduling,
with a complete web UI + persistence + Docker packaging.

---

## Technical overview (Algorithms + Data Model)

### Algorithms used
- **Greedy scheduling (heuristic):** builds a feasible schedule by selecting high-scoring pairs while enforcing constraints (no double booking, no unavailable slots, no duplicate active meetings).
- **Optimal scheduling (CLP(FD)):** models meeting assignments as finite-domain variables and uses constraint solving + optimization to maximize total match score subject to feasibility constraints.
- **Stable pairing (Gale–Shapley):** computes stable Startup ↔ Host pairs based on preference rankings derived from match scores and “wants_to_meet” rules, then schedules only those stable pairs (combined with CLP scheduling).

### Data model + persistence
- **Knowledge base facts:** `event/4`, `participant/4`, `participant_event/2`, `role/2`, `timeslot/3`, `meeting/6`, `unavailable/3`, etc.
- **Persistence strategy:** dynamic predicates are saved to a Prolog file (`data/promatch_db.pl`) and reloaded on startup. The Docker setup mounts `./data` to `/app/data` so the state survives container restarts.
- **Safety-by-design updates:** meeting creation and regeneration enforce rules like canonical pair ordering, conflict checks, and respecting manual/confirmed meetings.

---

# Licence

This project is protected under MIT Licence (see LICENSE for more information).

