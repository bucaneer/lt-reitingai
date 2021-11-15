### Lietuvos partijų populiarumo reitingai

Pateikiami surinkti partijų populiarimo duomenys pagal visuomenės aplklausas (`polls.csv`), jų apibendrinimo modelio skriptas (`model.R`) bei naujausi apibendrinti reitingai (`_output/model-latest.csv`). Failas `index.html` ir katalogas `assets` yra skirti interaktyviam duomenų atvaizdavimui.

Visuomenės apklausų duomenys surinkti iš žiniasklaidos. Apklausų agentūros nurodytos `polls.csv` stulpelyje `pollster`:

- Baltijos tyrimai / ELTA
- OMD Snapshots / Laisvės partija
- RAIT / BNS
- Spinter tyrimai / Delfi
- Vilmorus / Lietuvos rytas

Seimo rinkimų duomenys iš Vyriausiosios rinkimų komisijos pažymėti "VRK".

Apklausų apibendrinimo modelio skriptas paremtas [Poll Tracker](https://github.com/jackobailey/poll_tracker), autorius Jack Bailey.

*************************************

### Lithuanian party opinion polling

This repo provides a collection of opinion poll results (`polls.csv`), a pooling model script (`model.R`) and the latest pooled results (`_output/model-latest.csv`). File `index_en.html` and directory `assets` are used for interactive display of the results.

Opinion poll data is collected from public media. The pollsters are specified in `polls.csv`:

- Baltijos tyrimai / ELTA
- OMD Snapshots / Laisvės partija
- RAIT / BNS
- Spinter tyrimai / Delfi
- Vilmorus / Lietuvos rytas

Parliamentary election results from The Central Electoral Commission of Lithuania are marked "VRK".

The pooling model script is based on the [Poll Tracker](https://github.com/jackobailey/poll_tracker) by Jack Bailey.