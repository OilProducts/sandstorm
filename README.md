# Sandstorm (3D Falling Sand)

A simple **3D falling-sand / cellular-automata** demo built with **Bevy 0.17**. Each particle occupies one voxel in a fixed-size 3D grid and updates in a fixed tick loop, producing sandbox-y piling, flow, and buoyancy effects.

## Run

```bash
cargo run
```

## Controls

- `Tab`: toggle mouse capture (flycam look)
- `W/A/S/D`: move
- `Space` / `Shift`: up / down
- `Ctrl`: speed boost
- `Mouse Wheel`: change brush radius
- `LMB`: place selected particle
- `RMB`: erase particles
- `1..8`: select material (Sand, Water, Stone, Wood, Oil, Lava, Smoke, Glass)
- `R`: clear the simulation
- `Esc`: release mouse capture

## Materials & interactions

This is intentionally “game-y” rather than physically accurate. The rules are:

- The world is a 3D voxel grid; each occupied cell contains at most one particle.
- Particles try to move one cell per update according to their material rules.
- **Static solids** never move and block everything.
- **Density swap:** when a moving particle tries to enter an occupied cell, it may swap places with the occupant **only if** the mover is *denser* and the occupant is *not static*. This is what makes heavier things sink and lighter fluids float.

### Materials

| Material | Type | Behavior | Notable interactions |
|---|---|---|---|
| **Stone** | Static solid | Never moves | Blocks all motion |
| **Wood** | Static solid | Never moves | Blocks all motion |
| **Glass** | Static solid | Never moves | Blocks all motion (rendered translucent) |
| **Sand** | Granular | Falls down; if blocked, tries diagonal down moves | Sinks through lower-density fluids via density swap |
| **Water** | Fluid | Falls, then spreads sideways | Displaces smoke; oil tends to float on top of water |
| **Oil** | Fluid | Like water, but lighter | Floats on water (water can swap into oil) |
| **Lava** | Fluid | Like water, but heavier and slower | Sinks through most non-static materials; rendered emissive (“glows”) |
| **Smoke** | Gas | Rises, then drifts sideways | Gets displaced by fluids/sand; can’t push through denser particles |

### “Emissive” note (lava)

Lava uses `StandardMaterial.emissive`, meaning it renders as self-lit and stays bright in darkness (and can contribute to post effects like bloom), but it does **not** emit real light into the scene by itself. Scene lighting still comes from actual `PointLight` / `DirectionalLight` entities.

## Tuning

- Grid size and cell size are in `src/sim.rs` (`GridConfig`).
- Movement rules are in `src/sim.rs` (`step_sand`, `step_fluid`, `step_smoke`).

