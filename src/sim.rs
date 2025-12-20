use bevy::prelude::*;
use std::collections::HashSet;

const FIRE_TTL: u8 = 20;

const FIRE_IGNITE_OIL_PCT: u64 = 35;
const FIRE_IGNITE_WOOD_PCT: u64 = 20;
const LAVA_IGNITE_OIL_PCT: u64 = 30;
const LAVA_IGNITE_WOOD_PCT: u64 = 12;

const WATER_EVAPORATE_NEAR_FIRE_PCT: u64 = 18;
const VAPOR_CONDENSE_PCT: u64 = 2;
const SAND_GLASS_NEAR_LAVA_PCT: u64 = 22;

const CARDINAL_NEIGHBORS: [IVec3; 6] = [
    IVec3::new(1, 0, 0),
    IVec3::new(-1, 0, 0),
    IVec3::new(0, 1, 0),
    IVec3::new(0, -1, 0),
    IVec3::new(0, 0, 1),
    IVec3::new(0, 0, -1),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParticleKind {
    Sand,
    Water,
    Stone,
    Wood,
    Oil,
    Lava,
    Smoke,
    WaterVapor,
    Fire,
    Glass,
}

impl ParticleKind {
    pub fn name(self) -> &'static str {
        match self {
            ParticleKind::Sand => "Sand",
            ParticleKind::Water => "Water",
            ParticleKind::Stone => "Stone",
            ParticleKind::Wood => "Wood",
            ParticleKind::Oil => "Oil",
            ParticleKind::Lava => "Lava",
            ParticleKind::Smoke => "Smoke",
            ParticleKind::WaterVapor => "Water Vapor",
            ParticleKind::Fire => "Fire",
            ParticleKind::Glass => "Glass",
        }
    }

    pub fn density(self) -> f32 {
        match self {
            ParticleKind::Fire => 0.01,
            ParticleKind::WaterVapor => 0.03,
            ParticleKind::Smoke => 0.05,
            ParticleKind::Oil => 0.8,
            ParticleKind::Water => 1.0,
            ParticleKind::Wood => 1.2,
            ParticleKind::Sand => 2.0,
            ParticleKind::Lava => 2.4,
            ParticleKind::Glass => 3.0,
            ParticleKind::Stone => 99.0,
        }
    }

    pub fn is_static(self) -> bool {
        matches!(
            self,
            ParticleKind::Stone | ParticleKind::Wood | ParticleKind::Glass
        )
    }

    pub fn update_period(self) -> u64 {
        match self {
            ParticleKind::Lava => 4,
            _ => 2,
        }
    }

    pub fn initial_ttl(self) -> u8 {
        match self {
            ParticleKind::Fire => FIRE_TTL,
            _ => 0,
        }
    }
}

#[derive(Component, Debug, Clone, Copy)]
pub struct Particle {
    pub kind: ParticleKind,
    pub ttl: u8,
}

#[derive(Component, Debug, Clone, Copy)]
pub struct CellPos(pub IVec3);

#[derive(Resource, Debug, Clone, Copy)]
pub struct GridConfig {
    pub dims: UVec3,
    pub cell_size: f32,
    pub origin: Vec3,
}

impl Default for GridConfig {
    fn default() -> Self {
        let dims = UVec3::new(96, 96, 96);
        let cell_size = 0.25;
        let origin = Vec3::new(
            -(dims.x as f32) * cell_size * 0.5,
            0.0,
            -(dims.z as f32) * cell_size * 0.5,
        );
        Self {
            dims,
            cell_size,
            origin,
        }
    }
}

impl GridConfig {
    pub fn aabb_world(self) -> (Vec3, Vec3) {
        let min = self.origin;
        let max = self.origin
            + Vec3::new(
                self.dims.x as f32 * self.cell_size,
                self.dims.y as f32 * self.cell_size,
                self.dims.z as f32 * self.cell_size,
            );
        (min, max)
    }

    pub fn cell_center_world(self, cell: IVec3) -> Vec3 {
        self.origin
            + (Vec3::new(
                cell.x as f32 + 0.5,
                cell.y as f32 + 0.5,
                cell.z as f32 + 0.5,
            ) * self.cell_size)
    }

    pub fn world_to_cell(self, world: Vec3) -> IVec3 {
        let local = (world - self.origin) / self.cell_size;
        IVec3::new(
            local.x.floor() as i32,
            local.y.floor() as i32,
            local.z.floor() as i32,
        )
    }
}

#[derive(Resource, Debug, Clone)]
pub struct VoxelGrid {
    pub dims: UVec3,
    cells: Vec<Option<Entity>>,
}

impl VoxelGrid {
    pub fn new(dims: UVec3) -> Self {
        let len = dims.x as usize * dims.y as usize * dims.z as usize;
        Self {
            dims,
            cells: vec![None; len],
        }
    }

    pub fn clear(&mut self) {
        self.cells.fill(None);
    }

    pub fn in_bounds(&self, cell: IVec3) -> bool {
        cell.x >= 0
            && cell.y >= 0
            && cell.z >= 0
            && (cell.x as u32) < self.dims.x
            && (cell.y as u32) < self.dims.y
            && (cell.z as u32) < self.dims.z
    }

    fn idx(&self, cell: IVec3) -> Option<usize> {
        if !self.in_bounds(cell) {
            return None;
        }
        let x = cell.x as usize;
        let y = cell.y as usize;
        let z = cell.z as usize;

        let w = self.dims.x as usize;
        let d = self.dims.z as usize;
        Some((y * w * d) + (z * w) + x)
    }

    pub fn get(&self, cell: IVec3) -> Option<Entity> {
        self.idx(cell).and_then(|i| self.cells[i])
    }

    pub fn set(&mut self, cell: IVec3, entity: Option<Entity>) {
        let Some(i) = self.idx(cell) else { return };
        self.cells[i] = entity;
    }

    pub fn occupied_count(&self) -> usize {
        self.cells.iter().filter(|c| c.is_some()).count()
    }
}

impl FromWorld for VoxelGrid {
    fn from_world(world: &mut World) -> Self {
        let config = *world.resource::<GridConfig>();
        Self::new(config.dims)
    }
}

#[derive(Resource, Debug, Clone)]
pub struct ParticleMeshes {
    pub cube: Handle<Mesh>,
}

impl FromWorld for ParticleMeshes {
    fn from_world(world: &mut World) -> Self {
        let mut meshes = world.resource_mut::<Assets<Mesh>>();
        let cube = meshes.add(Cuboid::from_size(Vec3::ONE));
        Self { cube }
    }
}

#[derive(Resource, Debug, Clone)]
pub struct ParticlePalette {
    sand: Handle<StandardMaterial>,
    water: Handle<StandardMaterial>,
    stone: Handle<StandardMaterial>,
    wood: Handle<StandardMaterial>,
    oil: Handle<StandardMaterial>,
    lava: Handle<StandardMaterial>,
    smoke: Handle<StandardMaterial>,
    water_vapor: Handle<StandardMaterial>,
    fire: Handle<StandardMaterial>,
    glass: Handle<StandardMaterial>,
}

impl ParticlePalette {
    pub fn material(&self, kind: ParticleKind) -> Handle<StandardMaterial> {
        match kind {
            ParticleKind::Sand => self.sand.clone(),
            ParticleKind::Water => self.water.clone(),
            ParticleKind::Stone => self.stone.clone(),
            ParticleKind::Wood => self.wood.clone(),
            ParticleKind::Oil => self.oil.clone(),
            ParticleKind::Lava => self.lava.clone(),
            ParticleKind::Smoke => self.smoke.clone(),
            ParticleKind::WaterVapor => self.water_vapor.clone(),
            ParticleKind::Fire => self.fire.clone(),
            ParticleKind::Glass => self.glass.clone(),
        }
    }

    pub fn base_color(kind: ParticleKind) -> Color {
        match kind {
            ParticleKind::Sand => Color::srgb(0.90, 0.78, 0.33),
            ParticleKind::Water => Color::srgba(0.22, 0.45, 0.95, 0.60),
            ParticleKind::Stone => Color::srgb(0.45, 0.46, 0.50),
            ParticleKind::Wood => Color::srgb(0.52, 0.32, 0.17),
            ParticleKind::Oil => Color::srgba(0.18, 0.16, 0.05, 0.70),
            ParticleKind::Lava => Color::srgb(1.00, 0.35, 0.05),
            ParticleKind::Smoke => Color::srgba(0.60, 0.60, 0.60, 0.25),
            ParticleKind::WaterVapor => Color::srgba(0.70, 0.88, 1.00, 0.22),
            ParticleKind::Fire => Color::srgba(1.00, 0.55, 0.10, 0.80),
            ParticleKind::Glass => Color::srgba(0.60, 0.90, 1.00, 0.25),
        }
    }
}

impl FromWorld for ParticlePalette {
    fn from_world(world: &mut World) -> Self {
        let mut materials = world.resource_mut::<Assets<StandardMaterial>>();

        let sand = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Sand),
            perceptual_roughness: 1.0,
            ..default()
        });
        let water = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Water),
            alpha_mode: AlphaMode::Blend,
            perceptual_roughness: 0.35,
            ..default()
        });
        let stone = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Stone),
            perceptual_roughness: 1.0,
            ..default()
        });
        let wood = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Wood),
            perceptual_roughness: 0.85,
            ..default()
        });
        let oil = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Oil),
            alpha_mode: AlphaMode::Blend,
            perceptual_roughness: 0.55,
            ..default()
        });
        let lava = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Lava),
            emissive: LinearRgba::rgb(3.5, 1.2, 0.2),
            perceptual_roughness: 0.7,
            ..default()
        });
        let smoke = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Smoke),
            alpha_mode: AlphaMode::Blend,
            unlit: true,
            ..default()
        });
        let water_vapor = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::WaterVapor),
            alpha_mode: AlphaMode::Blend,
            unlit: true,
            ..default()
        });
        let fire = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Fire),
            alpha_mode: AlphaMode::Blend,
            emissive: LinearRgba::rgb(6.0, 2.0, 0.3),
            unlit: true,
            ..default()
        });
        let glass = materials.add(StandardMaterial {
            base_color: Self::base_color(ParticleKind::Glass),
            alpha_mode: AlphaMode::Blend,
            metallic: 0.15,
            perceptual_roughness: 0.05,
            ..default()
        });

        Self {
            sand,
            water,
            stone,
            wood,
            oil,
            lava,
            smoke,
            water_vapor,
            fire,
            glass,
        }
    }
}

#[derive(Resource, Debug, Default, Clone, Copy)]
pub struct SimTick(pub u64);

pub fn spawn_particle(
    commands: &mut Commands,
    grid: &mut VoxelGrid,
    config: GridConfig,
    meshes: &ParticleMeshes,
    palette: &ParticlePalette,
    kind: ParticleKind,
    cell: IVec3,
) -> Option<Entity> {
    if !grid.in_bounds(cell) || grid.get(cell).is_some() {
        return None;
    }

    let world_pos = config.cell_center_world(cell);
    let entity = commands
        .spawn((
            Particle {
                kind,
                ttl: kind.initial_ttl(),
            },
            CellPos(cell),
            Mesh3d(meshes.cube.clone()),
            MeshMaterial3d(palette.material(kind)),
            Transform::from_translation(world_pos).with_scale(Vec3::splat(config.cell_size)),
        ))
        .id();
    grid.set(cell, Some(entity));
    Some(entity)
}

pub fn despawn_particle_at(commands: &mut Commands, grid: &mut VoxelGrid, cell: IVec3) -> bool {
    let Some(entity) = grid.get(cell) else {
        return false;
    };
    commands.entity(entity).despawn();
    grid.set(cell, None);
    true
}

pub fn simulate(
    mut grid: ResMut<VoxelGrid>,
    config: Res<GridConfig>,
    palette: Res<ParticlePalette>,
    mut tick: ResMut<SimTick>,
    mut particles: Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
    mut moved: Local<HashSet<Entity>>,
) {
    tick.0 = tick.0.wrapping_add(1);
    moved.clear();

    let dims = grid.dims;
    let w = dims.x as i32;
    let h = dims.y as i32;
    let d = dims.z as i32;

    // Use a hashed tick so update throttling (e.g. period=2/4) doesn't lock some materials into a
    // single scan direction and create visible drift/bias.
    let order = hash_u64(tick.0);
    let x_forward = (order & 1) == 0;
    let z_forward = (order & 2) == 0;
    let y_forward = (order & 4) == 0;

    for yi in 0..h {
        let y = if y_forward { yi } else { h - 1 - yi };
        for xi in 0..w {
            let x = if x_forward { xi } else { w - 1 - xi };
            for zi in 0..d {
                let z = if z_forward { zi } else { d - 1 - zi };
                let cell = IVec3::new(x, y, z);
                let Some(entity) = grid.get(cell) else {
                    continue;
                };
                if moved.contains(&entity) {
                    continue;
                }

                let kind = {
                    let Ok((particle, _, _, _)) = particles.get_mut(entity) else {
                        grid.set(cell, None);
                        continue;
                    };
                    particle.kind
                };
                if kind.is_static() {
                    continue;
                }
                let period = kind.update_period();
                if period > 1 && (tick.0 % period) != 0 {
                    continue;
                }

                let did_move = match kind {
                    ParticleKind::Sand => step_sand(
                        entity,
                        cell,
                        kind,
                        tick.0,
                        &mut grid,
                        *config,
                        &palette,
                        &mut particles,
                        &mut *moved,
                    ),
                    ParticleKind::Water | ParticleKind::Oil | ParticleKind::Lava => step_fluid(
                        entity,
                        cell,
                        kind,
                        tick.0,
                        &mut grid,
                        *config,
                        &palette,
                        &mut particles,
                        &mut *moved,
                    ),
                    ParticleKind::Smoke => step_smoke(
                        entity,
                        cell,
                        kind,
                        tick.0,
                        &mut grid,
                        *config,
                        &palette,
                        &mut particles,
                        &mut *moved,
                    ),
                    ParticleKind::WaterVapor => step_water_vapor(
                        entity,
                        cell,
                        kind,
                        tick.0,
                        &mut grid,
                        *config,
                        &palette,
                        &mut particles,
                        &mut *moved,
                    ),
                    ParticleKind::Fire => step_fire(
                        entity,
                        cell,
                        kind,
                        tick.0,
                        &mut grid,
                        *config,
                        &palette,
                        &mut particles,
                        &mut *moved,
                    ),
                    ParticleKind::Stone | ParticleKind::Wood | ParticleKind::Glass => false,
                };

                if did_move {
                    moved.insert(entity);
                }
            }
        }
    }
}

fn step_sand(
    entity: Entity,
    cell: IVec3,
    kind: ParticleKind,
    tick: u64,
    grid: &mut VoxelGrid,
    config: GridConfig,
    palette: &ParticlePalette,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
    moved: &mut HashSet<Entity>,
) -> bool {
    let down = IVec3::new(0, -1, 0);

    let below_is_lava = match grid.get(cell + down) {
        None => false,
        Some(below) => match particles.get_mut(below) {
            Ok((particle, _cell_pos, _transform, _mat)) => particle.kind == ParticleKind::Lava,
            Err(_) => {
                grid.set(cell + down, None);
                false
            }
        },
    };
    if below_is_lava && chance_percent((tick << 9) ^ pack_cell(cell), SAND_GLASS_NEAR_LAVA_PCT) {
        let Ok((mut particle, _cell_pos, _transform, mut mat)) = particles.get_mut(entity) else {
            grid.set(cell, None);
            return false;
        };
        particle.kind = ParticleKind::Glass;
        particle.ttl = ParticleKind::Glass.initial_ttl();
        *mat = MeshMaterial3d(palette.material(ParticleKind::Glass));
        return true;
    }

    if try_move_or_swap(
        entity,
        cell,
        cell + down,
        kind,
        grid,
        config,
        palette,
        particles,
        moved,
    ) {
        return true;
    }

    const OFFSETS: [IVec3; 8] = [
        IVec3::new(1, -1, 0),
        IVec3::new(1, -1, 1),
        IVec3::new(0, -1, 1),
        IVec3::new(-1, -1, 1),
        IVec3::new(-1, -1, 0),
        IVec3::new(-1, -1, -1),
        IVec3::new(0, -1, -1),
        IVec3::new(1, -1, -1),
    ];

    let start = (hash_u64(tick ^ pack_cell(cell)) as usize) % OFFSETS.len();
    for i in 0..OFFSETS.len() {
        let offset = OFFSETS[(start + i) % OFFSETS.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    false
}

fn step_fluid(
    entity: Entity,
    cell: IVec3,
    kind: ParticleKind,
    tick: u64,
    grid: &mut VoxelGrid,
    config: GridConfig,
    palette: &ParticlePalette,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
    moved: &mut HashSet<Entity>,
) -> bool {
    if kind == ParticleKind::Water {
        let mut near_fire = false;
        for offset in CARDINAL_NEIGHBORS {
            let Some(other) = grid.get(cell + offset) else {
                continue;
            };
            let Ok((other_particle, _cell_pos, _transform, _mat)) = particles.get_mut(other) else {
                grid.set(cell + offset, None);
                continue;
            };
            if other_particle.kind == ParticleKind::Fire {
                near_fire = true;
                break;
            }
        }

        if near_fire
            && chance_percent((tick << 10) ^ pack_cell(cell), WATER_EVAPORATE_NEAR_FIRE_PCT)
        {
            let Ok((mut particle, _cell_pos, _transform, mut mat)) = particles.get_mut(entity)
            else {
                grid.set(cell, None);
                return false;
            };
            particle.kind = ParticleKind::WaterVapor;
            particle.ttl = ParticleKind::WaterVapor.initial_ttl();
            *mat = MeshMaterial3d(palette.material(ParticleKind::WaterVapor));
            return true;
        }
    }

    if kind == ParticleKind::Lava {
        let start = (hash_u64((tick << 11) ^ pack_cell(cell)) as usize) % CARDINAL_NEIGHBORS.len();
        for i in 0..CARDINAL_NEIGHBORS.len() {
            let offset = CARDINAL_NEIGHBORS[(start + i) % CARDINAL_NEIGHBORS.len()];
            let other_cell = cell + offset;
            let Some(other) = grid.get(other_cell) else {
                continue;
            };
            if moved.contains(&other) {
                continue;
            }
            let Ok((mut other_particle, _cell_pos, _transform, mut mat)) =
                particles.get_mut(other)
            else {
                grid.set(other_cell, None);
                continue;
            };

            let new_kind = match other_particle.kind {
                ParticleKind::Oil => chance_percent(
                    (tick << 12) ^ pack_cell(other_cell),
                    LAVA_IGNITE_OIL_PCT,
                )
                .then_some(ParticleKind::Fire),
                ParticleKind::Wood => chance_percent(
                    (tick << 12) ^ pack_cell(other_cell),
                    LAVA_IGNITE_WOOD_PCT,
                )
                .then_some(ParticleKind::Fire),
                ParticleKind::Sand => chance_percent(
                    (tick << 12) ^ pack_cell(other_cell),
                    SAND_GLASS_NEAR_LAVA_PCT,
                )
                .then_some(ParticleKind::Glass),
                _ => None,
            };
            let Some(new_kind) = new_kind else {
                continue;
            };

            other_particle.kind = new_kind;
            other_particle.ttl = new_kind.initial_ttl();
            *mat = MeshMaterial3d(palette.material(new_kind));
            moved.insert(other);
            break;
        }
    }

    let down = IVec3::new(0, -1, 0);
    if try_move_or_swap(
        entity,
        cell,
        cell + down,
        kind,
        grid,
        config,
        palette,
        particles,
        moved,
    ) {
        return true;
    }

    const OFFSETS_DOWN: [IVec3; 8] = [
        IVec3::new(1, -1, 0),
        IVec3::new(1, -1, 1),
        IVec3::new(0, -1, 1),
        IVec3::new(-1, -1, 1),
        IVec3::new(-1, -1, 0),
        IVec3::new(-1, -1, -1),
        IVec3::new(0, -1, -1),
        IVec3::new(1, -1, -1),
    ];
    const OFFSETS_SIDE: [IVec3; 8] = [
        IVec3::new(1, 0, 0),
        IVec3::new(1, 0, 1),
        IVec3::new(0, 0, 1),
        IVec3::new(-1, 0, 1),
        IVec3::new(-1, 0, 0),
        IVec3::new(-1, 0, -1),
        IVec3::new(0, 0, -1),
        IVec3::new(1, 0, -1),
    ];

    let start_down = (hash_u64((tick << 1) ^ pack_cell(cell)) as usize) % OFFSETS_DOWN.len();
    for i in 0..OFFSETS_DOWN.len() {
        let offset = OFFSETS_DOWN[(start_down + i) % OFFSETS_DOWN.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    // Prevent isolated single-cell fluids from doing a random walk when resting on a flat surface.
    // Only allow sideways spreading when either:
    // - there is something above this cell (pressure), or
    // - this particle is connected to at least one same-kind neighbor, and the move keeps it connected.
    let has_above = grid.get(cell + IVec3::Y).is_some();
    if !has_above && !has_same_kind_neighbor(cell, kind, grid, particles) {
        return false;
    }

    let start_side = (hash_u64((tick << 2) ^ pack_cell(cell)) as usize) % OFFSETS_SIDE.len();
    for i in 0..OFFSETS_SIDE.len() {
        let offset = OFFSETS_SIDE[(start_side + i) % OFFSETS_SIDE.len()];
        let to = cell + offset;
        if !has_above && !would_have_same_kind_neighbor_after_move(to, cell, kind, grid, particles)
        {
            continue;
        }
        if try_move_or_swap(entity, cell, to, kind, grid, config, palette, particles, moved) {
            return true;
        }
    }

    false
}

fn step_smoke(
    entity: Entity,
    cell: IVec3,
    kind: ParticleKind,
    tick: u64,
    grid: &mut VoxelGrid,
    config: GridConfig,
    palette: &ParticlePalette,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
    moved: &mut HashSet<Entity>,
) -> bool {
    let up = IVec3::new(0, 1, 0);
    if try_move_or_swap(
        entity,
        cell,
        cell + up,
        kind,
        grid,
        config,
        palette,
        particles,
        moved,
    ) {
        return true;
    }

    const OFFSETS_UP: [IVec3; 8] = [
        IVec3::new(1, 1, 0),
        IVec3::new(1, 1, 1),
        IVec3::new(0, 1, 1),
        IVec3::new(-1, 1, 1),
        IVec3::new(-1, 1, 0),
        IVec3::new(-1, 1, -1),
        IVec3::new(0, 1, -1),
        IVec3::new(1, 1, -1),
    ];
    const OFFSETS_SIDE: [IVec3; 8] = [
        IVec3::new(1, 0, 0),
        IVec3::new(1, 0, 1),
        IVec3::new(0, 0, 1),
        IVec3::new(-1, 0, 1),
        IVec3::new(-1, 0, 0),
        IVec3::new(-1, 0, -1),
        IVec3::new(0, 0, -1),
        IVec3::new(1, 0, -1),
    ];

    let start_up = (hash_u64((tick << 3) ^ pack_cell(cell)) as usize) % OFFSETS_UP.len();
    for i in 0..OFFSETS_UP.len() {
        let offset = OFFSETS_UP[(start_up + i) % OFFSETS_UP.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    let start_side = (hash_u64((tick << 4) ^ pack_cell(cell)) as usize) % OFFSETS_SIDE.len();
    for i in 0..OFFSETS_SIDE.len() {
        let offset = OFFSETS_SIDE[(start_side + i) % OFFSETS_SIDE.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    false
}

fn step_water_vapor(
    entity: Entity,
    cell: IVec3,
    kind: ParticleKind,
    tick: u64,
    grid: &mut VoxelGrid,
    config: GridConfig,
    palette: &ParticlePalette,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
    moved: &mut HashSet<Entity>,
) -> bool {
    let mut near_heat = false;
    for offset in CARDINAL_NEIGHBORS {
        let Some(other) = grid.get(cell + offset) else {
            continue;
        };
        let Ok((other_particle, _cell_pos, _transform, _mat)) = particles.get_mut(other) else {
            grid.set(cell + offset, None);
            continue;
        };
        if matches!(other_particle.kind, ParticleKind::Fire | ParticleKind::Lava) {
            near_heat = true;
            break;
        }
    }

    let top_y = grid.dims.y as i32 - 1;
    let blocked_above = cell.y >= top_y || grid.get(cell + IVec3::Y).is_some();
    if blocked_above && !near_heat && chance_percent((tick << 13) ^ pack_cell(cell), VAPOR_CONDENSE_PCT)
    {
        let Ok((mut particle, _cell_pos, _transform, mut mat)) = particles.get_mut(entity) else {
            grid.set(cell, None);
            return false;
        };
        particle.kind = ParticleKind::Water;
        particle.ttl = ParticleKind::Water.initial_ttl();
        *mat = MeshMaterial3d(palette.material(ParticleKind::Water));
        return true;
    }

    let up = IVec3::new(0, 1, 0);
    if try_move_or_swap(
        entity,
        cell,
        cell + up,
        kind,
        grid,
        config,
        palette,
        particles,
        moved,
    ) {
        return true;
    }

    const OFFSETS_UP: [IVec3; 8] = [
        IVec3::new(1, 1, 0),
        IVec3::new(1, 1, 1),
        IVec3::new(0, 1, 1),
        IVec3::new(-1, 1, 1),
        IVec3::new(-1, 1, 0),
        IVec3::new(-1, 1, -1),
        IVec3::new(0, 1, -1),
        IVec3::new(1, 1, -1),
    ];
    const OFFSETS_SIDE: [IVec3; 8] = [
        IVec3::new(1, 0, 0),
        IVec3::new(1, 0, 1),
        IVec3::new(0, 0, 1),
        IVec3::new(-1, 0, 1),
        IVec3::new(-1, 0, 0),
        IVec3::new(-1, 0, -1),
        IVec3::new(0, 0, -1),
        IVec3::new(1, 0, -1),
    ];

    let start_up = (hash_u64((tick << 5) ^ pack_cell(cell)) as usize) % OFFSETS_UP.len();
    for i in 0..OFFSETS_UP.len() {
        let offset = OFFSETS_UP[(start_up + i) % OFFSETS_UP.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    let start_side = (hash_u64((tick << 6) ^ pack_cell(cell)) as usize) % OFFSETS_SIDE.len();
    for i in 0..OFFSETS_SIDE.len() {
        let offset = OFFSETS_SIDE[(start_side + i) % OFFSETS_SIDE.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    false
}

fn step_fire(
    entity: Entity,
    cell: IVec3,
    kind: ParticleKind,
    tick: u64,
    grid: &mut VoxelGrid,
    config: GridConfig,
    palette: &ParticlePalette,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
    moved: &mut HashSet<Entity>,
) -> bool {
    {
        let Ok((mut particle, _cell_pos, _transform, mut mat)) = particles.get_mut(entity) else {
            grid.set(cell, None);
            return false;
        };

        particle.ttl = particle.ttl.saturating_sub(1);
        if particle.ttl == 0 {
            particle.kind = ParticleKind::Smoke;
            *mat = MeshMaterial3d(palette.material(ParticleKind::Smoke));
            return true;
        }
    }

    let mut has_water_neighbor = false;
    for offset in CARDINAL_NEIGHBORS {
        let Some(other) = grid.get(cell + offset) else {
            continue;
        };
        let Ok((other_particle, _cell_pos, _transform, _mat)) = particles.get_mut(other) else {
            grid.set(cell + offset, None);
            continue;
        };
        if other_particle.kind == ParticleKind::Water {
            has_water_neighbor = true;
            break;
        }
    }
    if has_water_neighbor {
        let Ok((mut particle, _cell_pos, _transform, mut mat)) = particles.get_mut(entity) else {
            grid.set(cell, None);
            return false;
        };
        particle.kind = ParticleKind::WaterVapor;
        particle.ttl = ParticleKind::WaterVapor.initial_ttl();
        *mat = MeshMaterial3d(palette.material(ParticleKind::WaterVapor));
        return true;
    }

    let start =
        (hash_u64((tick << 14) ^ pack_cell(cell)) as usize) % CARDINAL_NEIGHBORS.len();
    for i in 0..CARDINAL_NEIGHBORS.len() {
        let offset = CARDINAL_NEIGHBORS[(start + i) % CARDINAL_NEIGHBORS.len()];
        let other_cell = cell + offset;
        let Some(other) = grid.get(other_cell) else {
            continue;
        };
        if moved.contains(&other) {
            continue;
        }
        let Ok((mut other_particle, _cell_pos, _transform, mut mat)) = particles.get_mut(other)
        else {
            grid.set(other_cell, None);
            continue;
        };

        let ignite_pct = match other_particle.kind {
            ParticleKind::Oil => FIRE_IGNITE_OIL_PCT,
            ParticleKind::Wood => FIRE_IGNITE_WOOD_PCT,
            _ => continue,
        };
        if !chance_percent((tick << 15) ^ pack_cell(other_cell), ignite_pct) {
            continue;
        }

        other_particle.kind = ParticleKind::Fire;
        other_particle.ttl = ParticleKind::Fire.initial_ttl();
        *mat = MeshMaterial3d(palette.material(ParticleKind::Fire));
        moved.insert(other);
        break;
    }

    let up = IVec3::new(0, 1, 0);
    if try_move_or_swap(
        entity,
        cell,
        cell + up,
        kind,
        grid,
        config,
        palette,
        particles,
        moved,
    ) {
        return true;
    }

    const OFFSETS_UP: [IVec3; 8] = [
        IVec3::new(1, 1, 0),
        IVec3::new(1, 1, 1),
        IVec3::new(0, 1, 1),
        IVec3::new(-1, 1, 1),
        IVec3::new(-1, 1, 0),
        IVec3::new(-1, 1, -1),
        IVec3::new(0, 1, -1),
        IVec3::new(1, 1, -1),
    ];
    const OFFSETS_SIDE: [IVec3; 8] = [
        IVec3::new(1, 0, 0),
        IVec3::new(1, 0, 1),
        IVec3::new(0, 0, 1),
        IVec3::new(-1, 0, 1),
        IVec3::new(-1, 0, 0),
        IVec3::new(-1, 0, -1),
        IVec3::new(0, 0, -1),
        IVec3::new(1, 0, -1),
    ];

    let start_up = (hash_u64((tick << 7) ^ pack_cell(cell)) as usize) % OFFSETS_UP.len();
    for i in 0..OFFSETS_UP.len() {
        let offset = OFFSETS_UP[(start_up + i) % OFFSETS_UP.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    let start_side = (hash_u64((tick << 8) ^ pack_cell(cell)) as usize) % OFFSETS_SIDE.len();
    for i in 0..OFFSETS_SIDE.len() {
        let offset = OFFSETS_SIDE[(start_side + i) % OFFSETS_SIDE.len()];
        if try_move_or_swap(
            entity,
            cell,
            cell + offset,
            kind,
            grid,
            config,
            palette,
            particles,
            moved,
        ) {
            return true;
        }
    }

    false
}

fn try_move_or_swap(
    entity: Entity,
    from: IVec3,
    to: IVec3,
    mover_kind: ParticleKind,
    grid: &mut VoxelGrid,
    config: GridConfig,
    palette: &ParticlePalette,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
    moved: &mut HashSet<Entity>,
) -> bool {
    if !grid.in_bounds(to) {
        return false;
    }

    let Some(from_entity) = grid.get(from) else {
        return false;
    };
    debug_assert_eq!(from_entity, entity);

    match grid.get(to) {
        None => {
            if let Ok((_p, mut cell_pos, mut transform, _mat)) = particles.get_mut(entity) {
                cell_pos.0 = to;
                transform.translation = config.cell_center_world(to);
                grid.set(from, None);
                grid.set(to, Some(entity));
                return true;
            }
            false
        }
        Some(other) => {
            if other == entity || moved.contains(&other) {
                return false;
            }

            let Ok([mut mover, mut target]) = particles.get_many_mut([entity, other]) else {
                if particles.get_mut(entity).is_err() {
                    grid.set(from, None);
                }
                if particles.get_mut(other).is_err() {
                    grid.set(to, None);
                }
                return false;
            };

            let other_kind = target.0.kind;

            if mover_kind == ParticleKind::Fire && other_kind == ParticleKind::Water {
                mover.0.kind = ParticleKind::WaterVapor;
                mover.0.ttl = ParticleKind::WaterVapor.initial_ttl();
                *mover.3 = MeshMaterial3d(palette.material(ParticleKind::WaterVapor));
                return true;
            }

            let mut other_kind = other_kind;
            if mover_kind == ParticleKind::Water && other_kind == ParticleKind::Fire {
                target.0.kind = ParticleKind::WaterVapor;
                target.0.ttl = ParticleKind::WaterVapor.initial_ttl();
                *target.3 = MeshMaterial3d(palette.material(ParticleKind::WaterVapor));
                other_kind = ParticleKind::WaterVapor;
            }

            if (mover_kind == ParticleKind::Water && other_kind == ParticleKind::Lava)
                || (mover_kind == ParticleKind::Lava && other_kind == ParticleKind::Water)
            {
                let vapor_y = if mover_kind == ParticleKind::Water {
                    from.y
                } else {
                    to.y
                };
                let stone_y = if mover_kind == ParticleKind::Water {
                    to.y
                } else {
                    from.y
                };

                if mover_kind == ParticleKind::Water {
                    mover.0.kind = ParticleKind::WaterVapor;
                    mover.0.ttl = ParticleKind::WaterVapor.initial_ttl();
                    *mover.3 = MeshMaterial3d(palette.material(ParticleKind::WaterVapor));

                    target.0.kind = ParticleKind::Stone;
                    target.0.ttl = ParticleKind::Stone.initial_ttl();
                    *target.3 = MeshMaterial3d(palette.material(ParticleKind::Stone));
                } else {
                    mover.0.kind = ParticleKind::Stone;
                    mover.0.ttl = ParticleKind::Stone.initial_ttl();
                    *mover.3 = MeshMaterial3d(palette.material(ParticleKind::Stone));

                    target.0.kind = ParticleKind::WaterVapor;
                    target.0.ttl = ParticleKind::WaterVapor.initial_ttl();
                    *target.3 = MeshMaterial3d(palette.material(ParticleKind::WaterVapor));
                }

                if vapor_y < stone_y {
                    mover.1.0 = to;
                    mover.2.translation = config.cell_center_world(to);
                    target.1.0 = from;
                    target.2.translation = config.cell_center_world(from);
                    grid.set(from, Some(other));
                    grid.set(to, Some(entity));
                }

                moved.insert(other);
                return true;
            }

            if other_kind.is_static() {
                return false;
            }
            if mover_kind.density() <= other_kind.density() {
                return false;
            }

            mover.1.0 = to;
            mover.2.translation = config.cell_center_world(to);
            target.1.0 = from;
            target.2.translation = config.cell_center_world(from);
            grid.set(from, Some(other));
            grid.set(to, Some(entity));
            moved.insert(other);
            true
        }
    }
}

const CONNECTED_NEIGHBORS: [IVec3; 10] = [
    IVec3::new(1, 0, 0),
    IVec3::new(-1, 0, 0),
    IVec3::new(0, 0, 1),
    IVec3::new(0, 0, -1),
    IVec3::new(1, 0, 1),
    IVec3::new(1, 0, -1),
    IVec3::new(-1, 0, 1),
    IVec3::new(-1, 0, -1),
    IVec3::new(0, 1, 0),
    IVec3::new(0, -1, 0),
];

fn has_same_kind_neighbor(
    cell: IVec3,
    kind: ParticleKind,
    grid: &VoxelGrid,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
) -> bool {
    for offset in CONNECTED_NEIGHBORS {
        let Some(entity) = grid.get(cell + offset) else {
            continue;
        };
        let Ok((particle, _cell_pos, _transform, _mat)) = particles.get_mut(entity) else {
            continue;
        };
        if particle.kind == kind {
            return true;
        }
    }
    false
}

fn would_have_same_kind_neighbor_after_move(
    to: IVec3,
    from: IVec3,
    kind: ParticleKind,
    grid: &VoxelGrid,
    particles: &mut Query<(
        &mut Particle,
        &mut CellPos,
        &mut Transform,
        &mut MeshMaterial3d<StandardMaterial>,
    )>,
) -> bool {
    for offset in CONNECTED_NEIGHBORS {
        let neighbor = to + offset;
        if neighbor == from {
            continue;
        }
        let Some(entity) = grid.get(neighbor) else {
            continue;
        };
        let Ok((particle, _cell_pos, _transform, _mat)) = particles.get_mut(entity) else {
            continue;
        };
        if particle.kind == kind {
            return true;
        }
    }
    false
}

fn chance_percent(seed: u64, percent: u64) -> bool {
    percent > 0 && (hash_u64(seed) % 100) < percent
}

fn pack_cell(cell: IVec3) -> u64 {
    let x = cell.x as u64 & 0xFFFF;
    let y = cell.y as u64 & 0xFFFF;
    let z = cell.z as u64 & 0xFFFF;
    x | (y << 16) | (z << 32)
}

fn hash_u64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E3779B97F4A7C15);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
    x ^ (x >> 31)
}
