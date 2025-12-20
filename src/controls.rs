use bevy::input::mouse::{AccumulatedMouseMotion, AccumulatedMouseScroll};
use bevy::prelude::*;
use bevy::window::{CursorGrabMode, CursorOptions, PrimaryWindow};

use crate::sim::{
    GridConfig, Particle, ParticleKind, ParticleMeshes, ParticlePalette, VoxelGrid,
    despawn_particle_at, spawn_particle,
};

#[derive(Component)]
pub struct MainCamera;

#[derive(Component)]
pub struct PreviewCube;

#[derive(Resource, Debug, Clone)]
pub struct PreviewMaterial(pub Handle<StandardMaterial>);

#[derive(Component, Debug, Clone, Copy)]
pub struct FlyCam {
    pub yaw: f32,
    pub pitch: f32,
    pub speed: f32,
    pub sensitivity: f32,
}

#[derive(Resource, Debug, Clone, Copy)]
pub struct CursorLocked(pub bool);

impl Default for CursorLocked {
    fn default() -> Self {
        Self(false)
    }
}

#[derive(Resource, Debug, Clone, Copy)]
pub struct SelectedKind {
    pub kind: ParticleKind,
}

impl Default for SelectedKind {
    fn default() -> Self {
        Self {
            kind: ParticleKind::Sand,
        }
    }
}

#[derive(Resource, Debug, Clone, Copy)]
pub struct Brush {
    pub radius: i32,
}

impl Default for Brush {
    fn default() -> Self {
        Self { radius: 1 }
    }
}

#[derive(Resource, Debug, Default, Clone, Copy)]
pub struct AimState {
    pub hit: Option<IVec3>,
    pub place: Option<IVec3>,
}

#[derive(Resource, Debug, Clone, Copy)]
pub struct PaintCooldown {
    pub seconds: f32,
}

impl Default for PaintCooldown {
    fn default() -> Self {
        Self { seconds: 0.0 }
    }
}

pub fn select_kind(keys: Res<ButtonInput<KeyCode>>, mut selected: ResMut<SelectedKind>) {
    let mappings = [
        (KeyCode::Digit1, ParticleKind::Sand),
        (KeyCode::Digit2, ParticleKind::Water),
        (KeyCode::Digit3, ParticleKind::Stone),
        (KeyCode::Digit4, ParticleKind::Wood),
        (KeyCode::Digit5, ParticleKind::Oil),
        (KeyCode::Digit6, ParticleKind::Lava),
        (KeyCode::Digit7, ParticleKind::Smoke),
        (KeyCode::Digit8, ParticleKind::Glass),
        (KeyCode::Digit9, ParticleKind::WaterVapor),
        (KeyCode::Digit0, ParticleKind::Fire),
    ];

    for (key, kind) in mappings {
        if keys.just_pressed(key) {
            selected.kind = kind;
        }
    }
}

pub fn cursor_lock_toggle(keys: Res<ButtonInput<KeyCode>>, mut locked: ResMut<CursorLocked>) {
    if keys.just_pressed(KeyCode::Tab) {
        locked.0 = !locked.0;
    }
    if keys.just_pressed(KeyCode::Escape) {
        locked.0 = false;
    }
}

pub fn apply_cursor_lock(
    locked: Res<CursorLocked>,
    mut cursor_options: Query<&mut CursorOptions, With<PrimaryWindow>>,
) {
    if !locked.is_changed() {
        return;
    }

    let Ok(mut options) = cursor_options.single_mut() else {
        return;
    };

    if locked.0 {
        options.visible = false;
        options.grab_mode = CursorGrabMode::Locked;
    } else {
        options.visible = true;
        options.grab_mode = CursorGrabMode::None;
    }
}

pub fn adjust_brush(scroll: Res<AccumulatedMouseScroll>, mut brush: ResMut<Brush>) {
    let delta = scroll.delta.y;
    if delta.abs() < f32::EPSILON {
        return;
    }
    let step = delta.signum() as i32;
    brush.radius = (brush.radius + step).clamp(0, 5);
}

pub fn fly_camera(
    time: Res<Time>,
    keys: Res<ButtonInput<KeyCode>>,
    mouse: Res<AccumulatedMouseMotion>,
    locked: Res<CursorLocked>,
    mut cameras: Query<(&mut Transform, &mut FlyCam), With<MainCamera>>,
) {
    let Ok((mut transform, mut cam)) = cameras.single_mut() else {
        return;
    };

    if locked.0 {
        cam.yaw -= mouse.delta.x * cam.sensitivity;
        cam.pitch -= mouse.delta.y * cam.sensitivity;
        cam.pitch = cam.pitch.clamp(-1.54, 1.54);
    }

    transform.rotation = Quat::from_rotation_y(cam.yaw) * Quat::from_rotation_x(cam.pitch);

    let mut move_dir = Vec3::ZERO;
    let forward = transform.rotation * Vec3::NEG_Z;
    let right = transform.rotation * Vec3::X;
    let up = Vec3::Y;

    if keys.pressed(KeyCode::KeyW) {
        move_dir += forward;
    }
    if keys.pressed(KeyCode::KeyS) {
        move_dir -= forward;
    }
    if keys.pressed(KeyCode::KeyD) {
        move_dir += right;
    }
    if keys.pressed(KeyCode::KeyA) {
        move_dir -= right;
    }
    if keys.pressed(KeyCode::Space) {
        move_dir += up;
    }
    if keys.pressed(KeyCode::ShiftLeft) || keys.pressed(KeyCode::ShiftRight) {
        move_dir -= up;
    }

    let mut speed = cam.speed;
    if keys.pressed(KeyCode::ControlLeft) || keys.pressed(KeyCode::ControlRight) {
        speed *= 3.0;
    }

    if move_dir.length_squared() > 0.0 {
        transform.translation += move_dir.normalize() * speed * time.delta_secs();
    }
}

pub fn update_aim(
    windows: Query<&Window, With<PrimaryWindow>>,
    cameras: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
    locked: Res<CursorLocked>,
    config: Res<GridConfig>,
    grid: Res<VoxelGrid>,
    mut aim: ResMut<AimState>,
) {
    let Ok(window) = windows.single() else {
        aim.hit = None;
        aim.place = None;
        return;
    };
    let Ok((camera, cam_transform)) = cameras.single() else {
        aim.hit = None;
        aim.place = None;
        return;
    };

    let viewport_pos: Option<Vec2> = if locked.0 {
        Some(window.size() * 0.5)
    } else {
        window.cursor_position()
    };

    let Some(viewport_pos) = viewport_pos else {
        aim.hit = None;
        aim.place = None;
        return;
    };

    let Ok(ray) = camera.viewport_to_world(cam_transform, viewport_pos) else {
        aim.hit = None;
        aim.place = None;
        return;
    };

    let (hit, last_empty) = raycast_voxels(&grid, *config, ray.origin, ray.direction.as_vec3());
    aim.hit = hit;
    aim.place = last_empty;
}

pub fn handle_painting(
    mut commands: Commands,
    time: Res<Time>,
    mouse: Res<ButtonInput<MouseButton>>,
    keys: Res<ButtonInput<KeyCode>>,
    selected: Res<SelectedKind>,
    brush: Res<Brush>,
    aim: Res<AimState>,
    meshes: Res<ParticleMeshes>,
    palette: Res<ParticlePalette>,
    config: Res<GridConfig>,
    mut grid: ResMut<VoxelGrid>,
    mut cooldown: ResMut<PaintCooldown>,
) {
    if keys.pressed(KeyCode::AltLeft) || keys.pressed(KeyCode::AltRight) {
        return;
    }

    cooldown.seconds = (cooldown.seconds - time.delta_secs()).max(0.0);

    let wants_place = mouse.pressed(MouseButton::Left);
    let wants_erase = mouse.pressed(MouseButton::Right);
    if !wants_place && !wants_erase {
        return;
    }
    if cooldown.seconds > 0.0 {
        return;
    }

    let radius = brush.radius;
    let target = if wants_place { aim.place } else { aim.hit };
    let Some(target) = target else {
        cooldown.seconds = 0.03;
        return;
    };

    let r2 = radius * radius;
    for dy in -radius..=radius {
        for dx in -radius..=radius {
            for dz in -radius..=radius {
                if dx * dx + dy * dy + dz * dz > r2 {
                    continue;
                }
                let cell = target + IVec3::new(dx, dy, dz);
                if wants_place {
                    let _ = spawn_particle(
                        &mut commands,
                        &mut grid,
                        *config,
                        &meshes,
                        &palette,
                        selected.kind,
                        cell,
                    );
                } else {
                    let _ = despawn_particle_at(&mut commands, &mut grid, cell);
                }
            }
        }
    }

    cooldown.seconds = 0.03;
}

pub fn update_preview(
    aim: Res<AimState>,
    selected: Res<SelectedKind>,
    config: Res<GridConfig>,
    preview_material: Res<PreviewMaterial>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut preview: Query<(&mut Transform, &mut Visibility), With<PreviewCube>>,
) {
    let Ok((mut transform, mut vis)) = preview.single_mut() else {
        return;
    };

    if selected.is_changed() {
        if let Some(mat) = materials.get_mut(&preview_material.0) {
            mat.base_color = ParticlePalette::base_color(selected.kind).with_alpha(0.35);
        }
    }

    if let Some(cell) = aim.place {
        transform.translation = config.cell_center_world(cell);
        *vis = Visibility::Visible;
    } else {
        *vis = Visibility::Hidden;
    }
}

pub fn clear_sim(
    mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    particles: Query<Entity, With<Particle>>,
    mut grid: ResMut<VoxelGrid>,
) {
    if !keys.just_pressed(KeyCode::KeyR) {
        return;
    }

    for entity in &particles {
        commands.entity(entity).despawn();
    }
    grid.clear();
}

fn raycast_voxels(
    grid: &VoxelGrid,
    config: GridConfig,
    origin: Vec3,
    dir: Vec3,
) -> (Option<IVec3>, Option<IVec3>) {
    let (min, max) = config.aabb_world();
    let Some((mut t, t_exit)) = ray_aabb(origin, dir, min, max) else {
        return (None, None);
    };

    t = t.max(0.0);
    let start = origin + dir * (t + 1e-4);
    let mut cell = config.world_to_cell(start);
    if !grid.in_bounds(cell) {
        return (None, None);
    }

    let step_x = dir.x.signum() as i32;
    let step_y = dir.y.signum() as i32;
    let step_z = dir.z.signum() as i32;

    let t_delta_x = if step_x != 0 {
        config.cell_size / dir.x.abs()
    } else {
        f32::INFINITY
    };
    let t_delta_y = if step_y != 0 {
        config.cell_size / dir.y.abs()
    } else {
        f32::INFINITY
    };
    let t_delta_z = if step_z != 0 {
        config.cell_size / dir.z.abs()
    } else {
        f32::INFINITY
    };

    let next_boundary_x = if step_x > 0 {
        config.origin.x + (cell.x as f32 + 1.0) * config.cell_size
    } else {
        config.origin.x + (cell.x as f32) * config.cell_size
    };
    let next_boundary_y = if step_y > 0 {
        config.origin.y + (cell.y as f32 + 1.0) * config.cell_size
    } else {
        config.origin.y + (cell.y as f32) * config.cell_size
    };
    let next_boundary_z = if step_z > 0 {
        config.origin.z + (cell.z as f32 + 1.0) * config.cell_size
    } else {
        config.origin.z + (cell.z as f32) * config.cell_size
    };

    let mut t_max_x = if step_x != 0 {
        (next_boundary_x - origin.x) / dir.x
    } else {
        f32::INFINITY
    };
    let mut t_max_y = if step_y != 0 {
        (next_boundary_y - origin.y) / dir.y
    } else {
        f32::INFINITY
    };
    let mut t_max_z = if step_z != 0 {
        (next_boundary_z - origin.z) / dir.z
    } else {
        f32::INFINITY
    };

    let mut last_empty = None;
    while t <= t_exit && grid.in_bounds(cell) {
        if grid.get(cell).is_some() {
            return (Some(cell), last_empty);
        }
        last_empty = Some(cell);

        if t_max_x < t_max_y && t_max_x < t_max_z {
            cell.x += step_x;
            t = t_max_x;
            t_max_x += t_delta_x;
        } else if t_max_y < t_max_z {
            cell.y += step_y;
            t = t_max_y;
            t_max_y += t_delta_y;
        } else {
            cell.z += step_z;
            t = t_max_z;
            t_max_z += t_delta_z;
        }
    }

    (None, last_empty)
}

fn ray_aabb(origin: Vec3, dir: Vec3, min: Vec3, max: Vec3) -> Option<(f32, f32)> {
    let mut t_min = -f32::INFINITY;
    let mut t_max = f32::INFINITY;

    for axis in 0..3 {
        let (o, d, mn, mx) = match axis {
            0 => (origin.x, dir.x, min.x, max.x),
            1 => (origin.y, dir.y, min.y, max.y),
            _ => (origin.z, dir.z, min.z, max.z),
        };

        if d.abs() < 1e-8 {
            if o < mn || o > mx {
                return None;
            }
            continue;
        }

        let inv = 1.0 / d;
        let mut t1 = (mn - o) * inv;
        let mut t2 = (mx - o) * inv;
        if t1 > t2 {
            std::mem::swap(&mut t1, &mut t2);
        }
        t_min = t_min.max(t1);
        t_max = t_max.min(t2);
        if t_max < t_min {
            return None;
        }
    }

    Some((t_min, t_max))
}
