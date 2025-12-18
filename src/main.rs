mod controls;
mod sim;
mod ui;

use bevy::prelude::*;
use bevy::window::{PresentMode, WindowResolution};

use crate::controls::{AimState, Brush, CursorLocked, FlyCam, PaintCooldown, PreviewCube};
use crate::sim::{
    GridConfig, ParticleKind, ParticleMeshes, ParticlePalette, SimTick, VoxelGrid, spawn_particle,
};

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::srgb(0.02, 0.02, 0.03)))
        .insert_resource(Time::<Fixed>::from_hz(60.0))
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "Sandstorm 3D (Falling Sand)".to_string(),
                resolution: WindowResolution::new(1280, 720),
                present_mode: PresentMode::AutoVsync,
                ..default()
            }),
            ..default()
        }))
        .init_resource::<GridConfig>()
        .init_resource::<VoxelGrid>()
        .init_resource::<ParticleMeshes>()
        .init_resource::<ParticlePalette>()
        .init_resource::<SimTick>()
        .init_resource::<CursorLocked>()
        .init_resource::<controls::SelectedKind>()
        .init_resource::<Brush>()
        .init_resource::<AimState>()
        .init_resource::<PaintCooldown>()
        .add_systems(Startup, setup_scene)
        .add_systems(Startup, ui::setup_hud)
        .add_systems(
            Update,
            (
                controls::select_kind,
                controls::cursor_lock_toggle,
                controls::apply_cursor_lock,
                controls::adjust_brush,
                controls::fly_camera,
                controls::update_aim,
                controls::update_preview,
                controls::handle_painting,
                controls::clear_sim,
                ui::update_hud,
            )
                .chain(),
        )
        .add_systems(FixedUpdate, sim::simulate)
        .run();
}

fn setup_scene(
    mut commands: Commands,
    mut materials: ResMut<Assets<StandardMaterial>>,
    config: Res<GridConfig>,
    mut grid: ResMut<VoxelGrid>,
    particle_meshes: Res<ParticleMeshes>,
    palette: Res<ParticlePalette>,
) {
    commands.insert_resource(AmbientLight {
        color: Color::srgb(1.0, 1.0, 1.0),
        brightness: 0.60,
        ..default()
    });

    commands.spawn((
        DirectionalLight {
            // Shadows from thousands of voxels tend to obscure the sim. Keeping this off also
            // ensures the "inside of the box" stays readable.
            shadows_enabled: false,
            illuminance: 20_000.0,
            ..default()
        },
        Transform::from_rotation(Quat::from_euler(
            EulerRot::XYZ,
            -std::f32::consts::FRAC_PI_4,
            -std::f32::consts::FRAC_PI_4,
            0.0,
        )),
    ));

    let size_x = config.dims.x as f32 * config.cell_size;
    let size_y = config.dims.y as f32 * config.cell_size;
    let size_z = config.dims.z as f32 * config.cell_size;
    let center = config.origin + Vec3::new(size_x * 0.5, size_y * 0.5, size_z * 0.5);
    let wall_thickness = config.cell_size * 0.5;

    // Interior fill lights to keep particles visible inside the container.
    let light_range = size_x.max(size_y).max(size_z) * 1.8;
    commands.spawn((
        PointLight {
            intensity: 18_000.0,
            range: light_range,
            color: Color::srgb(1.0, 0.95, 0.90),
            shadows_enabled: false,
            ..default()
        },
        Transform::from_translation(center + Vec3::new(0.0, size_y * 0.80, 0.0)),
    ));
    commands.spawn((
        PointLight {
            intensity: 8_000.0,
            range: light_range,
            color: Color::srgb(0.70, 0.80, 1.0),
            shadows_enabled: false,
            ..default()
        },
        Transform::from_translation(
            center + Vec3::new(-size_x * 0.25, size_y * 0.25, size_z * 0.25),
        ),
    ));

    let container_material = materials.add(StandardMaterial {
        base_color: Color::srgb(0.10, 0.10, 0.12),
        perceptual_roughness: 0.95,
        ..default()
    });

    // Make the "front" wall (positive Z, where the camera starts) transparent so you can see inside
    // without having to fly into the container.
    let front_wall_material = materials.add(StandardMaterial {
        base_color: Color::srgba(0.10, 0.12, 0.14, 0.08),
        alpha_mode: AlphaMode::Blend,
        cull_mode: None,
        perceptual_roughness: 0.15,
        ..default()
    });

    commands.spawn((
        Mesh3d(particle_meshes.cube.clone()),
        MeshMaterial3d(container_material.clone()),
        Transform::from_translation(Vec3::new(
            center.x,
            config.origin.y - wall_thickness * 0.5,
            center.z,
        ))
        .with_scale(Vec3::new(
            size_x + wall_thickness * 2.0,
            wall_thickness,
            size_z + wall_thickness * 2.0,
        )),
    ));

    let wall_scale_x = Vec3::new(wall_thickness, size_y, size_z + wall_thickness * 2.0);
    let wall_scale_z = Vec3::new(size_x + wall_thickness * 2.0, size_y, wall_thickness);

    commands.spawn((
        Mesh3d(particle_meshes.cube.clone()),
        MeshMaterial3d(container_material.clone()),
        Transform::from_translation(Vec3::new(
            config.origin.x - wall_thickness * 0.5,
            center.y,
            center.z,
        ))
        .with_scale(wall_scale_x),
    ));
    commands.spawn((
        Mesh3d(particle_meshes.cube.clone()),
        MeshMaterial3d(container_material.clone()),
        Transform::from_translation(Vec3::new(
            config.origin.x + size_x + wall_thickness * 0.5,
            center.y,
            center.z,
        ))
        .with_scale(wall_scale_x),
    ));
    commands.spawn((
        Mesh3d(particle_meshes.cube.clone()),
        MeshMaterial3d(container_material.clone()),
        Transform::from_translation(Vec3::new(
            center.x,
            center.y,
            config.origin.z - wall_thickness * 0.5,
        ))
        .with_scale(wall_scale_z),
    ));
    commands.spawn((
        Mesh3d(particle_meshes.cube.clone()),
        MeshMaterial3d(front_wall_material),
        Transform::from_translation(Vec3::new(
            center.x,
            center.y,
            config.origin.z + size_z + wall_thickness * 0.5,
        ))
        .with_scale(wall_scale_z),
    ));

    let preview_mat = materials.add(StandardMaterial {
        base_color: Color::srgba(1.0, 1.0, 1.0, 0.35),
        alpha_mode: AlphaMode::Blend,
        unlit: true,
        ..default()
    });
    commands.insert_resource(controls::PreviewMaterial(preview_mat.clone()));

    commands.spawn((
        PreviewCube,
        Mesh3d(particle_meshes.cube.clone()),
        MeshMaterial3d(preview_mat),
        Transform::from_translation(center).with_scale(Vec3::splat(config.cell_size)),
        Visibility::Hidden,
    ));

    let initial_yaw = 0.0;
    let initial_pitch = -0.30;
    let cam_rot = Quat::from_rotation_y(initial_yaw) * Quat::from_rotation_x(initial_pitch);
    commands.spawn((
        controls::MainCamera,
        Camera3d::default(),
        Transform::from_translation(center + Vec3::new(0.0, size_y * 0.25, size_z * 1.35))
            .with_rotation(cam_rot),
        FlyCam {
            yaw: initial_yaw,
            pitch: initial_pitch,
            speed: 10.0,
            sensitivity: 0.0025,
        },
    ));

    // Seed the world with a few piles to show all 8 materials.
    seed_demo(
        &mut commands,
        &mut grid,
        *config,
        &particle_meshes,
        &palette,
    );
}

fn seed_demo(
    commands: &mut Commands,
    grid: &mut VoxelGrid,
    config: GridConfig,
    meshes: &ParticleMeshes,
    palette: &ParticlePalette,
) {
    let w = config.dims.x as i32;
    let h = config.dims.y as i32;
    let d = config.dims.z as i32;

    let clamp = |cell: IVec3| {
        IVec3::new(
            cell.x.clamp(0, w - 1),
            cell.y.clamp(0, h - 1),
            cell.z.clamp(0, d - 1),
        )
    };

    // Water pool.
    for x in 4..14 {
        for z in 4..14 {
            for y in 0..5 {
                let cell = clamp(IVec3::new(x, y, z));
                let _ = spawn_particle(
                    commands,
                    grid,
                    config,
                    meshes,
                    palette,
                    ParticleKind::Water,
                    cell,
                );
            }
        }
    }

    // Oil layer above water.
    for x in 6..12 {
        for z in 6..12 {
            for y in 5..7 {
                let cell = clamp(IVec3::new(x, y, z));
                let _ = spawn_particle(
                    commands,
                    grid,
                    config,
                    meshes,
                    palette,
                    ParticleKind::Oil,
                    cell,
                );
            }
        }
    }

    // Sand blob near the top.
    for x in (w / 2 - 4)..(w / 2 + 4) {
        for z in (d / 2 - 4)..(d / 2 + 4) {
            for y in (h - 10)..(h - 6) {
                let cell = clamp(IVec3::new(x, y, z));
                let _ = spawn_particle(
                    commands,
                    grid,
                    config,
                    meshes,
                    palette,
                    ParticleKind::Sand,
                    cell,
                );
            }
        }
    }

    // A stone pillar.
    for y in 0..12 {
        let cell = clamp(IVec3::new(w / 2 + 10, y, d / 2));
        let _ = spawn_particle(
            commands,
            grid,
            config,
            meshes,
            palette,
            ParticleKind::Stone,
            cell,
        );
    }

    // Wooden platform.
    for x in (w / 2 - 6)..(w / 2 + 6) {
        for z in (d / 2 + 10)..(d / 2 + 14) {
            let cell = clamp(IVec3::new(x, 10, z));
            let _ = spawn_particle(
                commands,
                grid,
                config,
                meshes,
                palette,
                ParticleKind::Wood,
                cell,
            );
        }
    }

    // Smoke puff.
    for x in (w / 2 - 2)..(w / 2 + 2) {
        for z in (d / 2 - 14)..(d / 2 - 10) {
            for y in 2..6 {
                let cell = clamp(IVec3::new(x, y, z));
                let _ = spawn_particle(
                    commands,
                    grid,
                    config,
                    meshes,
                    palette,
                    ParticleKind::Smoke,
                    cell,
                );
            }
        }
    }

    // Lava blob (slow).
    for x in (w / 2 + 12)..(w / 2 + 15) {
        for z in (d / 2 - 2)..(d / 2 + 2) {
            for y in (h - 8)..(h - 6) {
                let cell = clamp(IVec3::new(x, y, z));
                let _ = spawn_particle(
                    commands,
                    grid,
                    config,
                    meshes,
                    palette,
                    ParticleKind::Lava,
                    cell,
                );
            }
        }
    }

    // Glass wall segment.
    for y in 0..10 {
        for z in (d / 2 - 2)..(d / 2 + 2) {
            let cell = clamp(IVec3::new(w / 2 - 12, y, z));
            let _ = spawn_particle(
                commands,
                grid,
                config,
                meshes,
                palette,
                ParticleKind::Glass,
                cell,
            );
        }
    }
}
