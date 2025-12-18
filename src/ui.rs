use bevy::prelude::*;

use crate::controls::{Brush, CursorLocked, SelectedKind};
use crate::sim::VoxelGrid;

#[derive(Component)]
pub(crate) struct HudText;

#[derive(Component)]
pub(crate) struct Crosshair;

pub fn setup_hud(mut commands: Commands) {
    commands.spawn((
        HudText,
        Visibility::Visible,
        Node {
            position_type: PositionType::Absolute,
            left: Val::Px(12.0),
            top: Val::Px(12.0),
            ..default()
        },
        Text::new(""),
        TextFont {
            font_size: 14.0,
            ..default()
        },
        TextColor(Color::srgb(0.92, 0.92, 0.95)),
    ));

    commands.spawn((
        Crosshair,
        Visibility::Hidden,
        Node {
            position_type: PositionType::Absolute,
            left: Val::Percent(50.0),
            top: Val::Percent(50.0),
            margin: UiRect {
                left: Val::Px(-6.0),
                top: Val::Px(-10.0),
                ..default()
            },
            ..default()
        },
        Text::new("+"),
        TextFont {
            font_size: 22.0,
            ..default()
        },
        TextColor(Color::srgba(1.0, 1.0, 1.0, 0.7)),
    ));
}

pub fn update_hud(
    grid: Res<VoxelGrid>,
    selected: Res<SelectedKind>,
    brush: Res<Brush>,
    locked: Res<CursorLocked>,
    mut hud: Query<&mut Text, With<HudText>>,
    mut crosshair: Query<&mut Visibility, With<Crosshair>>,
) {
    let Ok(mut text) = hud.single_mut() else {
        return;
    };
    let Ok(mut crosshair_vis) = crosshair.single_mut() else {
        return;
    };

    *crosshair_vis = if locked.0 {
        Visibility::Visible
    } else {
        Visibility::Hidden
    };

    let current = selected.kind;
    let brush_radius = brush.radius;
    let lock_state = if locked.0 { "ON" } else { "OFF" };

    text.0 = format!(
        "Sandstorm 3D (Bevy 0.17)\n\
Tab: toggle mouse capture ({lock_state})\n\
WASD + Space/Shift: fly  |  Ctrl: boost\n\
LMB: place  |  RMB: erase\n\
Mouse Wheel: brush radius ({brush_radius})\n\
1 Sand  2 Water  3 Stone  4 Wood  5 Oil  6 Lava  7 Smoke  8 Glass\n\
Selected: {name}\n\
Particles: {count}\n\
R: clear",
        name = current.name(),
        count = grid.occupied_count(),
    );
}
