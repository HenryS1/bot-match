use serde::{Deserialize, Serialize};
use serde_json::Deserializer;
use serde;
use std::io;

#[derive(Serialize, Deserialize, Debug)]
struct Coord {
    x: i32,
    y: i32,
}

#[derive(Serialize, Deserialize, Debug)]
struct Player {
    money: i32,
    base: Coord,
    health: i32,
}

#[derive(Serialize, Deserialize, Debug)]
struct MapEntry {
    r#type: Box<str>,
    position: Coord,
    team: Option<Box<str>>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all="kebab-case")]
struct State {
    you: Box<str>,
    map: Vec<MapEntry>,
    turns_remaining: i32,
    player1: Player,
    player2: Player,
}

fn compute_move(state: State) -> std::string::String {
    let my_player = if "player1" == state.you.as_ref() {
        &state.player1
    } else {
        &state.player2
    };
    let other_player = if "player1" == state.you.as_ref() {
        &state.player2
    } else {
        &state.player1
    };
    if my_player.money >= 10 {
        let my_base_coord = &my_player.base;
        let enemy_base_coord = &other_player.base;
        format!(
            "Build Scout ({}, {}) ({}, {}) Down",
            my_base_coord.x,
            my_base_coord.y + 1,
            enemy_base_coord.x,
            enemy_base_coord.y,
        )
    } else {
        "No-op".to_string()
    }
}

fn main() {
    println!("Ready");
    let stdin = io::stdin();
    let all_states = Deserializer::from_reader(stdin.lock()).into_iter::<State>();
    for result in all_states {
        match result {
            Ok(state) => println!("{}", compute_move(state)),
            Err(error) => {
                eprintln!("{}", error);
                println!("No-op");
            }
        }
    }
}
