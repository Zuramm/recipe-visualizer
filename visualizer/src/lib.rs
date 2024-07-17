#![allow(dead_code)]
use serde::{Deserialize, Serialize};

pub mod layout;

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct Step {
    pub time_s: usize,
    pub title: String,
    pub note: Option<String>,
    pub ingredients: Vec<String>,
    pub utensils: Vec<String>,
    pub requires: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Ingredient {
    pub amount: Option<String>,
    pub name: String,
    pub comment: Option<String>,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct Recipe {
    pub name: String,
    pub ingredients: Vec<Ingredient>,
    pub steps: Vec<Step>,
}

impl Recipe {
    pub fn edges(&self) -> Vec<(usize, usize)> {
        self.steps
            .iter()
            .enumerate()
            .flat_map(|(i, step)| step.requires.iter().copied().map(move |j| (i, j)))
            .collect()
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
// }
