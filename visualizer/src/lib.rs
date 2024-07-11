#[derive(Debug, Default, Clone, PartialEq)]
struct Step {
    time_s: usize,
    title: String,
    note: Option<String>,
    ingredients: Vec<String>,
    utensils: Vec<String>,
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
