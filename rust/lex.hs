use std::collections::HashSet;
use std::collections::VecDeque;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;

pub struct Lexer {
    token_buffer: VecDeque<String>,
    imported_files: HashSet<PathBuf>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            token_buffer: VecDeque::new(),
            imported_files: Vec::new(),
        }
    }

    fn import_file<P: AsRef<Path>>(&mut self, path: P) {
        let path = path.as_ref().canonicalize().unwrap(); 

        if self.imported_files.contains(&path) {
            return
        }

        let mut file = File::open(path.clone()).unwrap();

        let mut contents = String::new();

        file.read_to_string(&mut contents).unwrap();

        let mut token_buffer = contents
            .split_whitespace()
            .map(String::from)
            .collect::<VecDeque<_>>();

        token_buffer.extend(self.token_buffer.clone());

        self.token_buffer = token_buffer;

        self.imported_files.insert(path);
    }
}

impl Iterator for Lexer {
    type Item = String;

    fn next(&mut self) -> Option<String> {
        let token = self.token_buffer.pop_front()?;
        let token = token.as_str(); 

        match token {
            "$(" => loop {
                let token = self.token_buffer.pop_front()?;

                if token == "$)" {
                    break self.token_buffer.pop_front();
                }
            },
            "$[" => {
                let path = self.token_buffer.pop_front().unwrap();

                assert!(self.token_buffer.pop_front().unwrap() == "$]");

                self.import_file(path);

                self.token_buffer.pop_front()
            },
            _ => Some(token.to_owned()),
        }
    }
}

fn main() {
    let mut lexer = Lexer::new();

    lexer.import_file("set.mm");

    for token in lexer {
        println!("{}", token);
    }
}
