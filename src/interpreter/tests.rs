use std::fs;
use std::io::Read;
use std::path::{PathBuf, Path};
use bytecode::{OptimizationLevel, optimize_module};
use bytecode::test::generate_byte_code;
use super::{run_byte_code, ExecutionResult};
use super::value::Value;

pub struct Test
{
    pub name: String,
    pub ret: isize,
    pub code: String,
}

impl Test
{
    pub fn load(path: &Path) -> Test
    {
        let mut file = fs::File::open(path).unwrap();
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();

        assert!(data.starts_with("#ret:"));
        let ret: String = data.chars().skip(5).take_while(|c| c.is_numeric()).collect();
        Test{
            name: path.file_stem().unwrap().to_str().unwrap().into(),
            ret: isize::from_str_radix(&ret, 10).unwrap(),
            code: data,
        }
    }

    pub fn run(&self, dump: bool) -> ExecutionResult<isize>
    {
        let mut bc_mod = match generate_byte_code(&self.code, dump)
        {
            Ok(bc_mod) => bc_mod,
            Err(e) => return Err(format!("Compile error: {}", e)),
        };

        optimize_module(&mut bc_mod, OptimizationLevel::Normal);
        let result = run_byte_code(&bc_mod)?;

        match result
        {
            Value::Int(r) => Ok(r),
            _ => {
                Err(format!("Expecting int return type, got {}", result))
            },
        }
    }
}


fn run_test(prog: &Path, dump: bool) -> ExecutionResult<isize>
{
    let test = Test::load(prog);
    let ret = test.run(dump)?;
    if ret != test.ret {
        Err(format!("Return value doesn't match: {}, expecting {}", ret, test.ret))
    } else {
        Ok(ret)
    }
}

fn run_tests_in_directory(dir: fs::ReadDir) -> usize
{
    println!();
    println!("Running tests:");
    println!("==============");
    let mut failures = 0;
    let mut paths : Vec<PathBuf> = dir.map(|dir_entry| dir_entry.unwrap().path()).collect();
    paths.sort();

    for path in &paths {
        if path.extension().unwrap_or_default() == "nomad" {
            let r = run_test(&path, false);
            println!("{}: {:?}", path.file_stem().unwrap().to_str().unwrap(), r);
            if !r.is_ok()  {
                failures += 1;
            }
        }
    }

    failures
}

#[test]
fn test_all()
{
    let mut testcode_found = false;
    for path in &["testcode", "../testcode"] {
        if let Ok(dir) = fs::read_dir(path) {
            assert!(run_tests_in_directory(dir) == 0);
            testcode_found = true;
            break;
        }
    }

    assert!(testcode_found);
}
