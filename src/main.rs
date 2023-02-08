use sprite_extractor::{FrameDataBuilder};
use clap::{Parser};
use sprite_extractor::Args;
use std::{io, thread};
use std::fs::{self, DirEntry};
use std::path::Path;
use itertools::Itertools;
use std::ffi::OsString;
use std::path::PathBuf;

fn vec_to_tuples(v: &Vec<DirEntry>) -> Vec<OsString> {

    v
    .iter()
    .map(|e| e.path().file_stem().unwrap().to_os_string())
    .duplicates()
    .collect()
}


fn main() {

    let resource_path = Path::new("../duelyst/app/resources/units/");
    let paths: Vec<DirEntry> = fs::read_dir(resource_path).unwrap().map(|p| p.unwrap()).collect();

    let mut t = vec_to_tuples(&paths);

     
    let mut handles = vec![];

    for p in t.iter_mut() {

        //println!("Path: {:?}", p);

        //let ao = OsString::from("hello/");
        //println!("Join path: {:?}", resource_path.to_path_buf().join("hello/").join("hello"));

        let fp = PathBuf::from(resource_path.to_path_buf().join(p.to_str().unwrap()).with_extension("plist"));
        let ip = PathBuf::from(resource_path.to_path_buf().join(p.to_str().unwrap()).with_extension("png"));

        p.push("/"); //Allow things to be pushed to the end as directories
        let op = PathBuf::from(PathBuf::from("/tests/data/").join(p.to_str().unwrap()).join("output/"));

        let a = Args {
            frame_data: Some(fp),
            image: Some(ip),
            output: Some(op),
        };
         
        let handle = thread::spawn(|| {
            if let Ok(fdb) = FrameDataBuilder::new().from_args(a) {
                let fd = fdb.build();
        
    
                fd.save();
            }
        });
        
        handles.push(handle);
    }

    
    for handle in handles {
        handle.join().unwrap();
    }
    //visit_dirs(Path::new(&resource_path), &|e: &DirEntry| println!("File: {:?}", e.file_name())).unwrap();
    /* 
    let args = Args::parse();
    if let Ok(fdb) = FrameDataBuilder::new().from_args(args) {
        let fd = fdb.build();

        fd.save();
    }
    */
}

// one possible implementation of walking a directory only visiting files
fn visit_dirs(dir: &Path, cb: &dyn Fn(&DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}