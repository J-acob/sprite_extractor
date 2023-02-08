use clap::{Parser};
use serde::*;
use std::{error::Error, fmt};
use std::fs::File;
use std::io::BufReader;
pub type Tuple<T> = (T, T);
pub type NestedTuple<T> = (Tuple<T>, Tuple<T>);
use std::{collections::HashMap, path::{PathBuf, Path}};
use image::{io::Reader as ImageReader, DynamicImage, ImageResult, imageops};
use std::ffi::OsStr;
use std::str::FromStr;

/// Module for serde containing functions to parse custom formatted string values in the form of {{n1, n2}, {n3, n4}}.
/// n1..n4 are numbers
pub mod nested_tuple_format {
    
    use crate::NestedTuple;
    use crate::Tuple;

    use serde::{self, Deserialize, Serializer, Deserializer};

    pub fn serialize<S>(
        tuple: &NestedTuple<u32>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = format!("{{{{{},{}}},{{{},{}}}}}", tuple.0.0, tuple.0.1, tuple.1.0, tuple.1.1);
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D>(
        deserializer: D,
    ) -> Result<NestedTuple<u32>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;

        let v: Vec<Tuple<u32>> = 
            s
            .split(['{','}'])
            .filter(|c| *c != "" )
            .filter(|c| *c != ",")
            .map(|s| s.split(",").collect::<Vec<_>>())
            .map(|t| (t[0].parse::<u32>().unwrap(), t[1].parse::<u32>().unwrap()))
            .collect();

        Ok((v[0], v[1]))
    }

}

pub mod tuple_format {
    use crate::Tuple;
    use serde::{self, Deserialize, Serializer, Deserializer};

    pub fn serialize<S>(
        tuple: &Tuple<u32>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = format!("{{{},{}}}", tuple.0, tuple.1);
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D>(
        deserializer: D,
    ) -> Result<Tuple<u32>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;

        let v: Vec<Tuple<u32>> = 
            s
            .split(['{','}'])
            .filter(|c| *c != "" )
            .filter(|c| *c != ",")
            .map(|s| s.split(",").collect::<Vec<_>>())
            .map(|t| (t[0].parse::<u32>().unwrap(), t[1].parse::<u32>().unwrap()))
            .collect();

        Ok(v[0])
    }

}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
/// This struct represents the data regarding an individual frame's construction
pub struct Frame {
    #[serde(with="nested_tuple_format")]
    pub frame: NestedTuple<u32>,
    #[serde(with="tuple_format")]
    pub offset: Tuple<u32>,
    pub rotated: bool,
    #[serde(with="nested_tuple_format")]
    pub source_color_rect: NestedTuple<u32>,
    #[serde(with="tuple_format")]
    pub source_size: (u32, u32)
}

#[derive(Serialize, Deserialize, Debug)]
/// This struct represents the `metadata` section which occurs at the end of a file containing frame data
#[serde(rename_all = "camelCase")]
pub struct Metadata {
    pub format: i32,
    #[serde(with="tuple_format")]
    pub size: Tuple<u32>,
    pub texture_file_name: String,   
}

#[derive(Deserialize, Debug)]
/// This struct represents the "Frame Data" contained in a file
pub struct FrameData {
    pub frames: HashMap<String, Frame>,
    pub metadata: Metadata,
    #[serde(skip)]
    output_directory: Option<PathBuf>,
    #[serde(skip)]
    image_file_path: Option<PathBuf>,
    #[serde(skip)]
    image_data: Option<DynamicImage>,
}   

impl FrameData {
    /// Returns an (empty) default `FrameDataBuilder` struct
    pub fn builder() -> FrameDataBuilder {
        FrameDataBuilder::default()
    }

    fn setup_output_directory(mut self, output_directory: &PathBuf) -> FrameData {
        self.output_directory = Some(output_directory.to_path_buf());
        self
    }

    fn load_image_path(mut self, image_path: &PathBuf) -> FrameData {
        self.image_file_path = Some(image_path.to_path_buf());
        self
    }

    fn load_image_data(mut self) -> FrameData {
        
        if let Some(i) = &self.image_file_path {
            if let Ok(d) = get_image(i) {
                self.image_data = Some(d);
            }
        }
        self
    }

    /// Attempts to save the frame data
    pub fn save(self)  {
        match self.output_directory {
            Some(ref d) => {
                match d.is_dir() {
                    true => {
                        // Handle directory already existing;

                    },
                    false => {  
                        // Handle directory not existing (safe to create it!)
                        println!("Attempting to create dir at: {:?}", d);
                        std::fs::create_dir_all(d).unwrap();
                        
                        match self.image_data {
                            Some(mut image) => {

                                for (name, data) in self.frames.iter() {
                                    let (x, y) = data.frame.0;
                                    let (w, h) = data.frame.1;
                        
                                    let subimg 
                                        = imageops::crop(&mut image, x, y, w, h);

                                    let mut fp = PathBuf::from(d);
                                    fp.push(name);
                                    subimg.to_image().save(fp).unwrap();
                                }
                                
                            },
                            None => {
                                println!("Image data doesn't exist!")
                            }
                        }
                    }
                }
            },
            None => {
                // Handle a "None" value
            }
        }
    }
}

#[derive(Debug)]
/// A struct that indicates something went wrong with files being processed
pub struct FileProcessingError;

/// Placeholder error for generically returning an error from file processing.
impl Error for FileProcessingError {}

impl fmt::Display for FileProcessingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "There was a problem converting the file.")
    }
}

#[derive(Debug, PartialEq)]
/// The currently supported frame data formats
pub enum SupportedFrameDataFormat {
    Plist,
    Json,
}

/// Allows file extensions to be converted to the `SupportedFrameDataFormat` Enum.
impl FromStr for SupportedFrameDataFormat {
    type Err = FileProcessingError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "plist" | ".plist" => Ok(SupportedFrameDataFormat::Plist),
            "json" | ".json" => Ok(SupportedFrameDataFormat::Json),
            _ => Err(FileProcessingError),
        }
    }
}

/// This struct acts as the builder for the `FrameData` struct
#[derive(Default, Debug)]
pub struct FrameDataBuilder {
    image_path: PathBuf,
    frame_data_path: PathBuf,
    output_path: PathBuf,
    frame_data_format: Option<SupportedFrameDataFormat>,
    image_extension: String,
    frame_data_extension: String,
}

fn get_frame_data_from_file_path(s: SupportedFrameDataFormat, path: PathBuf) -> Result<FrameData, Box<dyn std::error::Error>> {
    match s {
        SupportedFrameDataFormat::Plist => plist::from_file(path).or_else(|err| Err(Box::new(err) as Box<dyn std::error::Error>)),
        SupportedFrameDataFormat::Json => {
            let json_file = File::open(path)?;
            let reader = BufReader::new(json_file);
            serde_json::from_reader(reader)
            .or_else(|err| Err(Box::new(err) as Box<dyn std::error::Error>))
        },
    }
}

impl FrameDataBuilder {
    pub fn new() -> FrameDataBuilder {
        FrameDataBuilder::default()
    }

    pub fn from_args(self, a: Args) -> Result<FrameDataBuilder, Box<dyn std::error::Error>> {
        self
            .image_path_from_optional(a.image)
            .unwrap()
            .frame_data_path_from_optional(a.frame_data)
            .unwrap()
            .output_path_from_optional(a.output)
    }

    fn image_path_from_optional(mut self, i: Option<PathBuf>) -> Result<FrameDataBuilder, Box<dyn std::error::Error>> {
        self.image_path = i.unwrap();
        Ok(self)
    }

    fn frame_data_path_from_optional(mut self, f: Option<PathBuf>) -> Result<FrameDataBuilder, Box<dyn std::error::Error>> {
        self.frame_data_path = f.unwrap();
        Ok(self)
    }

    fn output_path_from_optional(mut self, o: Option<PathBuf>) -> Result<FrameDataBuilder, Box<dyn std::error::Error>> {
        self.output_path = o.unwrap();
        Ok(self)
    }

    fn set_image_extension(mut self) -> FrameDataBuilder {
        self.image_extension = self.image_path.extension().and_then(OsStr::to_str).unwrap().to_string();
        self
    }
    
    fn set_frame_data_extension(mut self) -> FrameDataBuilder {
        self.frame_data_extension = self.frame_data_path.extension().and_then(OsStr::to_str).unwrap().to_string();
        self
    }

    fn load_supported_frame_data_format(mut self) -> FrameDataBuilder {
        self.frame_data_format = Some(SupportedFrameDataFormat::from_str(&self.frame_data_extension).unwrap());
        self
    }

    fn frame_data(self) -> Result<FrameData, Box<dyn std::error::Error>> {
        get_frame_data_from_file_path(self.frame_data_format.unwrap(), self.frame_data_path)
    }

    pub fn build(self) -> FrameData {
        let od = self.output_path.clone();
        let ip = self.image_path.clone();

        self
        .set_image_extension()
        .set_frame_data_extension()
        .load_supported_frame_data_format()
        .frame_data()
        .unwrap()
        .load_image_path(&ip)
        .load_image_data()
        .setup_output_directory(&od)
    }
}


/// Gets an image from a given path and decodes, returns an `ImageResult<DynamicImage>`
pub fn get_image<P>(image_path: P) -> ImageResult<DynamicImage> 
    where P: AsRef<Path> 
{
    Ok(ImageReader::open(image_path)?.with_guessed_format()?.decode()?)
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)] // Read from `Cargo.toml`
pub struct Args {
    #[arg(short, long, value_name="FILE")]
    /// The input image file
    pub image: Option<PathBuf>,

    #[arg(short, long, value_name="FILE")]
    /// The frame data file (png, jpg)
    pub frame_data: Option<PathBuf>,

    #[arg(short='o', long, value_name="FOLDER")]
    /// The location to output the frames to. Will default to the cwd if not specified.
    pub output: Option<PathBuf>,

}

/// Need to actually implement tests
#[test]
fn builder_test() {
    let fd: FrameData = FrameData { 
        frames: HashMap::from([
            (String::from("frame1.png"), Frame {
                frame: ((0, 1), (0, 1)),
                offset: (0, 0),
                source_size: (0, 0),
                rotated: false,
                source_color_rect: ((0,0), (0,0))
            }),
            (String::from("frame2.png"), Frame {
                frame: ((0, 1), (0, 1)),
                offset: (0, 0),
                source_size: (0, 0),
                rotated: false,
                source_color_rect: ((0,0), (0,0))
            })
        ]), 
        metadata: Metadata { format: 2, size: (256, 256), texture_file_name: "frames.png".to_string() },
        output_directory: None,
        image_file_path: None,
        image_data: None,
    };
}