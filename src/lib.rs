use clap::{Parser};
use itertools::Itertools;
use serde::*;
use image::{io::Reader as ImageReader, DynamicImage, ImageResult, imageops, SubImage, ImageBuffer, Rgb, Rgba, RgbaImage, ImageOutputFormat, GenericImage};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    fs::File,
    io::{Cursor, BufReader, BufWriter},
    io::prelude::*,
    ffi::OsStr,
    str::FromStr,
    collections::{HashMap, HashSet},
    path::{PathBuf, Path},
    ops::{Deref, DerefMut}
};

use regex::Regex;

pub type Tuple<T> = (T, T);
pub type NestedTuple<T> = (Tuple<T>, Tuple<T>);

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)] // Read from `Cargo.toml`
pub struct Args {
    #[arg(short='i', long, value_name="FILE")]
    /// The input image file
    pub image: Option<PathBuf>,

    #[arg(short='f', long, value_name="FILE")]
    /// The frame data file (png, jpg)
    pub frame_data: Option<PathBuf>,

    #[arg(short='o', long, value_name="FOLDER")]
    /// The location to output the frames to. Will default to the cwd if not specified.
    pub output: Option<PathBuf>,
}

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
    pub output_directory: Option<PathBuf>,
    #[serde(skip)]
    image_file_path: Option<PathBuf>,
    #[serde(skip)]
    image_data: Option<DynamicImage>,
}

/// Errors for when the program handles input image files
#[derive(Debug)]
pub enum FrameDataError {
    Save,
    Image,
    NonePath,
}

impl Display for FrameDataError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Image => write!(f, "Error setting image data."),
            Self::Save => write!(f, "Error saving image data."), 
            Self::NonePath => write!(f, "None path was recieved.")
        }
    }
}

impl Error for FrameDataError {}


fn extract_name_and_number(input: &str) -> Option<(String, i32)> {
    let re = Regex::new(r"(.*)_(\d+)\.png$").unwrap();
    let captures = re.captures(input)?;
    let name = captures[1].to_owned();
    let number = captures[2].parse::<i32>().ok()?;
    Some((name, number))
}

/// Exracts the name of the animation
fn extract_animation_name(input: &str) -> Option<String> {
    let re = Regex::new(r"^(.*)_\w+_\d+\.png$").unwrap();
    let captures = re.captures(input)?;
    Some(captures[1].to_owned())
}

impl FrameData {
    /// Returns an (empty) default `FrameDataBuilder` struct
    pub fn builder() -> FrameDataBuilder {
        FrameDataBuilder::default()
    }

    fn set_output_directory(mut self, output_directory: &PathBuf) -> Result<FrameData, Box<dyn std::error::Error>> {
        self.output_directory = Some(output_directory.to_path_buf());
        Ok(self)
    }

    fn set_image_path(mut self, image_path: &PathBuf) -> Result<FrameData, Box<dyn std::error::Error>> {
        self.image_file_path = Some(image_path.to_path_buf());
        Ok(self)
    }

    fn set_image_data(mut self) ->  Result<FrameData, Box<dyn std::error::Error>> {
        self
        .image_file_path
        .as_ref()
        .ok_or_else(|| Box::new(ImageFileError::InvalidImageFile) as Box<dyn std::error::Error>)
        .and_then(|e| get_image(e).or_else(|e| Err(Box::new(e) as Box<dyn std::error::Error>)))
        .and_then(|i| {
            self.image_data = Some(i);
            Ok(self)
        })
    }

    /// Returns a vector of Image buffers
    fn get_frames_as_buffer_vec(&self) -> Vec<ImageBuffer<Rgba<u8>, Vec<u8>>> {
        let mut images = vec![];
        if let Some(image) = &self.image_data {   
            for (_name, &Frame { frame: ((x, y), (w, h)), ..}) in &self.frames {
                images.push(imageops::crop_imm(image, x, y, w, h).to_image())
            }
        }
        images
    }

    /// Sort frames according to their names into vectors
    pub fn save_to_ordered_and_mapped_frames(&self) {
        let mut name_map: HashMap<String, Vec<(String, DynamicImage)>> = HashMap::new();

        let mut animation_collection_name: Option<String> = None;

        if let Some(image) = &self.image_data {

            for (frame_name, &Frame { frame: ((x, y), (w, h)), ..}) in self.frames.iter() {

                // Set the top level directory name for outputting files
                if animation_collection_name.is_none() {
                    animation_collection_name = Some(extract_animation_name(frame_name).unwrap());
                }

                if let Some((name, num)) = &extract_name_and_number(frame_name) {

                    if name_map.contains_key(name) == true {
                        if let Some(vector) = name_map.get_mut(name) {
                            vector.push((frame_name.to_string(), image.crop_imm(x, y, w, h)));
                        }
                    } else {
                        name_map.insert(name.to_string(), vec![(frame_name.to_string(), image.crop_imm(x, y, w, h))]);
                    }
                }
            }

            // Sort induvidual frames to respect numbered ordering
            for vector in name_map.values_mut() {
                vector.sort_by(|a, b| 
                
                    match (extract_name_and_number(&a.0), extract_name_and_number(&b.0)) {
                        (Some((s1, n1)), Some((s2, n2))) => {
                            n1.cmp(&n2)
                        }, 
                        _  => {
                            std::cmp::Ordering::Equal
                        }
                    }
                );
            }

            // Sort the hashmap keys by alphabetical order so we have consistent ordering
            let mut animation_vector: Vec<(&String, &Vec<(String, DynamicImage)>)> = name_map.iter().collect();
            animation_vector.sort_by(|(s1, v1), (s2, v2) |{
                s1.to_lowercase().cmp(&s2.to_lowercase())
            });

            // Calculate long image saving
            let image_buffers = self.get_frames_as_buffer_vec();
            let dimensions: Vec<(u32, u32)> 
                = image_buffers
                .iter()
                .map(|image_buffer| image_buffer.dimensions())
                .collect();
                
            let max_width = dimensions.iter().map(|(x, y)| x).max();
            let max_height = dimensions.iter().map(|(x, y)| y).max();

            let total_width: u32 = dimensions.iter().map(|(x, y)| y).sum();

            // Create new image buffer
            let mut image_output_buffer = RgbaImage::new(total_width, *max_height.unwrap());

            // Process the frame data and save it to the respective folders
            if let Some(output) = &self.output_directory {
                if let Some(anim_folder) = animation_collection_name {
                    let mut ap = output.clone();
                    ap.push(&anim_folder);
                    std::fs::create_dir_all(&ap).unwrap();

                    let mut frame_num = 0;

                    for (anim_name, frames) in animation_vector.iter() {
                        let mut p = output.clone();
                        p.push(&anim_folder);
                        p.push(&anim_name);

                        std::fs::create_dir_all(p).unwrap();

                        for (i, (frame_name, frame_data)) in frames.iter().enumerate() {
                            let mut anim_path = output.clone();
                            anim_path.push(&anim_folder);
                            anim_path.push(&anim_name);
                            anim_path.push(&frame_name);

                            imageops::replace(&mut image_output_buffer, frame_data, (max_width.unwrap() * frame_num as u32).into(), 0);

                            frame_num += 1;

                            //frame_data.save(anim_path).unwrap();
                        }
                    }

                    image_output_buffer.save(&ap.with_file_name("long.png")).ok().unwrap();
                }
            }
        }
    }

    /// Saves all the frames to one long image
    pub fn save_to_long_image(&self) {
        let image_buffers = self.get_frames_as_buffer_vec();
        let dimensions: Vec<(u32, u32)> 
            = image_buffers
            .iter()
            .map(|image_buffer| image_buffer.dimensions())
            .collect();

        let max_width = dimensions.iter().map(|(x, ..)| x).max();
        let max_height = dimensions.iter().map(|(.., y)| y).max();
        
        let total_width: u32 = dimensions.iter().map(|(x, y)| x).sum();

        // Create new image buffer
        let mut image_output_buffer = RgbaImage::new(total_width, *max_height.unwrap());

        if let Some(path) = &self.output_directory {
            std::fs::create_dir_all(path).unwrap();
            
            for (i, image) in image_buffers.iter().enumerate() {

                imageops::replace(&mut image_output_buffer, image, (100 * i as u32).into(), 0);

                //let mut save_path = PathBuf::from(path);
                //save_path.push(format!("{}.png", i.to_string()));
                //image.save_with_format(save_path, image::ImageFormat::Png).unwrap();
            }

            image_output_buffer.save(path.with_file_name("long.png")).ok().unwrap();
            //image::save_buffer(path, stream, width, height, color)
        }
    }

    /// Attempts to save the frame data
    pub fn save(self)  {
        match self.output_directory {
            Some(ref d) => {
                match d.is_dir() {
                    true => {
                        //TODO handle directory already existing
                        println!("Directory already exists!");
                    },
                    false => {  
                        // Handle directory not existing (safe to create it!)
                        println!("Attempting to create dir at: {:?}", d);
                        std::fs::create_dir_all(d).unwrap();
                        
                        match self.image_data {
                            Some(image) => {

                                for (name, data) in self.frames.iter() {
                                    let (x, y) = data.frame.0;
                                    let (w, h) = data.frame.1;
                        
                                    let subimg 
                                        = imageops::crop_imm(&image, x, y, w, h);

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


/// This struct acts as the builder for the `FrameData` struct
#[derive(Default, Debug)]
pub struct FrameDataBuilder {
    image_path: Option<PathBuf>,
    frame_data_path: Option<PathBuf>,
    output_path: Option<PathBuf>,
    frame_data_format: Option<SupportedFrameDataFormat>,
    image_data_format: Option<SupportedImageFormat>,
}


/// Errors for when the program handles input image files
#[derive(Debug)]
pub enum ImageFileError {
    InvalidImageFile,
    SupportedImageFile,
    InvalidImageExtension,
}

impl Display for ImageFileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidImageFile => write!(f, "Image file is invalid."),
            Self::SupportedImageFile => write!(f, "Image file type is not supported."),
            Self::InvalidImageExtension => write!(f, "Image file extension is invalid"),
        }
    }
}
impl Error for ImageFileError {}


/// Supported image file formats
#[derive(Debug, PartialEq)]
pub enum SupportedImageFormat {
    Png
}

impl FromStr for SupportedImageFormat {
    type Err = ImageFileError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            ".png" | "png" => Ok(SupportedImageFormat::Png),
            _ => Err(ImageFileError::SupportedImageFile)
        }
    }
}

/// Errors for when the program handles files which are expected to be of the frame data format
#[derive(Debug)] 
pub enum FrameDataFileError {
   SupportedFrameDataFile,
   NonePath,
   Conversion,
}

impl Display for FrameDataFileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
           Self::SupportedFrameDataFile => write!(f, "Frame data is not supported."),
           Self::NonePath => write!(f, "Cannot find frame data file."),
           Self::Conversion => write!(f, "There was an error when converting this file path from an OsString to a &str")
        }
    }
}
impl Error for FrameDataFileError {}

#[derive(Debug, PartialEq)]
/// The currently supported frame data formats
pub enum SupportedFrameDataFormat {
    Plist,
    Json,
}

/// Allows file extensions to be converted to the `SupportedFrameDataFormat` Enum.
impl FromStr for SupportedFrameDataFormat {
    type Err = FrameDataFileError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "plist" | ".plist" => Ok(SupportedFrameDataFormat::Plist),
            "json" | ".json" => Ok(SupportedFrameDataFormat::Json),
            _ => Err(FrameDataFileError::SupportedFrameDataFile),
        }
    }
}


/// Errors for when the program interacts with the output directory
#[derive(Debug)]
pub enum OutputDirectoryError {
    InvalidPath,
    FilesPresent,
    MissingDirectory,
}

impl Display for OutputDirectoryError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPath => write!(f, "Output directory path is invalid."),
            Self::FilesPresent => write!(f, "Cannot write to directory with files present."), 
            Self::MissingDirectory => write!(f, "Output path was None value in argument struct")
        }
    }
}
impl Error for OutputDirectoryError {}


#[derive(Debug)]
pub enum FrameDataBuilderError {
    Processing,
}

impl Display for FrameDataBuilderError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Processing => write!(f, "Error when converting to FrameData structure")
        }
    }
}
impl Error for FrameDataBuilderError {}

impl FrameDataBuilder {
    pub fn new() -> FrameDataBuilder {
        FrameDataBuilder::default()
    }
    pub fn from_args(self, a: Args) -> Result<FrameDataBuilder, Box<dyn std::error::Error>> {
        Ok(
            self
            .set_image_path(a.image)?
            .set_frame_data_path(a.frame_data)?
            .set_output_path(a.output)?
        )
    }
    fn set_image_path(mut self, i: Option<PathBuf>) -> Result<FrameDataBuilder, Box<ImageFileError>> {
        match i {
            Some(image) => {
                self.image_path = Some(image);
                Ok(self)
            }, 
            None => {
                Err(Box::new(ImageFileError::InvalidImageFile))
            }
        }
    }

    fn set_frame_data_path(mut self, f: Option<PathBuf>) -> Result<FrameDataBuilder, Box<FrameDataFileError>> {
        match f {
            Some(path) => {
                self.frame_data_path = Some(path);
                Ok(self)
            },
            None => {
                Err(Box::new(FrameDataFileError::NonePath))
            }
        }
    }

    fn set_output_path(mut self, o: Option<PathBuf>) -> Result<FrameDataBuilder, Box<OutputDirectoryError>> {
        match o {
            Some(output_dir) => {
                self.output_path = Some(output_dir);
                Ok(self)
            },
            None => {
                Err(Box::new(OutputDirectoryError::InvalidPath))
            }
        }
    }

    fn set_image_extension(mut self) -> Result<FrameDataBuilder, ImageFileError> {
        match self.image_path.to_owned() {
            Some(path) => {
                path
                .extension()
                .and_then(OsStr::to_str)
                .ok_or_else(|| ImageFileError::InvalidImageFile)
                .and_then(SupportedImageFormat::from_str)
                .and_then(|format| {
                    self.image_data_format = Some(format);
                    Ok(self)
                })  
            },
            None => {
                Err(ImageFileError::InvalidImageFile)
            }
        }
    }

    fn set_frame_data_extension(mut self) -> Result<FrameDataBuilder, FrameDataFileError> {
        match self.frame_data_path.to_owned() {
            Some(path) => {
                path
                .extension()
                .and_then(OsStr::to_str)
                .ok_or_else(|| FrameDataFileError::NonePath)
                .and_then(SupportedFrameDataFormat::from_str)
                .and_then(|format| {
                    self.frame_data_format = Some(format);
                    Ok(self)
                })
            },
            None => {
                Err(FrameDataFileError::NonePath)
            }
        }
    }

    /// Will use this structs set frame data file to be ready for processing. Will return an error from the serde serialization error for each attempt
    fn frame_data(self) -> Result<FrameData, Box<dyn std::error::Error>> {
        match self.frame_data_format {
            Some(format) => {
                match format {
                    SupportedFrameDataFormat::Plist => {
                        match self.frame_data_path {
                            Some(path) => {
                                plist::from_file(path)
                                .or_else(|err|Err(Box::new(err) as Box<dyn std::error::Error>))
                            },
                            None => {
                                Err(Box::new(FrameDataFileError::NonePath))
                            }
                        }
                    },
                    SupportedFrameDataFormat::Json => {
                        match self.frame_data_path {
                            Some(path) => {
                                let json_file = File::open(path)?;
                                let reader = BufReader::new(json_file);
                                serde_json::from_reader(reader)
                                .or_else(|err| Err(Box::new(err) as Box<dyn std::error::Error>))
                            }, 
                            None => {
                                Err(Box::new(FrameDataFileError::NonePath))
                            }
                        }
                    },
                }
            },
            None => Err(Box::new(FrameDataBuilderError::Processing))
        }
    }

    pub fn build(self) -> Result<FrameData, Box<dyn std::error::Error>> {

        match (self.output_path.to_owned(), self.image_path.to_owned()) {
            (Some(o), Some(i)) => {
                self
                .set_image_extension()?
                .set_frame_data_extension()?
                .frame_data()?
                .set_image_path(&i)?
                .set_image_data()?
                .set_output_directory(&o)
            }
            _ => {
                Err(Box::new(FrameDataBuilderError::Processing))
            }
        }
    }
}


/// Gets an image from a given path and decodes, returns an `ImageResult<DynamicImage>`
pub fn get_image<P>(image_path: P) -> ImageResult<DynamicImage> 
    where P: AsRef<Path> 
{
    Ok(ImageReader::open(image_path)?.with_guessed_format()?.decode()?)
}

// Need to actually implement tests
/*
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
*/

#[test]
fn anim_name_extraction() {

    assert_eq!(extract_animation_name("boss_andromeda_attack_000.png").unwrap(), "boss_andromeda".to_string());
}