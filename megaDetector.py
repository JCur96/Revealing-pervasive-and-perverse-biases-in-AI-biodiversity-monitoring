import os
import numpy as np
from PIL import Image
import torch
from torch.utils.data import DataLoader
from PytorchWildlife.models import detection as pw_detection
from PytorchWildlife.data import transforms as pw_trans
from PytorchWildlife.data import datasets as pw_data
from PytorchWildlife import utils as pw_utils
import argparse
import json
import math
import shutil

import supervision as sv

from memory_profiler import profile

def save_crop_images(results, output_dir):
    """
    Save cropped images based on the detection bounding boxes.
    
    Args:
        results (list): 
            Detection results containing image ID and detections.
        output_dir (str): 
            Directory to save the cropped images.
    """
    assert(isinstance(results, list))
    os.makedirs(output_dir, exist_ok=True)
    with sv.ImageSink(target_dir_path=output_dir, overwrite=True) as sink:
        for entry in results:
            for i, (xyxy, _, _, cat, _) in enumerate(entry["detections"]):
                cropped_img = sv.crop_image(image=np.array(Image.open(entry["img_id"])), xyxy=xyxy)
                # cropped_img = np.ascontiguousarray(cropped_img)
                # cropped_img = torch.from_numpy(cropped_img).float()
                # cropped_img *= 255.0
                sink.save_image(
                    image=cropped_img,
                    image_name="{}_{}_{}".format(int(cat), i, entry["img_id"].rsplit('/', 1)[1])
                )

def crop_images_from_json(json_file_path, image_folder):
    """
    Reads a JSON file with bbox information and crops images accordingly.

    Args:
    json_file_path (str): Path to the JSON file containing bbox data.
    image_folder (str): Folder where the images are stored.

    The JSON file should have a format like:
    {
        "image1.jpg": {"bboxes": [[x1, y1, width1, height1], [x2, y2, width2, height2], ...]},
        "image2.jpg": {"bboxes": [[x1, y1, width1, height1], [x2, y2, width2, height2], ...]},
        ...
    }
    """
    # Read JSON file
    with open(json_file_path, 'r') as file:
        data = json.load(file)

    # Process each image
    for image_name, properties in data.items():
        image_path = f"{image_folder}/{image_name}"
        bboxes = properties["bboxes"]

        with Image.open(image_path) as img:
            # Crop for each bbox
            for i, bbox in enumerate(bboxes):
                cropped_img = img.crop((bbox[0], bbox[1], bbox[0] + bbox[2], bbox[1] + bbox[3]))
                cropped_img.save(f"{image_folder}/cropped_{i}_{image_name}")


parser = argparse.ArgumentParser(description='Mega Detector run.')
parser.add_argument('-i', '--input_dir', type=str, required=True, help='Path to the input dir.')
parser.add_argument('-o', '--output_file_path', type=str, required=False, help='Path to the output file.')
parser.add_argument('-t', '--type', type=str, required=True, choices=["annotations", "crops", "json"], help='Type of output, one of "annotations", "crops" or "json".')
parser.add_argument('-dc', '--delete_crops', type=bool, required=False, default=False, help='Delete crops directories if they exist.')
parser.add_argument('-rb', '--rename_bbox', type=bool, required=False, default=False, help='Rename bboxOut.json if it exists.')
args = parser.parse_args()

def merge_dictionaries(original, new):
    for key, value in new.items():
        if key in original:
            if isinstance(original[key], list) and isinstance(value, list):
                original[key].extend(value)  # Assuming it's okay to combine lists
            elif isinstance(original[key], dict) and isinstance(value, dict):
                # Recursive merge for nested dictionaries
                merge_dictionaries(original[key], value)
            else:
                # Handle non-container types or conflicting types: overwrite or custom logic
                original[key] = value
        else:
            original[key] = value


print(torch.cuda.is_available())
DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")
detection_model = pw_detection.MegaDetectorV5(device=DEVICE, pretrained=True)

for subdir in os.listdir(args.input_dir):
    full_subdir_path = os.path.join(args.input_dir, subdir)
    if os.path.isdir(full_subdir_path):
        dirs_in_subdir = [f.path for f in os.scandir(full_subdir_path) if f.is_dir()]
    else:
        print(f"Error: {full_subdir_path} is not a directory")
    for s in dirs_in_subdir:
        if 'crops' in s:
            print(f"{s} contains crops")
        if args.delete_crops:
            shutil.rmtree(s)
            print(f"deleted {s}")
        continue
    if not os.path.isdir(full_subdir_path):
        print(f"Error: {full_subdir_path} is not a directory")
        continue
    else:
        if any('bboxOut.json' in s for s in os.listdir(full_subdir_path)):
            if args.rename_bbox:
                os.rename(full_subdir_path + '/bboxOut.json', args.input_dir + '/' + subdir + '_bboxOut_old.json')
                print("renamed bboxOut.json to bboxOut_old.json")
            else:
                print("bboxOut.json already made, skipping")
            # remove the bbox if delete_crops is true
            if args.delete_crops:
                os.remove(full_subdir_path + '/bboxOut.json')
                print("deleted bboxOut.json")
            continue
    if any(f.endswith('.jpg') or f.endswith('.jpeg') or f.endswith('.png') for f in os.listdir(full_subdir_path)):
        print(f"{subdir} contains images")
    else:
        print(f"{subdir} contains no images, skipping")
        continue
    num_files = len([name for name in os.listdir(full_subdir_path) if os.path.isfile(os.path.join(full_subdir_path, name))])
    print(f"{subdir} contains num_files: {num_files}")
    if num_files == 0:
        print(f"{subdir} has no files, skipping")
        continue
    if num_files > 2000:
        num_batches = math.ceil(num_files / 2000)
        print(f"num_batches: {num_batches}")
        for i in range(num_batches):
            batch_dir = full_subdir_path + f"_batch{i}/"
            if not os.path.exists(batch_dir):
                print(f"creating dir: {batch_dir}")
                os.mkdir(batch_dir)
            else:
                print(f"dir already exists: {batch_dir}")
        file_list = [f for f in os.listdir(full_subdir_path) if os.path.isfile(os.path.join(full_subdir_path, f))]
        chunk_list = [file_list[i:i + 2000] for i in range(0, len(file_list), 2000)]
        for i, chunk in enumerate(chunk_list):
            for file in chunk:
                print(f"moving {file} to {full_subdir_path + f'_batch{i}/'}")
                shutil.move(full_subdir_path + '/' + file, full_subdir_path + f'_batch{i}/' + file)
        
        continue


    dir_path = os.path.join(args.input_dir, subdir)
    tgt_folder_path = dir_path
    dataset = pw_data.DetectionImageFolder(
        tgt_folder_path,
        transform=pw_trans.MegaDetector_v5_Transform(target_size=detection_model.IMAGE_SIZE,
                                                    stride=detection_model.STRIDE)
    )
    loader = DataLoader(dataset, batch_size=12, shuffle=False, 
                        pin_memory=True, num_workers=2, drop_last=False)

    results = detection_model.batch_image_detection(loader)
    if args.type == "crops":
        for entry in results:
            entry["img_id"] 
            print(entry)
    if args.type == "json":
        local_output_file = os.path.join(dir_path + "/bboxOut.json")
        print("created outJson")
        pw_utils.save_detection_json(results, local_output_file,
                                    categories=detection_model.CLASS_NAMES)
        del results
        torch.cuda.empty_cache()





