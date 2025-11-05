import os
import json
import argparse
import base64
import openai
import time
import requests

# Initialize OpenAI API key
openai.organization = "YOUR_ORG_HERE"
openai.api_key = os.getenv("OPENAI_API_KEY")
parser = argparse.ArgumentParser(description='Process a JSON file using OpenAI.')
parser.add_argument('-i', '--image_path', type=str, required=True, help='Path to the input JSON file.')
parser.add_argument('-o', '--output_file', type=str, required=True, help='Path to the output JSON file.')
args = parser.parse_args()

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
    
with open(args.image_path, 'r', encoding='utf-8') as f:
    image_paths = json.load(f)
for image_path in image_paths:
    if 'response' not in image_path:
        image_path['response'] = None
    else: 
        continue

for image_path in image_paths:
    image_size = os.path.getsize(image_path['path']) / 1000000
    # skip any image over 20MB 
    if image_size > 20:
        print(f'Image size of {image_path["path"]} is {image_size} MB, skipping')
        continue
    if image_path['response'] == 'Unknown':
        image_path['response'] = None
    if image_path['response'] is not None:
        print(f'Image {image_path["path"]} has already been processed, skipping')
        continue
    base64_image = encode_image(image_path['path'])
    headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {openai.api_key}"
    }

    payload = {
        "model": "gpt-4o",
        "seed": 42,
        "messages": [
        {
            "role": "user",
            "content": [
            {
                "type": "text",
                 "text": "Identify the animal in this photo. You must pick from the following options, even if you cannot reliably find an animal in the image, do not supply an answer other than from those listed. Only supply the option picked: aardvark, aardwolf, African wild dog, baboon, bat, bat-eared fox, buffalo, bushbuck, caracal, cattle, cheetah, civet, dik-dik, duiker, eland, elephant, genet, giraffe, Grant's gazelle, guineafowl, hare, hartebeest, hippopotamus, honey badger, brown hyena, spotted hyena, striped hyena, impala, insect/spider, jackal, kori bustard, kudu, leopard, lion cub, female lion, male lion, mongoose, ostrich, other bird, pangolin, porcupine, reedbuck, reptiles, rhinoceros, rodents, secretarybird, serval, steenbok, Thomson's gazelle, topi, vervet monkey, vulture, warthog, waterbuck, wildcat, wildebeest, zebra, zorilla" # prompt goes here
            },
            {
                "type": "image_url",
                "image_url": {
                "url": f"data:image/jpeg;base64,{base64_image}"
                }
            }
            ]
        }
        ],
        "max_tokens": 300
    }

    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload, timeout=30)

    try:
        image_path['response'] = response.json()['choices'][0]['message']['content']
        print(image_path['response'])
        with open(args.output_file, 'w', encoding='utf-8') as f:
            json.dump(image_paths, f, indent=4)
    except KeyError:
        if response.json()['error']['type'] == 'server_error':
            print(response.json()['error'])
        elif response.json()['error']['code'] == 'rate_limit_exceeded':
            print('Request rate limit exceeded, trying again in 15 minutes')
            time.sleep(900)
        elif response.json()['error']['type'] == 'invalid_request_error':
            print('invalid request error, image size likely exceeded 20 MB')
            print(response.json())
        else:
            print('Unhandled error code, please see response below to diagnose')
            print(response.json())
with open(args.output_file, 'w', encoding='utf-8') as f:
    json.dump(image_paths, f, indent=4)

