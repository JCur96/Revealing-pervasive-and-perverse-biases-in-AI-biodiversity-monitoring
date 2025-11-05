# python file to generate request jsons for lila downloads 
# reads in full train_val_splits_image.json 
# trims to given number of images / objects for train and val per class
# using a random selection of images where possible 
# outputs the final request json
import json
import argparse
import random
from collections import defaultdict

def main():
    parser = argparse.ArgumentParser(description='Generate JSON file for LILA downloads.')
    parser.add_argument('-i', '--input_data', type=str, required=True, help='Path to the input JSON file.')
    parser.add_argument('-nt', '--num_train', type=int, required=True, help='Number of training images per class.')
    parser.add_argument('-nv', '--num_val', type=int, required=True, help='Number of validation images per class.')
    parser.add_argument('-o', '--out', type=str, required=True, help='Path to the output JSON file.')
    args = parser.parse_args()

    with open(args.input_data, 'r') as f:
        data = json.load(f)
    
    # get unique categories from data, in either or both of 'train' and 'val']
    categories = []
    for key in ['train', 'val']:
        categories.extend([item['category'] for item in data[key]])
    # set all values to strings
    categories = [[str(item)] for item in categories]
    categories = [item for sublist in categories for item in sublist]
    categories = list(set(categories))
    # remove all punctuation, brackets and quotes
    categories = [item.replace("'", "").replace('"', '').replace('[', '').replace(']', '') for item in categories]    
    print(categories)

    data_train = data['train']
    data_val = data['val']
    # convert the category names to strings (from lists)
    for item in data_train:
        item['category'] = str(item['category'])
    for item in data_val:
        item['category'] = str(item['category'])
    # replace all the punctuation, brackets and quotes in the category names of data val and train
    for item in data_train:
        item['category'] = item['category'].replace("'", "").replace('"', '').replace('[', '').replace(']', '')
    for item in data_val:
        item['category'] = item['category'].replace("'", "").replace('"', '').replace('[', '').replace(']', '')
    # print(data_train)

    # unique 'categories' should be same across both train and val, but might not be
    categories = set([item['category'] for item in data_train])

    data_out = {}
    data_out['train'] = []
    data_out['val'] = []

    for category in categories:
        # get all 'id' for 'category' in 'train'
        category_train = [item['id'] for item in data_train if item['category'] == category]
        # print(category_train)
        # get all 'id' for 'category' in 'val'
        category_val = [item['id'] for item in data_val if item['category'] == category]
        # print(category_val)

        # get random 'id' for 'category' in 'train'
        random_train = random.sample(category_train, min(len(category_train), args.num_train))
        # get random 'id' for 'category' in 'val'
        random_val = random.sample(category_val, min(len(category_val), args.num_val))

        # add to 'train' and 'val' in data
        data_out['train'] += [{'id': item, 'category': category} for item in random_train]
        data_out['val'] += [{'id': item, 'category': category} for item in random_val]
        # print(len(data_out['val']))
        # print out the number of images per category
        print(f"Category: {category}, train: {len(random_train)}, val: {len(random_val)}")

    # print out the number of images per category in train and val in data_out
    category_counts = defaultdict(lambda: defaultdict(int))
    for key in ['train', 'val']:
        for item in data_out[key]:
            category_counts[item['category']][key] += 1
    print("Category counts:")
    for category, counts in category_counts.items():
        print(f"{category}:")
        for key, count in counts.items():
            print(f"  {key}: {count}")

    with open(args.out, 'w') as f:
        json.dump(data_out, f)

    

    


if __name__ == '__main__':
    main()