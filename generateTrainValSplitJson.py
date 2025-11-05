import json
import argparse
# import os
import random

def main():
    parser = argparse.ArgumentParser(description='Process a JSON file using OpenAI.')
    parser.add_argument('-i', '--input_data', type=str, required=True, help='Path to the input JSON file.')
    parser.add_argument('-o', '--out', type=str, required=True, help='Path to the output JSON file.')
    parser.add_argument('-ds', '--data_set', type=str, required=False, help='what dataset is this for', choices=['orinoquia', 'north_america', 'serengeti', 'borneo', 'wellington'])
    parser.add_argument('-f', '--full_data', type=bool, required=False, help='write out full data for each image, sorted into train and val, not just location')
    parser.add_argument('-r', '--resume', type=bool, required=False, help='resume from temp file')
    args = parser.parse_args()
    with open(args.input_data, 'r') as f:
        data = json.load(f)

    # for mapping categories to locations 
    category_dict = {}
    category_dict['categories'] = [{'id': category['id'], 'name': category['name']} for category in data['categories']]
    annotations_dict = {}
    annotations_dict['annotations'] = [{'image_id': annotation['image_id'], 'category_id': annotation['category_id']} for annotation in data['annotations']]
    # swap out the category_id for the category name in the annotations_dict]
    for annotation in annotations_dict['annotations']:
        annotation['category_id'] = [category['name'] for category in category_dict['categories'] if category['id'] == annotation['category_id']][0]

    # delete category_dict from memory
    del category_dict

    # Create a dictionary that maps image_id to category_id
    image_id_to_category_map = {annotation['image_id']: annotation['category_id'] for annotation in annotations_dict['annotations']}

    for image in data['images']:
        image['category'] = image_id_to_category_map.get(image['id'])



    loc_list = []
    loc_category_list = []
    category_list = []
    train_val_split = {}
    train_val_split['train'] = []
    train_val_split['val'] = []
    # study_list = []
    if args.data_set == 'wellington':
        
        for image in data['images']:
            location = image['location']
            # camera = image['camera']
            category = image['category']
            if category not in category_list:
                category_list.append(category)
            # make a tuple of location and camera
            # loc_cam = (location, camera)
            # if loc_cam not in loc_cam_list:
            #     loc_cam_list.append(loc_cam)
            if location not in loc_list:
                loc_list.append(location)
            # if camera not in cam_list:
            #     cam_list.append(camera)
            # make a tuple of location and category
            loc_category = (location, category)
            if loc_category not in loc_category_list:
                loc_category_list.append(loc_category)
        
        # randomly select 80% of locations for training and 20% for validation
        random.shuffle(loc_list)
        train_loc = loc_list[:int(0.8*len(loc_list))]
        val_loc = loc_list[int(0.8*len(loc_list)):]

        # check that we have at least one of each category in both train and val
        # if not, reshuffle the locations until we do
        print(f'Shuffling {len(loc_list)} locations. This may take a long time...')
        for category in category_list:
            train_locs = [loc for loc in train_loc if (loc, category) in loc_category_list]
            val_locs = [loc for loc in val_loc if (loc, category) in loc_category_list]
            while len(train_locs) == 0 or len(val_locs) == 0:
                random.shuffle(loc_list)
                train_loc = loc_list[:int(0.8*len(loc_list))]
                val_loc = loc_list[int(0.8*len(loc_list)):]
                train_locs = [loc for loc in train_loc if (loc, category) in loc_category_list]
                val_locs = [loc for loc in val_loc if (loc, category) in loc_category_list]
        
        # if the location is already present in train or val, don't add it again
        for loc in train_loc:
            # print(loc)
            train_val_split['train'].append(loc)
        for loc in val_loc:
            train_val_split['val'].append(loc)
        
        train_val_split['train'] = list(set(train_val_split['train']))
        train_val_split['val'] = list(set(train_val_split['val']))
        

        # see if we have at least one of each category in both train and val
        print(f'Number of locations total: {len(loc_list)}')
        for category in category_list:
            train_locs = [loc for loc in train_val_split['train'] if (loc, category) in loc_category_list]
            # print(train_val_split['train'])
            # print(train_locs)
            val_locs = [loc for loc in train_val_split['val'] if (loc, category) in loc_category_list]
            print(f'{category}: {len(train_locs)} in train, {len(val_locs)} in val')

    if args.data_set == 'serengeti':
        # read in the S1-6 recommended splits
        with open('../data/Serengeti/SnapshotSerengetiSplits_v0.json', 'r') as f:
                recommended_splits = json.load(f)
        
        for image in data['images']:
            location = image['location']
            if location not in loc_list:
                loc_list.append(location)
        
            category = image['category']
            loc_category = (location, category)
            if loc_category not in loc_category_list:
                loc_category_list.append(loc_category)
            
            if category not in category_list:
                category_list.append(category)
       
        # place in appropriate split based on recommended splits
        print(f'Using recommended splits, which span all 11 seasons. {len(loc_list)} locations in total. recommended splits {len(recommended_splits["train"])} in train, {len(recommended_splits["val"])} in val.')
        for location in loc_list:
            if location in recommended_splits['train']:
                train_val_split['train'].append(location)
            else:
                train_val_split['val'].append(location)
        # remove from loc_list 
        for location in train_val_split['train']:
            loc_list.remove(location)
        for location in train_val_split['val']:
            loc_list.remove(location) # vectorise all that later

        train_loc = train_val_split['train']
        val_loc = train_val_split['val']

        # write out the train and val splits, in case of crashes
        with open(f'{args.out}_locs_temp.json', 'w') as f:
            json.dump(train_val_split, f)
        
    else:    
        for image in data['images']:
            location = image['location']
            # study = image['study']
            if location not in loc_list:
                loc_list.append(location)

            category = image['category']
            loc_category = (location, category)
            if loc_category not in loc_category_list:
                loc_category_list.append(loc_category)

            if category not in category_list:
                category_list.append(category)
                
        
        random.shuffle(loc_list)
        train_loc = loc_list[:int(0.8*len(loc_list))]
        val_loc = loc_list[int(0.8*len(loc_list)):]

        # check that we have at least one of each category in both train and val
        # if not, reshuffle the locations until we do
        print(f'Shuffling {len(loc_list)} locations. This may take a long time...')
        for category in category_list:
            train_locs = [loc for loc in train_loc if (loc, category) in loc_category_list]
            val_locs = [loc for loc in val_loc if (loc, category) in loc_category_list]
            number_of_locs_in_category = len([loc for loc in loc_list if (loc, category) in loc_category_list])
            if number_of_locs_in_category == 1:
                print(f'Only one location for {category}, dropping it from the split')
                train_locs =[]
                val_locs = []
                continue
            else:
                print(f'{category}: total number of locations is {number_of_locs_in_category}')
                while len(train_locs) == 0 or len(val_locs) == 0:
                    random.shuffle(loc_list)
                    train_loc = loc_list[:int(0.8*len(loc_list))]
                    val_loc = loc_list[int(0.8*len(loc_list)):]
                    train_locs = [loc for loc in train_loc if (loc, category) in loc_category_list]
                    val_locs = [loc for loc in val_loc if (loc, category) in loc_category_list]
        for loc in train_loc:
            train_val_split['train'].append(loc)
        for loc in val_loc:
            train_val_split['val'].append(loc)
        
        train_val_split['train'] = list(set(train_val_split['train']))
        train_val_split['val'] = list(set(train_val_split['val']))

        number_to_go = len(category_list)
        for category in category_list:
            train_locs = [loc for loc in train_val_split['train'] if (loc, category) in loc_category_list]
            val_locs = [loc for loc in train_val_split['val'] if (loc, category) in loc_category_list]
            print(f'{category}: {len(train_locs)} in train, {len(val_locs)} in val')
            number_to_go -= 1
            print(f'{number_to_go} categories left to check')
    
 
    if args.full_data:
        if args.resume:
            with open(f'{args.out}_locs_temp.json', 'r') as f:
                train_val_split = json.load(f)
                train_loc = train_val_split['train']
                val_loc = train_val_split['val']
        print('Writing out full data for each image, this will take some time...\n \n \n')
        train_val_split['train'] = [{'id': image['id']} for image in data['images'] if image['location'] in train_loc]
        train_val_split['val'] = [{'id': image['id']} for image in data['images'] if image['location'] in val_loc]

        # append a new key to each object within train and val 
        print('Adding category to each annotation...')
        category_dict = {}
        category_dict['categories'] = [{'id': category['id'], 'name': category['name']} for category in data['categories']]
        annotations_dict = {}
        annotations_dict['annotations'] = [{'image_id': annotation['image_id'], 'category_id': annotation['category_id']} for annotation in data['annotations']]
        # swap out the category_id for the category name in the annotations_dict]
        for annotation in annotations_dict['annotations']:
            annotation['category_id'] = [category['name'] for category in category_dict['categories'] if category['id'] == annotation['category_id']][0]

        # delete category_dict from memory
        del category_dict
        print('Added category to each annotation and deleted category_dict from memory...')

        print('Creating a dictionary that maps image_id to category_id...')
        # Create a dictionary that maps image_id to category_id
        image_id_to_category_map = {annotation['image_id']: annotation['category_id'] for annotation in annotations_dict['annotations']}
        # print(image_to_category)
        
        print('Adding categories to the images in train and val...')
        # Add annotations['category_id'] to the images in train and val
        for object in train_val_split['train']:
            object['category'] = image_id_to_category_map.get(object['id'])
        for object in train_val_split['val']:
            object['category'] = image_id_to_category_map.get(object['id'])
        
        if args.data_set != 'serengeti':
                
            print('Mapping categories to category names...')
            # add annotations['category_id'] to the images in train and val
            # get the number of objects in 'train'

            if args.resume:
                # open the temp_train file, skip those objects from the train_val_split['train'] list
                with open(f'{args.out}_temp_train.json', 'r') as f:
                    temp_train = json.load(f)

                num_objects = len(train_val_split['train'])
                print(f'Number of objects in train: {num_objects}')
                num_objects_in_temp = len(temp_train['train'])
                print(f'Skipping {num_objects_in_temp} objects from temp file...')
                # remove the objects in the temp file from the train_val_split['train'] list
                for object in temp_train['train']:
                    train_val_split['train'].remove(object)
                num_objects_remaining = num_objects - num_objects_in_temp
                
                for object in train_val_split['train']:
                    print(object)
                    if object['id'] in [temp_object['id'] for temp_object in temp_train['train']]:
                        continue
                    object['category'] = [annotation['category_id'] for annotation in annotations_dict['annotations'] if annotation['image_id'] == object['id']][0]
                    print(object)
                    num_objects_remaining -= 1
                    # print out the progress every 50 objects
                    if num_objects_remaining % 5000 == 0:
                        print(f'{num_objects_remaining} objects remaining...')
                        train_val_split['train'] += temp_train['train']
                        # write out a temp file, in case of crashes
                        with open(f'{args.out}_temp_train.json', 'w') as f:
                            json.dump(train_val_split, f)

            num_objects = len(train_val_split['train'])
            num_objects_remaining = num_objects
            print(f'Number of objects in train: {num_objects}')
            for object in train_val_split['train']:
                print(object)
                object['category'] = [annotation['category_id'] for annotation in annotations_dict['annotations'] if annotation['image_id'] == object['id']][0]
                print(object)
                num_objects_remaining -= 1
                if num_objects_remaining % 5000 == 0:
                    print(f'{num_objects_remaining} objects remaining...')
                    with open(f'{args.out}_temp_train.json', 'w') as f:
                        json.dump(train_val_split, f)
            
            if args.resume:
                # open the temp_train file, skip those objects from the train_val_split['train'] list
                with open(f'{args.out}_temp_val.json', 'r') as f:
                    temp_val = json.load(f)

                num_objects = len(train_val_split['val'])
                print(f'Number of objects in train: {num_objects}')
                num_objects_in_temp = len(temp_val['val'])
                print(f'Skipping {num_objects_in_temp} objects from temp file...')
                for object in temp_val['val']:
                    train_val_split['val'].remove(object)
                num_objects_remaining = num_objects - num_objects_in_temp
                for object in train_val_split['val']:
                    if object['id'] in [temp_object['id'] for temp_object in temp_val['val']]:
                        continue
                    object['category'] = [annotation['category_id'] for annotation in annotations_dict['annotations'] if annotation['image_id'] == object['id']][0]
                    num_objects_remaining -= 1
                    if num_objects_remaining % 5000 == 0:
                        print(f'{num_objects_remaining} objects remaining...')
                        train_val_split['val'] += temp_val['val']
                        with open(f'{args.out}_temp_val.json', 'w') as f:
                            json.dump(train_val_split, f)
            num_objects = len(train_val_split['val'])
            num_objects_remaining = num_objects
            print(f'Number of objects in val: {num_objects}')
            for object in train_val_split['val']:
                object['category'] = [annotation['category_id'] for annotation in annotations_dict['annotations'] if annotation['image_id'] == object['id']]
                num_objects_remaining -= 1
                if num_objects_remaining % 5000 == 0:
                    print(f'{num_objects_remaining} objects remaining...')
                    with open(f'{args.out}_temp_val.json', 'w') as f:
                        json.dump(train_val_split, f)


    else:
        train_val_split['train'] = train_loc
        train_val_split['val'] = val_loc

    with open(f'{args.out}.json', 'w') as f:
        json.dump(train_val_split, f)
        
if __name__ == '__main__':
    main()