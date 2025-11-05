import os
import json
import argparse
import pandas as pd
import string 

# def main(input_dir, output_file):
def main(input_file, mapping_file, output_file):
    # read all json files in input dir
    # append to a dataframe

    data = json.load(open(input_file))
    # convert json to pandas dataframe
    df = pd.DataFrame(data)

    with open(mapping_file) as f:
        image_to_species = f.readlines()
    # convert to a dictionary
    image_to_species = dict(x.strip().split(':') for x in image_to_species)
    # print(image_to_species)

    # change the path variable to contain the correct species, using the mapping txt
    for index, row in df.iterrows():
        subdir_num = row['path'].split('/')[5]
        if subdir_num in image_to_species:
            df.loc[index, 'correct_label'] = image_to_species[subdir_num].lstrip()

    num_correct = 0
    count = 0
    results_dict = {}
    for index, row in df.iterrows():
        response = str(row['response'])
        response = response.lower()
        response = response.translate(str.maketrans('', '', string.punctuation))
        if response == 'lioness':
            response = 'female lion'
        if response == 'insectsspider':
            response = 'insectspider'
        if response == 'batearredfox':
            response = 'bateared fox'
        if response == 'bat-earedfox':
            response = 'bateared fox'
        if response == 'batearned fox':
            response = 'bateared fox'
        if response == 'batearred fox':
            response = 'bateared fox'
        if response == 'gnu wildebeest':
            response = 'wildebeest'
        if response == 'hyaena':
            response = 'hyena'
        if response == 'servals':
            response = 'serval'
        if response == 'grey fox':
            response = 'gray fox'
        if response == 'racoon':
            response = 'raccoon'
        if response == 'horsetailed squirrel':
            response = 'horse tailed squirrel'
        if response == 'moonrat':
            response = 'moon rat'
        if response == 'stripestriped ground squirrel':
            response = 'striped ground squirrel'
        response = response.replace(" ", "")
        df.at[index, 'response'] = response
        correct_label = str(row['correct_label'])
        correct_label = correct_label.translate(str.maketrans('', '', string.punctuation))
        correct_label = correct_label.replace(" ","")
        correct_label = correct_label.lower()
        df.at[index, 'correct_label'] = correct_label
        if correct_label in response:
            df.at[index, 'correct_or_not'] = True
            num_correct += 1
            count += 1
        else:
            df.at[index, 'correct_or_not'] = False
            count += 1
    df.to_csv(output_file + "_raw.csv", index=False)
    percent_correct = (num_correct / count) * 100
    results_dict['overall_percent_correct'] = percent_correct
    print(f"Overall precent correct {percent_correct}")
    species = df['correct_label'].unique()
    grouped = df.groupby('correct_label')

    for species_name in species:
        speciesdf = grouped.get_group(species_name)
        species_count = speciesdf.shape[0]
        species_correct = speciesdf['response'] == speciesdf['correct_label']
        species_correct = species_correct.sum()
        species_percent_correct = (species_correct / species_count) * 100
        print(f"{species_name} : {species_percent_correct}")
        results_dict[species_name] = species_percent_correct
    with open(output_file + ".json", 'w') as fp: # this should be an argument
        json.dump(results_dict, fp)
    
    df = pd.DataFrame(list(results_dict.items()), columns=['species', 'percent_correct'])
    df.to_csv(output_file + ".csv", index=False)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Process CSV files.')
    parser.add_argument('-i', '--input_file', type=str, required=True, help='Path to the input dir.')
    parser.add_argument('-m', '--mapping_file', type=str, required=True, help='Path to the mapping file.')
    parser.add_argument('-o', '--output_file', type=str, required=False, help='Path to the output file.')
    args = parser.parse_args()
    main(args.input_file, args.mapping_file, args.output_file)