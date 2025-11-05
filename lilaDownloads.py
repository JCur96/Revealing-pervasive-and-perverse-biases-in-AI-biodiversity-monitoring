#
# download_lila_subset.py
#
# Example of how to download a list of files from LILA, e.g. all the files
# in a data set corresponding to a particular species.
#

#%% Constants and imports

import json
import urllib.request
import tempfile
import zipfile
import os
import itertools
import shutil
import argparse
import random
import pandas as pd

from collections import defaultdict

from tqdm import tqdm
from multiprocessing.pool import ThreadPool
from urllib.parse import urlparse

from data_management.lila.lila_common import \
    read_lila_all_images_file, is_empty, azure_url_to_gcp_http_url, lila_base_urls
from md_utils.url_utils import download_url

# new lila docs function 
def download_relative_url(relative_url, output_base, relative_url_to_nominal_provider, provider='gcp', 
                          verbose=False, overwrite=False):
    """
    Download a URL to output_base, preserving the path relative to the common LILA root.
    """
    
    assert not relative_url.startswith('/')
    
    if provider == 'azure':
        nominal_provider = relative_url_to_nominal_provider[relative_url] # hmmmmm
        if nominal_provider != 'azure':
            if verbose:
                print('URL {} not available on Azure, falling back to GCP'.format(
                    relative_url))
            provider = 'gcp'
            
    url = lila_base_urls[provider] + relative_url
    
    result = {'status':'unknown','url':url,'destination_filename':None}
    
    destination_filename = os.path.join(output_base,relative_url)
    result['destination_filename'] = destination_filename
    
    if ((os.path.isfile(destination_filename)) and (not overwrite)):
        result['status'] = 'skipped'
        return result
    try:
        download_url(url, destination_filename, verbose=verbose, force_download=overwrite)
    except Exception as e:
        print('Warning: error downloading URL {}: {}'.format(
            url,str(e)))     
        result['status'] = 'error: {}'.format(str(e))
        return result
    
    result['status'] = 'success'
    return result

def main():

    parser = argparse.ArgumentParser("downloadLila")
    parser.add_argument("-d", "--datasets", help="Datasets to download images from. Give as a comma seperated list.", type=str, choices=['orinoquia', 'north_america', 'serengeti', 'wellington'], nargs='+')
    parser.add_argument("-ld", "--lila_dir", help="URL to the metadata file.", type=str, default='/mnt/i/lila_datasets')
    parser.add_argument("-fr", "--first_run", help="First run. Does the metadata need downloading and seperating?", type=bool, default=False) # first run?
    args = parser.parse_args()

    lila_local_base = os.path.expanduser(args.lila_dir) 

    metadata_dir = os.path.join(lila_local_base,'metadata')
    os.makedirs(metadata_dir,exist_ok=True)

    output_dir = os.path.join(lila_local_base,'lila_downloads_by_dataset')
    os.makedirs(output_dir,exist_ok=True)

    verbose = True

    n_download_threads = 5

    preferred_provider = 'gcp' # 'azure', 'gcp', 'aws'

    if args.first_run:
        df = read_lila_all_images_file(metadata_dir)
        df_names = df['dataset_name'].unique()
        print(df_names)
        grouped = df.groupby('dataset_name')
        dfs = {name: group for name, group in grouped}
        df_list = [(name, group) for name, group in dfs.items()]
        for name, df_group in df_list:
            df_group.to_csv(f'{metadata_dir}/{name}.csv')
    datasets = args.datasets
    # split the list out to convert to dataset names
    for i in range(len(datasets)):
        if datasets[i] == 'orinoquia':
            datasets[i] = 'Orinoquia Camera Traps.csv'
        if datasets[i] == 'north_america':
            datasets[i] = 'NACTI.csv'
        if datasets[i] == 'serengeti':
            datasets[i] = 'Snapshot Serengeti.csv'
        if datasets[i] == 'wellington':
            datasets[i] = 'Wellington Camera Traps.csv'

    # find the corresponding metadata files
    metadata_dfs = {}
    for dataset in datasets:
        metadata_file = f'{metadata_dir}/{dataset}'
        df = pd.read_csv(metadata_file)
        metadata_dfs[dataset] = df

    for i in range(len(datasets)):
        if datasets[i] == 'Orinoquia Camera Traps.csv':
            datasets[i] = '../data/Orinoquia/lilaDownloads_1100.json'
        if datasets[i] == 'NACTI.csv':
            datasets[i] = '../data/NA/lilaDownloads_1100.json'
        if datasets[i] == 'Snapshot Serengeti.csv':
            datasets[i] = '../data/Serengeti/lilaDownloads_1100.json'
        if datasets[i] == 'Wellington Camera Traps.csv':
            datasets[i] = '../data/Wellington/lilaDownloads_1100.json'
    
    # read in the jsons
    dfs = {}
    for path in datasets:
        with open(path, 'r') as f:
            data = json.load(f)
            dfs[path] = data

    df_list_requests_train = ()
    for key in dfs.keys():
        df = pd.DataFrame.from_dict(dfs[key]['train'])
        df_list_requests_train = df_list_requests_train + (df,)

    df_list_requests_val = ()
    for key in dfs.keys():
        df = pd.DataFrame.from_dict(dfs[key]['val'])
        df_list_requests_val = df_list_requests_val + (df,)

    df_list_urls = ()
    for key in metadata_dfs.keys():
        df = metadata_dfs[key]
        df_list_urls = df_list_urls + (df,)
    
    for df in df_list_urls:
        df['image_id'] = df['image_id'].str.split(':').str[1]
    
    
    list_of_url_lists = []
    for i in range(len(df_list_requests_train)):
        urls = []
        df_resquests = df_list_requests_train[i]
        df_urls = df_list_urls[i]
        mask = df_urls['image_id'].apply(lambda x: any(id in x for id in df_resquests['id']))
        urls = df_urls.loc[mask, 'url'].tolist()
        list_of_url_lists.append(urls)
    for i in range(len(df_list_requests_val)):
        urls = []
        df_resquests = df_list_requests_val[i]
        df_urls = df_list_urls[i]
        mask = df_urls['image_id'].apply(lambda x: any(id in x for id in df_resquests['id']))
        urls = df_urls.loc[mask, 'url'].tolist()
        list_of_url_lists.append(urls)

    all_urls = list(itertools.chain(*list_of_url_lists))

    all_urls_relative = []

    relative_url_to_nominal_provider = {}

    for url in all_urls:
        found_base = False
        for provider in lila_base_urls.keys():
            base = lila_base_urls[provider]
            if url.startswith(base):
                relative_url = url.replace(base,'')
                all_urls_relative.append(relative_url)
                relative_url_to_nominal_provider[relative_url] = provider
                found_base = True
                break
        assert found_base
        
    assert len(all_urls) == len(all_urls_relative)

    print('Downloading {} images on {} workers, preferred provider is {}'.format(
    len(all_urls),n_download_threads,preferred_provider))

    if n_download_threads <= 1:

        results = []
        
        for url_relative in tqdm(all_urls_relative):        
            result = download_relative_url(url_relative,
                                        output_base=output_dir,
                                        relative_url_to_nominal_provider=relative_url_to_nominal_provider,
                                        provider=preferred_provider,
                                        verbose=verbose)
            results.append(result)
        
    else:

        pool = ThreadPool(n_download_threads)        
        results = list(tqdm(pool.imap(lambda s: download_relative_url(
            s,output_base=output_dir,relative_url_to_nominal_provider=relative_url_to_nominal_provider,provider=preferred_provider,verbose=verbose),
            all_urls_relative), total=len(all_urls_relative)))



if __name__ == '__main__':
    main()