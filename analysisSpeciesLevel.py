import json
import pandas as pd
import argparse
from pygbif import species
import requests


# from here get gbif species key for each species name, get full taxonomic hierarchy for each species key
# add genus, family and order to relevant cols

# run through and get common names 
species_names = pd.read_csv('../data/species_ranges_and_traits_iucn.csv')

# read in VernacularNames.tsv file
vernacular_names = pd.read_csv('../data/VernacularName.tsv', sep='\t')
vernacular_names = vernacular_names[vernacular_names['language'] == 'en']
vernacular_names = vernacular_names[['vernacularName', 'taxonID']]
vernacular_names.to_csv('../data/vernacular_names_english.csv', index=False)
vernacular_names['taxonID'] = vernacular_names['taxonID'].astype(str)
for index, row in species_names.iterrows():
    # get the sci_name
    sci_name = row['sci_name']
    taxonID = str(row['gbif_taxonID'])
    print(taxonID)
    try:
        if len(taxonID) > 0:
            common_name = vernacular_names[vernacular_names['taxonID'] == taxonID]['vernacularName'].values
            print(f'found common_name {common_name} for sci_name {sci_name}')
            species_names.at[index, 'common_name'] = common_name[0]
        else:
            print(f'no taxonID found for sci_name {sci_name}')
    except IndexError:
        print(f'index error for {sci_name}')
        species_names.at[index, 'common_name'] = None

# write out the updated species_names dataframe to a new csv
species_names.to_csv('../data/species_ranges_and_traits_gbif_iucn.csv', index=False)

