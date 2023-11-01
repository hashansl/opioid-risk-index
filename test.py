import pandas as pd
import matplotlib.pyplot as plt
import geopandas as gpd
import numpy as np
import itertools
from itertools import combinations
from scipy import spatial
import pickle as pickle
from pylab import *
from mpl_toolkits.mplot3d import Axes3D
# %matplotlib inline
import io
from PIL import Image, ImageDraw, ImageChops, ImageFont
import io
from tqdm import tqdm

import warnings

# Ignore FutureWarnings
warnings.simplefilter(action='ignore', category=FutureWarning)

Washington_Arlington_Alexandria_DC_VA_MD_WV_Counties = gpd.read_file('./data/DCMetroArea/Washington_Arlington_Alexandria_DC_VA_MD_WV_Counties.shp')

county_fips_list = Washington_Arlington_Alexandria_DC_VA_MD_WV_Counties['GEOID'].tolist()

us_svi = gpd.read_file('./data/SVI2020_US_tract.gdb')

dcmetro_svi = us_svi[us_svi['STCNTY'].isin(county_fips_list)]

dcmetro_svi.reset_index(drop=True)

dcmetro_svi['RPL_THEMES'] = dcmetro_svi['RPL_THEMES'].replace(-999.00, 0)

df_less = dcmetro_svi[['COUNTY','FIPS','LOCATION','RPL_THEMES','geometry']]

def fig2img(fig):
     #convert matplot fig to image and return it

     buf = io.BytesIO()
     fig.savefig(buf)
     buf.seek(0)
     img = Image.open(buf)
     return img


list_gif = []

def test_funf():
    
    filtration_threshold_=1
    selected_edges = []
    
    # Calculate total iterations for tqdm
    total_iterations = int((filtration_threshold_ - 0.9) / 0.001)
    
    with tqdm(total=total_iterations) as pbar:

        while filtration_threshold_ > 0.9:

            filtered_df = df_less[df_less['RPL_THEMES'] > filtration_threshold_]

            # Perform a spatial join to find adjacent precincts
            adjacent_precincts = gpd.sjoin(filtered_df, filtered_df, predicate='intersects', how='left')

            # Filter the results to include only the adjacent states
            adjacent_precincts = adjacent_precincts.query('FIPS_left != FIPS_right')

            # Group the resulting dataframe by the original precinct Name and create a list of adjacent precinct Name
            adjacent_precincts = adjacent_precincts.groupby('FIPS_left')['FIPS_right'].apply(list).reset_index()

            city_sets = {}

            # iterate through rows and define cities as sets
            for index, row in adjacent_precincts.iterrows():
                city_set_name = row['FIPS_left']
                # print(city_set_name)
                city_set = set(row['FIPS_right'])
                city_sets[city_set_name] = city_set
                # print(city_set)

            #city centroids
            city_coordinates = {city.FIPS: np.array((city.geometry.centroid.x, city.geometry.centroid.y)) for _, city in filtered_df.iterrows()}

            # get all city names from the city sets and insert into an array
            points = [city_set_name for city_set_name in city_sets.keys()]

            # filtered_coordinates = {key: value for key, value in city_coordinates.items() if key in points}

            pairs = []
            for i in range(len(points)):
                        for j in range(i+1, len(points)):
                            pair = [points[i], points[j]]
                            if len(pair) == 2 and pair not in pairs:
                                pairs.append(pair)

            for pair in pairs:
                if pair[0] in city_sets[pair[1]]:

                    if pair not in selected_edges:
                        selected_edges.append(pair)
                        # print("Filteration threshold:",filtration_threshold_)
                        # print("Number of edges:",len(selected_edges))

                        # Create a figure and axis
                        fig, ax = plt.subplots(figsize=(20, 20))
                        ax.set_axis_off() 

                        # Plot the "wyoming_svi" DataFrame
                        dcmetro_svi.plot(ax=ax, column="RPL_THEMES", cmap='OrRd', edgecolor='black', linewidth=0.3)

                        # Iterate over selected edges and plot them on the same axis
                        for selected_pair in selected_edges:
                            ax.plot([city_coordinates[selected_pair[0]][0], city_coordinates[selected_pair[1]][0]],
                                    [city_coordinates[selected_pair[0]][1], city_coordinates[selected_pair[1]][1]], color='green')


                        # Flatten the list of lists to a single list of edges
                        flat_edges = [edge for sublist in selected_edges for edge in sublist]
                        unique_edges = list(set(flat_edges))

                        filtered_coordinates = {key: value for key, value in city_coordinates.items() if key in unique_edges}
                        # print(filtered_coordinates)
                        # print("Unique centroids",unique_edges)

                        # Color the triangles formed by any three points
                        for combo in combinations(filtered_coordinates.keys(), 3):
                            if combo[1] in city_sets[combo[0]] and combo[2] in city_sets[combo[0]] and combo[2] in city_sets[combo[1]]:

                                # Creating pairs
                                pair1 = [combo[0], combo[1]]
                                pair2 = [combo[0], combo[2]]
                                pair3 = [combo[2], combo[1]]
                                pair33 = [combo[1], combo[2]]


                                # Storing pairs in an array
                                # pairs_array = [pair1, pair2, pair3]
                                # print(selected_edges)
                                # print(pair1)
                                # print(pair2)
                                # print(pair33)



                                # if (pair1 or pair11) in selected_edges and (pair2 or pair22) in selected_edges and (pair3 or pair33) in selected_edges:
                                if pair1 in selected_edges and pair2 in selected_edges and pair33 in selected_edges:


                                    # if combo[1] in unique_edges and combo[2] in unique_edges and combo[2] in unique_edges:
                                    # print("comb 1: ",combo[0])
                                    # print("comb 2: ",combo[1])
                                    # print("comb 3: ",combo[2])

                                    p1 = city_coordinates[combo[0]]
                                    p2 = city_coordinates[combo[1]]
                                    p3 = city_coordinates[combo[2]]
                                    plt.fill([p1[0], p2[0], p3[0]], [p1[1], p2[1], p3[1]], color='yellow', alpha=0.3)

                                    plt.scatter(p1[0], p1[1], color='green',s=5)
                                    plt.scatter(p2[0], p2[1], color='green',s=5)
                                    plt.scatter(p3[0], p3[1], color='green',s=5) 

                        # Show the plot
                        # plt.show()

                        img = fig2img(fig)
                        list_gif.append(img)
                        plt.close()


            filtration_threshold_ -= 0.001
            print(filtration_threshold_)
            pbar.update(1)  # Update the progress bar

            
test_funf()

## If you want to save the GIF

list_gif[0].save('image8.gif',
                 save_all=True,append_images=list_gif[1:],optimize=False,duration=50,loop=0)

