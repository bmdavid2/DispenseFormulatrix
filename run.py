#!/usr/bin/python3

import argparse
import csv
import os
import string

import pandas as pd

# Define the command line interface
parser = argparse.ArgumentParser(description="Create Optimization Design Mantis Worklists.")

parser.add_argument(
    "-n",
    "--name",
    default="Experiment",
    type=str,
    help="The name of the experiment.",
)

parser.add_argument(
    "-d",
    "--design_file",
    required=True,
    type=str,
    help="The file path of the experimental design file.",
)

parser.add_argument(
    "-o",
    "--output_dir",
    default="./",
    type=str,
    help="The output directory for the worklist files.",
)

parser.add_argument(
    "-v",
    "--verbose",
    action="store_true",
    help="Enable verbose output of reagent volumes.",
)

args = parser.parse_args()

def process_experiment_design(filepath):
    """
    Read in design file matching the format of `Design_Template.xlsx`. 
    """
    
    design = pd.read_excel(filepath, sheet_name="DESIGN", index_col=0)
    reagents = pd.read_excel(filepath, sheet_name="REAGENTS", index_col=0)
    info = pd.read_excel(filepath, sheet_name="INFO", index_col=None)

    groups = dict(zip(info["Level_Values"].values, info["Level_Names"].values))
    rows = list(string.ascii_uppercase[0: int(info["N_Plate_Rows"].values[0])])
    cols = range(1, int(info["N_Plate_Cols"].values[0])+1)
    empty_plate = pd.DataFrame(data=None, columns=cols, index=rows)

    return design, reagents, groups, empty_plate


def lay_out_plates(design, reagents, groups, plate):
    """
    Create the dispense volume layouts for all the reagents.
    """
    
    auto_reagents = reagents[reagents["Automated"]]
    manual_reagents = reagents[~reagents["Automated"]]
    const_reagents = auto_reagents[auto_reagents["Constant"]]
    reagents = auto_reagents[~auto_reagents["Constant"]]
        
    # Assign Mantis reagent names to optimization design
    reagent_stock_names = {r: [r] for r in const_reagents.index.values}
    for r in reagents.index.values:
        try:
            design[r] = f"{r}_stock_" + design[r].replace(groups)
        except KeyError:
            print(f"ERROR: Could not locate reagent \"{r}\" in design table.")
            quit()
        reagent_stock_names[r] = [f"{r}_stock_" + l for l in groups.values()]

    # Assign Run_ID to their respective wells
    for row_idx, row_data in design.iterrows():
        plate.loc[row_data["Row"], row_data["Col"]] = row_data.name
    
    if args.verbose:
        print(f"\n####### PLATE LAYOUT EXPERIMENT IDs #######")
        print(plate.fillna("---"))
        
    # Assemble plate layouts with dispense volumes for each reagent
    dispense_volumes = {}
    for reagent in const_reagents.index.values:  
        volume = const_reagents.loc[reagent, "Vol"]
        reagent_volumes = plate.copy()
        reagent_volumes[~reagent_volumes.isnull()] = float(volume)  # Set non-NaN cells to dispense vol
        reagent_volumes = reagent_volumes.fillna(0.0)  # Set NaN cells to 0
        dispense_volumes[reagent] = reagent_volumes

        if args.verbose:
            print(f"\n####### REAGENT VOLUMES: {reagent} #######")
            print(reagent_volumes.replace({0.0: "---"}))
        
        
    for reagent in reagents.index.values:
        if args.verbose:
            print(f"\n####### REAGENT VOLUMES: {reagent} #######")
        
        volume = reagents.loc[reagent, "Vol"]
        stock_names = reagent_stock_names[reagent]

        for stock in stock_names:
            reagent_volumes = plate.copy()

            # Get design column of current reagent e.g. RNA
            reagent_col = design[reagent]

            # Get all indexes matching current reagent e.g. RNA_stock_A
            indexes_to_keep = set(reagent_col[reagent_col == stock].index)

            # Create replacement dict, where Run_IDs matching current reagent (e.g. RNA_stock_A)
            # are set to the reagent's dispense volume, and all others are set to 0.
            experiment_volumes = {}
            for index in design.index:
                if index in indexes_to_keep:
                    experiment_volumes[index] = float(volume)
                else:
                    experiment_volumes[index] = 0.0

            # Set volumes of Run_IDs matching current reagent
            reagent_volumes = reagent_volumes.fillna(0.0).replace(experiment_volumes)
            dispense_volumes[stock] = reagent_volumes
            if args.verbose:
                print(f"## {stock} ##")
                print(reagent_volumes.replace({0.0: "---"}))
                
    return dispense_volumes, reagent_stock_names

def create_files(output_folder, expt_name, reagents, dispense_volumes, reagent_stock_names):
    """
    Create all the Mantis worklist files and save them in `output_folder`.
    """
    
    if not os.path.isdir(output_folder):
        os.makedirs(output_folder)
            
    print()
    dispense_groups = reagents.groupby(by="Dispense_Group")
    for group_idx, group_df in dispense_groups:
        group_reagents = group_df.index.values
        dispense_layouts = {}
        for r in group_reagents:
            stocks = reagent_stock_names.get(r, [])
            dispense_layouts.update({s: dispense_volumes[s] for s in stocks})
        
        output_path = os.path.join(output_folder, f"{expt_name}_Group-{group_idx}.dl.txt")
        export_mantis_worklist(output_path, dispense_layouts)
    
def export_mantis_worklist(path, reagent_volumes):
    """
    Converts dictionary of reagent volumes into a worklist format readable by
    the Formulatrix Mantis.
    
    Inputs
    ------
    path: str
        Path to save the worklist, including its file name with extension. To be
        readable by the Mantis, it extension has to be '.dl.txt'.
    reagent_volumes: dict(str->pd.DataFrame)
        The keys of the dict are the reagent name to be used in the Mantis dispense.
        The values are a dataframe mapping out the rows and columns matching the layout
        of the dispense, where the values are the dispense volume in microliters.
    Outputs
    -------
    A dispense list saved at `path`.
    """

    num_reagents = len(reagent_volumes)
    delay_header = [num_reagents]
    for i in range(num_reagents):
        delay_header.extend([0, ""])
        
    with open(path, "w", newline="", encoding="utf-8") as f:
        # Header
        writer = csv.writer(f, delimiter="\t")
        writer.writerow(["[ Version: 5 ]"])
        writer.writerow(["breakaway_pcr_96.pd.txt"])
        writer.writerow(delay_header)
        writer.writerow([1])
        writer.writerow(delay_header)

        # Pipette volumes
        for reagent_name, vols in reagent_volumes.items():
            writer.writerow([reagent_name, "", "Normal"])
            writer.writerow(["Well", 1])
            writer.writerows(vols.values.tolist())
            
        print(f"Created worklist: {path}")
            
if __name__ == "__main__":
    # Process design file
    design, reagents, groups, plate = process_experiment_design(args.design_file)
    
    # Lay out plates
    dispense_volumes, reagent_stock_names = lay_out_plates(design, reagents, groups, plate)
    
    # Export the Mantis worklists
    create_files(args.output_dir, args.name, reagents, dispense_volumes, reagent_stock_names)
    
    
    
    